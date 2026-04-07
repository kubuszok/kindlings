package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.KindlingsCoproductHint
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.{ConfigCursor, ConfigReader}
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsEnumRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsEnumRule extends ReaderDerivationRule("handle as enum when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: ReaderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[ConfigReaderFailures, A]]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      implicit val FieldHintT: Type[KindlingsCoproductHint.Field[A]] = RTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[KindlingsCoproductHint.Wrapped[A]] = RTypes.wrappedCoproductHintType[A]
      implicit val FirstSuccessHintT: Type[KindlingsCoproductHint.FirstSuccess[A]] =
        RTypes.firstSuccessCoproductHintType[A]

      // Per-type override for sealed-trait dispatch. We summon each concrete variant
      // separately so the macro can detect which one is in scope. Users must declare
      // their hint with the concrete variant on the LHS:
      //   implicit val hint: KindlingsCoproductHint.Field[Foo] = KindlingsCoproductHint.Field(...)
      // (declaring as the abstract parent type would prevent the specific summon from
      // matching).
      val maybeFieldHint: Option[Expr[KindlingsCoproductHint.Field[A]]] =
        Expr.summonImplicit[KindlingsCoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[KindlingsCoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[KindlingsCoproductHint.Wrapped[A]].toOption
      val maybeFirstSuccessHint: Option[Expr[KindlingsCoproductHint.FirstSuccess[A]]] =
        Expr.summonImplicit[KindlingsCoproductHint.FirstSuccess[A]].toOption

      // Resolve the per-type encoder choice and the constructor-name transform:
      //  - FirstSuccess hint  → tryEachSubtype mode
      //  - Field hint         → discriminator mode with hint's fieldName / transform
      //  - Wrapped hint       → wrapping mode with hint's transform
      //  - no hint            → use global config (`PureConfig.discriminator` /
      //                          `PureConfig.transformConstructorNames`)
      val transformConstructorNamesExpr: Expr[String => String] = maybeFirstSuccessHint match {
        case Some(_) =>
          // FirstSuccess does not use a name transform; we still need a value here for
          // the dispatch tree's fallback path, but it's never invoked in this mode.
          Expr.quote(identity[String])
        case None =>
          maybeFieldHint match {
            case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
            case None    =>
              maybeWrappedHint match {
                case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
                case None    => Expr.quote(Expr.splice(rctx.config).transformConstructorNames)
              }
          }
      }

      val childrenList = enumm.directChildren.toList
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              PureConfigDerivationUtils.failedToMatchSubtype(
                Expr.splice(Expr(Type[A].prettyPrint)),
                Expr.splice(rctx.cursor),
                List.empty
              )
            ): Either[ConfigReaderFailures, A]
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving reader for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildReader[A, ChildType](childName, transformConstructorNamesExpr)
              }
            }
            .map { childDispatchers =>
              // Build a chain of `if (typeName == childName) <decode> else <else>` expressions
              // by folding over the dispatchers from right to left.
              val errorExpr: Expr[String] => Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]] =
                typeNameExpr =>
                  innerCursorExpr =>
                    Expr.quote {
                      Left(
                        PureConfigDerivationUtils.failedToMatchSubtype(
                          Expr.splice(typeNameExpr),
                          Expr.splice(innerCursorExpr),
                          Expr.splice(Expr(knownNames))
                        )
                      ): Either[ConfigReaderFailures, A]
                    }

              val dispatchTree: (Expr[String], Expr[ConfigCursor]) => Expr[Either[ConfigReaderFailures, A]] =
                (typeNameExpr, innerCursorExpr) =>
                  childDispatchers.toList.foldRight(errorExpr(typeNameExpr)(innerCursorExpr)) {
                    case (dispatcher, elseExpr) =>
                      dispatcher(typeNameExpr, innerCursorExpr, elseExpr)
                  }

              // FirstSuccessCoproductHint: not yet implemented at the macro level. The
              // existing dispatcher closure threads (typeName, innerCursor, else) and
              // doesn't expose the bare "run this child reader" call. The macro currently
              // ignores the hint when it's FirstSuccess and uses the global config's
              // discriminator instead. Refactoring the child dispatcher to support raw
              // invocation is a follow-up.
              val _ = maybeFirstSuccessHint // suppress unused warning until wired

              // Compute the resolved discriminator field at compile time, threading the
              // per-type CoproductHint variants through the global config fall-back.
              // Field hint → use its fieldName. Wrapped hint → no discriminator.
              // No hint → defer to global config at runtime.
              val discrFieldExpr: Expr[Option[String]] = maybeFieldHint match {
                case Some(h) => Expr.quote((Some(Expr.splice(h).fieldName): Option[String]))
                case None    =>
                  maybeWrappedHint match {
                    case Some(_) => Expr.quote(None: Option[String])
                    case None    => Expr.quote(Expr.splice(rctx.config).discriminator)
                  }
              }

              if (allCaseObjects) {
                Expr.quote {
                  val cur = Expr.splice(rctx.cursor)
                  // Singleton case objects: prefer to read as a string scalar
                  // ("VariantName"), falling back to wrapping/discriminator object encoding.
                  cur.asString match {
                    case Right(str) =>
                      val _ = str
                      Expr.splice(dispatchTree(Expr.quote(str), Expr.quote(cur)))
                    case Left(_) =>
                      Expr.splice(discrFieldExpr) match {
                        case Some(field) =>
                          cur.asObjectCursor.flatMap { obj =>
                            PureConfigDerivationUtils
                              .readDiscriminator(obj, field)
                              .flatMap { typeName =>
                                Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(cur)))
                              }
                          }
                        case None =>
                          cur.asObjectCursor.flatMap { obj =>
                            PureConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, innerCur) =>
                              Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(innerCur)))
                            }
                          }
                      }
                  }
                }
              } else {
                Expr.quote {
                  val cur = Expr.splice(rctx.cursor)
                  cur.asObjectCursor.flatMap { obj =>
                    Expr.splice(discrFieldExpr) match {
                      case Some(field) =>
                        PureConfigDerivationUtils.readDiscriminator(obj, field).flatMap { typeName =>
                          // For discriminator-style encoding, the inner cursor is the same
                          // object cursor (so the case-class rule can find its fields alongside
                          // the discriminator key, which it will ignore).
                          Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(cur)))
                        }
                      case None =>
                        PureConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, innerCur) =>
                          Expr.splice(dispatchTree(Expr.quote(typeName), Expr.quote(innerCur)))
                        }
                    }
                  }
                }
              }
            }
      }
    }

    /** Derive a single sealed-trait child case dispatcher: a function that takes the
      * runtime `(typeName, innerCursor)` and an `else` branch, and emits an `if (typeName
      * == "ChildName") childReader.from(innerCursor) else else`.
      *
      * The child case can be:
      *  - A user-provided implicit `ConfigReader[ChildType]`
      *  - A singleton case object → return the singleton value directly
      *  - A recursive derivation via the cached helper
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildReader[A: ReaderCtx, ChildType: Type](
        childName: String,
        transformConstructorNamesExpr: Expr[String => String]
    ): MIO[
      (Expr[String], Expr[ConfigCursor], Expr[Either[ConfigReaderFailures, A]]) => Expr[
        Either[ConfigReaderFailures, A]
      ]
    ] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val StringT: Type[String] = RTypes.String
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val ReaderResultA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      implicit val ReaderResultChild: Type[Either[ConfigReaderFailures, ChildType]] = RTypes.ReaderResult[ChildType]
      implicit val ReaderChild: Type[ConfigReader[ChildType]] = RTypes.ConfigReader[ChildType]

      RTypes
        .ConfigReader[ChildType]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[$childName], using it") >>
            MIO.pure {
              (
                  typeNameExpr: Expr[String],
                  innerCurExpr: Expr[ConfigCursor],
                  elseExpr: Expr[Either[ConfigReaderFailures, A]]
              ) =>
                Expr.quote {
                  if (
                    Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                      Expr.splice(typeNameExpr)
                  )
                    Expr
                      .splice(readerExpr)
                      .from(Expr.splice(innerCurExpr))
                      .asInstanceOf[Either[ConfigReaderFailures, A]]
                  else
                    Expr.splice(elseExpr)
                }
            }

        case Left(_) =>
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure {
                  (
                      typeNameExpr: Expr[String],
                      _: Expr[ConfigCursor],
                      elseExpr: Expr[Either[ConfigReaderFailures, A]]
                  ) =>
                    Expr.quote {
                      if (
                        Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                          Expr.splice(typeNameExpr)
                      )
                        Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConfigReaderFailures, A]
                      else
                        Expr.splice(elseExpr)
                    }
                }
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.cursor)).flatMap { _ =>
                rctx.getHelper[ChildType].map {
                  case Some(helper) =>
                    (
                        typeNameExpr: Expr[String],
                        innerCurExpr: Expr[ConfigCursor],
                        elseExpr: Expr[Either[ConfigReaderFailures, A]]
                    ) => {
                      val helperCallExpr = helper(innerCurExpr, rctx.config)
                      Expr.quote {
                        if (
                          Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))) ==
                            Expr.splice(typeNameExpr)
                        )
                          Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigReaderFailures, A]]
                        else
                          Expr.splice(elseExpr)
                      }
                    }

                  case None =>
                    (
                        _: Expr[String],
                        _: Expr[ConfigCursor],
                        elseExpr: Expr[Either[ConfigReaderFailures, A]]
                    ) => elseExpr
                }
              }
          }
      }
    }
  }
}
