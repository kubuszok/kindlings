package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.KindlingsCoproductHint
import hearth.kindlings.pureconfigderivation.internal.runtime.{EnumerationFailures, PureConfigDerivationUtils}
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
      // When no per-type hint is present and the global config was evaluable at compile time,
      // we can pre-compute transformConstructorNames(childName) as constant strings.
      val preComputedConstructorTransform: Option[String => String] =
        (maybeFirstSuccessHint, maybeFieldHint, maybeWrappedHint) match {
          case (None, None, None) => rctx.evaluatedConfig.map(_.transformConstructorNames)
          case _                  => None
        }

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
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving reader for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildReader[A, ChildType](
                  childName,
                  transformConstructorNamesExpr,
                  preComputedConstructorTransform
                )
              }
            }
            .map { childReaders =>
              maybeFirstSuccessHint match {
                case Some(_) => firstSuccess[A](childReaders.toList)
                case None    =>
                  byName[A](childReaders.toList, allCaseObjects, discriminatorExpr[A](maybeFieldHint, maybeWrappedHint))
              }
            }
      }
    }

    /** How to read one subtype: `read` reads it from the cursor of the subtype's value, `readFromObject` too, but (like
      * upstream's derived reader of a case object) requires that cursor to be an object.
      */
    final private case class ChildReader[A](
        childName: String,
        transformedName: Expr[String],
        read: Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]],
        readFromObject: Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]]
    )

    /** The discriminator field: the per-type hint's (`Field` = its field name, `Wrapped` = none), otherwise the global
      * config's (a constant when the config could be evaluated at compile time).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def discriminatorExpr[A: ReaderCtx](
        maybeFieldHint: Option[Expr[KindlingsCoproductHint.Field[A]]],
        maybeWrappedHint: Option[Expr[KindlingsCoproductHint.Wrapped[A]]]
    ): Expr[Option[String]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val FieldHintT: Type[KindlingsCoproductHint.Field[A]] = RTypes.fieldCoproductHintType[A]
      maybeFieldHint match {
        case Some(h) => Expr.quote((Some(Expr.splice(h).fieldName): Option[String]))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => Expr.quote(None: Option[String])
            case None    =>
              rctx.evaluatedConfig match {
                case Some(evConfig) =>
                  evConfig.discriminator match {
                    case Some(d) => Expr.quote((Some(Expr.splice(Expr(d))): Option[String]))
                    case None    => Expr.quote(None: Option[String])
                  }
                case None => Expr.quote(Expr.splice(rctx.config).discriminator)
              }
          }
      }
    }

    /** Subtypes selected by name:
      *   - with a discriminator field (upstream `FieldCoproductHint`): the value must be an object, its discriminator
      *     names the subtype, which is read from the object without the discriminator key; an unknown name fails with
      *     upstream's `UnexpectedValueForFieldCoproductHint` at the discriminator;
      *   - without one, a family of case objects is read from strings (upstream `deriveEnumerationReader`);
      *   - without one, other families are read from single-key objects `{ subtype-name { ... } }`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def byName[A: ReaderCtx](
        childReaders: List[ChildReader[A]],
        allCaseObjects: Boolean,
        discrFieldExpr: Expr[Option[String]]
    ): Expr[Either[ConfigReaderFailures, A]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]

      val knownNames: List[String] = childReaders.map(_.childName)

      // `if (typeName == "child-name") <read child> else ... else <unknown>`
      def dispatch(
          typeNameExpr: Expr[String],
          innerCursorExpr: Expr[ConfigCursor],
          unknownExpr: Expr[Either[ConfigReaderFailures, A]]
      ): Expr[Either[ConfigReaderFailures, A]] =
        childReaders.foldRight(unknownExpr) { (child, elseExpr) =>
          Expr.quote {
            if (Expr.splice(child.transformedName) == Expr.splice(typeNameExpr))
              Expr.splice(child.read(innerCursorExpr))
            else Expr.splice(elseExpr)
          }
        }

      val typeNameStr: String = Type[A].shortName

      val withoutDiscriminator: Expr[Either[ConfigReaderFailures, A]] =
        if (allCaseObjects)
          Expr.quote {
            val cur = Expr.splice(rctx.cursor)
            cur.asString.flatMap { str =>
              Expr.splice(
                dispatch(
                  Expr.quote(str),
                  Expr.quote(cur),
                  Expr.quote {
                    Left(
                      EnumerationFailures.invalidEnumValue(cur, str, Expr.splice(Expr(typeNameStr)))
                    ): Either[ConfigReaderFailures, A]
                  }
                )
              )
            }
          }
        else
          Expr.quote {
            val cur = Expr.splice(rctx.cursor)
            cur.asObjectCursor.flatMap { obj =>
              PureConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, innerCur) =>
                Expr.splice(
                  dispatch(
                    Expr.quote(typeName),
                    Expr.quote(innerCur),
                    Expr.quote {
                      Left(
                        PureConfigDerivationUtils.failedToMatchSubtype(
                          typeName,
                          innerCur,
                          Expr.splice(Expr(knownNames))
                        )
                      ): Either[ConfigReaderFailures, A]
                    }
                  )
                )
              }
            }
          }

      Expr.quote {
        Expr.splice(discrFieldExpr) match {
          case Some(field) =>
            Expr.splice(rctx.cursor).asObjectCursor.flatMap { obj =>
              obj.atKey(field).flatMap { discrCur =>
                discrCur.asString.flatMap { typeName =>
                  Expr.splice(
                    dispatch(
                      Expr.quote(typeName),
                      Expr.quote(obj.withoutKey(field): ConfigCursor),
                      Expr.quote {
                        Left(PureConfigDerivationUtils.unexpectedDiscriminatorValue(discrCur)): Either[
                          ConfigReaderFailures,
                          A
                        ]
                      }
                    )
                  )
                }
              }
            }
          case None =>
            Expr.splice(withoutDiscriminator)
        }
      }
    }

    /** Upstream `FirstSuccessCoproductHint`: every subtype is attempted on the same cursor, in the alphabetical order
      * of the subtypes' names (as upstream's derivation passes them to the hint), and the first one read successfully
      * wins. When none can be read, the failure is upstream's `NoValidCoproductOptionFound` with every attempt's
      * failures.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def firstSuccess[A: ReaderCtx](
        childReaders: List[ChildReader[A]]
    ): Expr[Either[ConfigReaderFailures, A]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      implicit val AttemptsT: Type[PureConfigDerivationUtils.FirstSuccessAttempts] = RTypes.FirstSuccessAttempts

      val ordered = childReaders.sortBy(_.childName)
      val optionNames: List[String] = ordered.map(_.childName)

      // No `val` per attempt (nested `ValDefs` here trip Scala 3's owner checks): a runtime `FirstSuccessAttempts`
      // records each failed attempt and the first success.
      def chain(attempts: Expr[PureConfigDerivationUtils.FirstSuccessAttempts]): Expr[Either[ConfigReaderFailures, A]] =
        ordered.foldRight(Expr.quote(Expr.splice(attempts).failed[A])) { (child, elseExpr) =>
          Expr.quote {
            if (
              Expr
                .splice(attempts)
                .succeeded(Expr.splice(child.readFromObject(Expr.quote(Expr.splice(attempts).cursor))))
            )
              Expr.splice(attempts).result[A]
            else Expr.splice(elseExpr)
          }
        }

      Expr.quote {
        val attempts = new PureConfigDerivationUtils.FirstSuccessAttempts(
          Expr.splice(rctx.cursor),
          Expr.splice(Expr(optionNames))
        )
        Expr.splice(chain(Expr.quote(attempts)))
      }
    }

    /** Derive how to read a single sealed-trait subtype (see [[ChildReader]]).
      *
      * The child case can be:
      *   - A user-provided implicit `ConfigReader[ChildType]`
      *   - A singleton case object → return the singleton value directly
      *   - A recursive derivation via the cached helper
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildReader[A: ReaderCtx, ChildType: Type](
        childName: String,
        transformConstructorNamesExpr: Expr[String => String],
        preComputedConstructorTransform: Option[String => String]
    ): MIO[ChildReader[A]] = {
      implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
      implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
      implicit val StringT: Type[String] = RTypes.String
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val ReaderResultA: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
      implicit val ReaderResultChild: Type[Either[ConfigReaderFailures, ChildType]] = RTypes.ReaderResult[ChildType]
      implicit val ReaderChild: Type[ConfigReader[ChildType]] = RTypes.ConfigReader[ChildType]

      // Pre-compute the transformed constructor name when possible.
      val transformedNameExpr: Expr[String] = preComputedConstructorTransform match {
        case Some(transform) => Expr(transform(childName))
        case None            => Expr.quote(Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))))
      }

      def same(read: Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]]): ChildReader[A] =
        ChildReader[A](childName, transformedNameExpr, read, read)

      RTypes
        .ConfigReader[ChildType]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[$childName], using it") >>
            MIO.pure(same { (innerCurExpr: Expr[ConfigCursor]) =>
              Expr.quote {
                Expr
                  .splice(readerExpr)
                  .from(Expr.splice(innerCurExpr))
                  .asInstanceOf[Either[ConfigReaderFailures, A]]
              }
            })

        case Left(_) =>
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure(
                  ChildReader[A](
                    childName,
                    transformedNameExpr,
                    (_: Expr[ConfigCursor]) =>
                      Expr.quote(Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConfigReaderFailures, A]),
                    (innerCurExpr: Expr[ConfigCursor]) =>
                      Expr.quote {
                        PureConfigDerivationUtils.readObjectAs[A](
                          Expr.splice(innerCurExpr),
                          Expr.splice(singleton).asInstanceOf[A]
                        )
                      }
                  )
                )
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.cursor)).flatMap { _ =>
                rctx.getHelper[ChildType].flatMap {
                  case Some(helper) =>
                    MIO.pure(same { (innerCurExpr: Expr[ConfigCursor]) =>
                      val helperCallExpr = helper(innerCurExpr, rctx.config)
                      Expr.quote(Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigReaderFailures, A]])
                    })
                  case None =>
                    val err = ReaderDerivationError.CannotConstructType(
                      Type[ChildType].prettyPrint,
                      isSingleton = false,
                      Some("no cached reader for the subtype")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
              }
          }
      }
    }
  }
}
