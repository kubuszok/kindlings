package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.{KindlingsCoproductHint, KindlingsProductHint}
import hearth.kindlings.pureconfigderivation.annotations.configKey
import hearth.kindlings.pureconfigderivation.internal.runtime.{EnumerationFailures, PureConfigDerivationUtils}
import pureconfig.{ConfigCursor, ConfigReader}
import pureconfig.error.ConfigReaderFailures
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait ReaderHandleAsEnumRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsEnumRule extends ReaderDerivationRule("handle as enum when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        parseEnum[A].toEither match {
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

      val childrenList = StableChildrenOrder.directChildren(ReaderHandleAsEnumRuleImpl.this)(enumm)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      // The discriminator field, when it is a compile-time constant (no per-type hint, evaluable global config).
      val staticDiscriminator: Option[String] =
        (maybeFirstSuccessHint, maybeFieldHint, maybeWrappedHint) match {
          case (None, None, None) => rctx.evaluatedConfig.flatMap(_.discriminator)
          case _                  => None
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
                  preComputedConstructorTransform,
                  staticDiscriminator
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
      *
      * `needsDiscriminatorRemoved`: with a discriminator field, upstream reads the subtype from the object without the
      * discriminator key (`withoutKey`, which copies the object). It is `false` only when reading from the object with
      * the key is provably the same: the subtype ignores the cursor (a case object), or its reader is derived here as a
      * case class that statically allows unknown keys and has no field whose key is the discriminator.
      */
    final private case class ChildReader[A](
        childName: String,
        transformedName: Expr[String],
        read: Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]],
        readFromObject: Expr[ConfigCursor] => Expr[Either[ConfigReaderFailures, A]],
        needsDiscriminatorRemoved: Boolean = true
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
          // the inner cursor, given whether the subtype needs the discriminator removed (a Boolean rather than the
          // ChildReader: lambdas passed into splices must not mention macro-internal types on Scala 2)
          innerCursorFor: Boolean => Expr[ConfigCursor],
          unknownExpr: Expr[Either[ConfigReaderFailures, A]]
      ): Expr[Either[ConfigReaderFailures, A]] =
        childReaders.foldRight(unknownExpr) { (child, elseExpr) =>
          val innerCursor = innerCursorFor(child.needsDiscriminatorRemoved)
          Expr.quote {
            if (Expr.splice(child.transformedName) == Expr.splice(typeNameExpr))
              Expr.splice(child.read(innerCursor))
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
                  _ => Expr.quote(cur),
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
                    _ => Expr.quote(innerCur),
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
                      // `withoutKey` copies the object, so it is done only for subtypes whose reading can depend on it
                      (needsRemoval: Boolean) =>
                        if (needsRemoval) Expr.quote(obj.withoutKey(field): ConfigCursor)
                        else Expr.quote(obj: ConfigCursor),
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
      implicit val EitherAnyT: Type[Either[ConfigReaderFailures, Any]] = RTypes.EitherFailuresAny
      implicit val ListEitherT: Type[List[Either[ConfigReaderFailures, Any]]] = RTypes.ListEitherFailuresAny

      val ordered = childReaders.sortBy(_.childName)
      val optionNames: List[String] = ordered.map(_.childName)

      // Each attempt is bound to a `val`; the next subtype is attempted only when it failed, and when every attempt
      // failed, their failures are reported together.
      def attempt(
          cursor: Expr[ConfigCursor],
          remaining: List[ChildReader[A]],
          failedReversed: List[Expr[Either[ConfigReaderFailures, A]]]
      ): Expr[Either[ConfigReaderFailures, A]] = remaining match {
        case child :: rest =>
          ValDefs.createVal[Either[ConfigReaderFailures, A]](child.readFromObject(cursor)).use { result =>
            Expr.quote {
              if (Expr.splice(result).isRight) Expr.splice(result)
              else Expr.splice(attempt(cursor, rest, result :: failedReversed))
            }
          }
        case Nil =>
          val results = failedReversed.foldLeft(Expr.quote(List.empty[Either[ConfigReaderFailures, Any]])) { (acc, r) =>
            Expr.quote((Expr.splice(r): Either[ConfigReaderFailures, Any]) :: Expr.splice(acc))
          }
          Expr.quote {
            Left(
              PureConfigDerivationUtils.noValidCoproductOptionFromResults(
                Expr.splice(cursor),
                Expr.splice(Expr(optionNames)),
                Expr.splice(results)
              )
            )
          }
      }

      ValDefs.createVal[ConfigCursor](rctx.cursor).use(cursor => attempt(cursor, ordered, Nil))
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
        preComputedConstructorTransform: Option[String => String],
        staticDiscriminator: Option[String]
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
              implicit val BooleanT: Type[Boolean] = RTypes.Boolean
              val (staticAllowUnknownKeys, allowUnknownKeysExpr) =
                ReaderHandleAsCaseClassRule.allowUnknownKeysOf[ChildType]
              Log.info(s"Using singleton for $childName") >>
                MIO.pure(
                  // A case object that does not allow unknown keys (upstream's `ProductHint[Obj.type](allowUnknownKeys =
                  // false)`) reports every key of the object it is read from (without the discriminator).
                  if (!staticAllowUnknownKeys.contains(true))
                    ChildReader[A](
                      childName,
                      transformedNameExpr,
                      (innerCurExpr: Expr[ConfigCursor]) =>
                        Expr.quote {
                          PureConfigDerivationUtils.checkNoKeys[A](
                            Expr.splice(innerCurExpr),
                            Expr.splice(singleton).asInstanceOf[A],
                            Expr.splice(allowUnknownKeysExpr)
                          )
                        },
                      (innerCurExpr: Expr[ConfigCursor]) =>
                        Expr.quote {
                          PureConfigDerivationUtils.readEmptyProduct[A](
                            Expr.splice(innerCurExpr),
                            Expr.splice(singleton).asInstanceOf[A],
                            Expr.splice(allowUnknownKeysExpr)
                          )
                        }
                    )
                  else
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
                        },
                      needsDiscriminatorRemoved = false // `read` ignores the cursor
                    )
                )
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.cursor)).flatMap { _ =>
                rctx.getHelper[ChildType].flatMap {
                  case Some(helper) =>
                    MIO.pure(
                      same { (innerCurExpr: Expr[ConfigCursor]) =>
                        val helperCallExpr = helper(innerCurExpr, rctx.config)
                        Expr.quote(Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigReaderFailures, A]])
                      }.copy(needsDiscriminatorRemoved = !discriminatorKeyIsIgnored[ChildType](staticDiscriminator))
                    )
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

    /** Whether the reader derived for the case class `ChildType` reads the same from an object with or without the
      * discriminator key: it statically allows unknown keys (global config evaluable at compile time, no per-type
      * `KindlingsProductHint`) and no field's key is the (statically known) discriminator. Anything else - a non-case
      * class, a hint, a runtime config or discriminator - conservatively answers `false`.
      */
    private def discriminatorKeyIsIgnored[ChildType: Type](staticDiscriminator: Option[String])(implicit
        ctx: ReaderCtx[?]
    ): Boolean = {
      implicit val configKeyT: Type[configKey] = RTypes.ConfigKey
      implicit val ProductHintT: Type[KindlingsProductHint[ChildType]] = RTypes.kindlingsProductHintType[ChildType]
      (for {
        discriminator <- staticDiscriminator
        config <- ctx.evaluatedConfig
        if config.allowUnknownKeys
        if Expr.summonImplicit[KindlingsProductHint[ChildType]].toOption.isEmpty
        caseClass <- CaseClass.parse[ChildType].toEither.toOption
      } yield caseClass.primaryConstructor.totalParameters.flatten.forall { case (name, param) =>
        getAnnotationStringArg[configKey](param).getOrElse(config.transformMemberNames(name)) != discriminator
      }).getOrElse(false)
    }
  }
}
