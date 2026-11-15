package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, CoproductHint}
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.{ConfigObject, ConfigValue}

trait ReaderHandleAsEnumRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsEnumRule extends ReaderDerivationRule("handle as enum when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
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
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val FieldHintT: Type[CoproductHint.Field[A]] = RTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[CoproductHint.Wrapped[A]] = RTypes.wrappedCoproductHintType[A]
      implicit val FirstSuccessHintT: Type[CoproductHint.FirstSuccess[A]] = RTypes.firstSuccessCoproductHintType[A]

      // Per-type override for sealed-trait dispatch: each concrete variant is summoned separately, so users must
      // declare their hint with the concrete variant as the type.
      val maybeFieldHint: Option[Expr[CoproductHint.Field[A]]] =
        Expr.summonImplicit[CoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[CoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[CoproductHint.Wrapped[A]].toOption
      val maybeFirstSuccessHint: Option[Expr[CoproductHint.FirstSuccess[A]]] =
        Expr.summonImplicit[CoproductHint.FirstSuccess[A]].toOption

      val transformConstructorNamesExpr: Expr[String => String] = maybeFirstSuccessHint match {
        case Some(_) => Expr.quote((name: String) => name) // names are not read with FirstSuccess
        case None    =>
          maybeFieldHint match {
            case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
            case None    =>
              maybeWrappedHint match {
                case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
                case None    => Expr.quote(Expr.splice(rctx.config).transformConstructorNames)
              }
          }
      }

      val discrFieldExpr: Expr[Option[String]] = maybeFieldHint match {
        case Some(h) => Expr.quote((Some(Expr.splice(h).fieldName): Option[String]))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => Expr.quote(None: Option[String])
            case None    => Expr.quote(Expr.splice(rctx.config).discriminator)
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
              SConfigDerivationUtils.failedToMatchSubtype(
                Expr.splice(Expr(Type[A].prettyPrint)),
                List.empty
              )
            ): Either[ConfigDecodingError, A]
          })

        case Some(children) =>
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving reader for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildReader[A, ChildType](childName, transformConstructorNamesExpr)
              }
            }
            .map { childReaders =>
              maybeFirstSuccessHint match {
                case Some(_) => firstSuccess[A](childReaders.toList)
                case None    => byName[A](childReaders.toList, allCaseObjects, discrFieldExpr)
              }
            }
      }
    }

    /** How to read one subtype: `read` reads it from the subtype's value, `readFromObject` too, but (like upstream's
      * derived reader of a case object) requires that value to be an object.
      */
    final private case class ChildReader[A](
        childName: String,
        transformedName: Expr[String],
        read: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]],
        readFromObject: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]]
    )

    /** Subtypes selected by name (as upstream PureConfig):
      *   - with a discriminator field (`FieldCoproductHint`): the value must be an object, its discriminator names the
      *     subtype, which is read from the object without the discriminator key; an unknown name fails with
      *     `UnexpectedValueForFieldCoproductHint` at the discriminator;
      *   - without one, a family of case objects is read from strings (`deriveEnumerationReader`);
      *   - without one, other families are read from single-key objects `{ subtype-name { ... } }`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def byName[A: ReaderCtx](
        childReaders: List[ChildReader[A]],
        allCaseObjects: Boolean,
        discrFieldExpr: Expr[Option[String]]
    ): Expr[Either[ConfigDecodingError, A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ConfigObjectT: Type[ConfigObject] = RTypes.ConfigObject
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      val knownNames: List[String] = childReaders.map(_.childName)

      // `if (typeName == "child-name") <read child> else ... else <unknown>`
      def dispatch(
          typeNameExpr: Expr[String],
          innerValueExpr: Expr[ConfigValue],
          unknownExpr: Expr[Either[ConfigDecodingError, A]]
      ): Expr[Either[ConfigDecodingError, A]] =
        childReaders.foldRight(unknownExpr) { (child, elseExpr) =>
          Expr.quote {
            if (Expr.splice(child.transformedName) == Expr.splice(typeNameExpr))
              Expr.splice(child.read(innerValueExpr))
            else Expr.splice(elseExpr)
          }
        }

      val typeNameStr: String = Type[A].shortName

      val withoutDiscriminator: Expr[Either[ConfigDecodingError, A]] =
        if (allCaseObjects)
          Expr.quote {
            val v = Expr.splice(rctx.value)
            SConfigDerivationUtils.asString(v).flatMap { str =>
              Expr.splice(
                dispatch(
                  Expr.quote(str),
                  Expr.quote(v),
                  Expr.quote {
                    Left(
                      SConfigDerivationUtils.invalidEnumValue(str, Expr.splice(Expr(typeNameStr)))
                    ): Either[ConfigDecodingError, A]
                  }
                )
              )
            }
          }
        else
          Expr.quote {
            SConfigDerivationUtils.asObject(Expr.splice(rctx.value)).flatMap { obj =>
              SConfigDerivationUtils.readWrapped(obj).flatMap { case (typeName, inner) =>
                // the subtype's failures are reported under its key
                Expr
                  .splice(
                    dispatch(
                      Expr.quote(typeName),
                      Expr.quote(inner),
                      Expr.quote {
                        Left(
                          SConfigDerivationUtils.failedToMatchSubtype(typeName, Expr.splice(Expr(knownNames)))
                        ): Either[ConfigDecodingError, A]
                      }
                    )
                  )
                  .left
                  .map(_.withParentPath(typeName))
              }
            }
          }

      Expr.quote {
        Expr.splice(discrFieldExpr) match {
          case Some(field) =>
            SConfigDerivationUtils.asObject(Expr.splice(rctx.value)).flatMap { obj =>
              SConfigDerivationUtils.readDiscriminatorField(obj, field).flatMap { typeName =>
                Expr.splice(
                  dispatch(
                    Expr.quote(typeName),
                    Expr.quote(obj.withoutKey(field): ConfigValue),
                    Expr.quote {
                      Left(SConfigDerivationUtils.unexpectedDiscriminatorValue(obj, field)): Either[
                        ConfigDecodingError,
                        A
                      ]
                    }
                  )
                )
              }
            }
          case None =>
            Expr.splice(withoutDiscriminator)
        }
      }
    }

    /** Upstream `FirstSuccessCoproductHint`: every subtype is attempted on the same value, in the alphabetical order of
      * the subtypes' names (as upstream's derivation passes them to the hint), and the first one read successfully
      * wins. When none can be read, the failure is `NoValidCoproductOptionFound` with every attempt's failure.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def firstSuccess[A: ReaderCtx](
        childReaders: List[ChildReader[A]]
    ): Expr[Either[ConfigDecodingError, A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val AttemptsT: Type[SConfigDerivationUtils.FirstSuccessAttempts] = RTypes.FirstSuccessAttempts

      val ordered = childReaders.sortBy(_.childName)
      val optionNames: List[String] = ordered.map(_.childName)

      // A runtime `FirstSuccessAttempts` records each failed attempt and the first success (no `val` per attempt).
      def chain(attempts: Expr[SConfigDerivationUtils.FirstSuccessAttempts]): Expr[Either[ConfigDecodingError, A]] =
        ordered.foldRight(Expr.quote(Expr.splice(attempts).failed[A])) { (child, elseExpr) =>
          Expr.quote {
            if (
              Expr
                .splice(attempts)
                .succeeded(Expr.splice(child.readFromObject(Expr.quote(Expr.splice(attempts).value))))
            )
              Expr.splice(attempts).result[A]
            else Expr.splice(elseExpr)
          }
        }

      Expr.quote {
        val attempts = new SConfigDerivationUtils.FirstSuccessAttempts(
          Expr.splice(rctx.value),
          Expr.splice(Expr(optionNames))
        )
        Expr.splice(chain(Expr.quote(attempts)))
      }
    }

    /** Derive how to read a single sealed-trait subtype (see [[ChildReader]]): a user-provided implicit
      * `ConfigReader[ChildType]`, a singleton case object, or a recursive derivation via the cached helper.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildReader[A: ReaderCtx, ChildType: Type](
        childName: String,
        transformConstructorNamesExpr: Expr[String => String]
    ): MIO[ChildReader[A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val ReaderResultA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val ReaderResultChild: Type[Either[ConfigDecodingError, ChildType]] = RTypes.ReaderResult[ChildType]
      implicit val ReaderChild: Type[ConfigReader[ChildType]] = RTypes.ConfigReader[ChildType]

      val transformedNameExpr: Expr[String] =
        Expr.quote(Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(childName))))

      def same(read: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]]): ChildReader[A] =
        ChildReader[A](childName, transformedNameExpr, read, read)

      RTypes
        .ConfigReader[ChildType]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[$childName], using it") >>
            MIO.pure(same { (innerValueExpr: Expr[ConfigValue]) =>
              Expr.quote {
                Expr
                  .splice(readerExpr)
                  .from(Expr.splice(innerValueExpr))
                  .asInstanceOf[Either[ConfigDecodingError, A]]
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
                    (_: Expr[ConfigValue]) =>
                      Expr.quote(Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConfigDecodingError, A]),
                    (innerValueExpr: Expr[ConfigValue]) =>
                      Expr.quote {
                        SConfigDerivationUtils.readObjectAs[A](
                          Expr.splice(innerValueExpr),
                          Expr.splice(singleton).asInstanceOf[A]
                        )
                      }
                  )
                )
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.value)).flatMap { _ =>
                rctx.getHelper[ChildType].flatMap {
                  case Some(helper) =>
                    MIO.pure(same { (innerValueExpr: Expr[ConfigValue]) =>
                      val helperCallExpr = helper(innerValueExpr, rctx.config)
                      Expr.quote(Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigDecodingError, A]])
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
