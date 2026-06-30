package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.CoproductHint
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait WriterHandleAsEnumRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsEnumRule extends WriterDerivationRule("handle as enum when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def encodeEnumCases[A: WriterCtx](
        enumm: Enum[A]
    ): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      @scala.annotation.nowarn("msg=is never used")
      implicit val StringT: Type[String] = WTypes.String
      implicit val FuncT: Type[String => String] = WTypes.StringToString
      implicit val OptionStringT: Type[Option[String]] = WTypes.OptionString
      implicit val FieldHintT: Type[CoproductHint.Field[A]] = WTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[CoproductHint.Wrapped[A]] = WTypes.wrappedCoproductHintType[A]

      val maybeFieldHint: Option[Expr[CoproductHint.Field[A]]] =
        Expr.summonImplicit[CoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[CoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[CoproductHint.Wrapped[A]].toOption

      val transformConstructorNamesExpr: Expr[String => String] = maybeFieldHint match {
        case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
        case None    =>
          maybeWrappedHint match {
            case Some(h) => Expr.quote(Expr.splice(h).transformConstructorNames)
            case None    => Expr.quote(Expr.splice(wctx.config).transformConstructorNames)
          }
      }

      val discrFieldExpr: Expr[Option[String]] = maybeFieldHint match {
        case Some(h) => Expr.quote((Some(Expr.splice(h).fieldName): Option[String]))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => Expr.quote(None: Option[String])
            case None    => Expr.quote(Expr.splice(wctx.config).discriminator)
          }
      }

      val childrenList = enumm.directChildren.toList
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, ConfigValue](wctx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Writing enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseValueMIO: MIO[Expr[ConfigValue]] =
              if (isEnumerationOrJavaEnum)
                MIO.pure(Expr.quote(SConfigDerivationUtils.emptyConfigObject))
              else deriveWriterRecursively[EnumCase](using wctx.nest(enumCaseValue))
            caseValueMIO.map { caseValue =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              if (allCaseObjects) {
                Expr.quote {
                  val name = Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(caseName)))
                  Expr.splice(discrFieldExpr) match {
                    case Some(discriminatorField) =>
                      SConfigDerivationUtils.addDiscriminator(
                        discriminatorField,
                        name,
                        Expr.splice(caseValue)
                      )
                    case None =>
                      SConfigDerivationUtils.writeEnumAsString(name)
                  }
                }
              } else {
                Expr.quote {
                  val name = Expr.splice(transformConstructorNamesExpr)(Expr.splice(Expr(caseName)))
                  Expr.splice(discrFieldExpr) match {
                    case Some(discriminatorField) =>
                      SConfigDerivationUtils.addDiscriminator(
                        discriminatorField,
                        name,
                        Expr.splice(caseValue)
                      )
                    case None =>
                      SConfigDerivationUtils.wrapWithTypeName(name, Expr.splice(caseValue))
                  }
                }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = WriterDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }
}
