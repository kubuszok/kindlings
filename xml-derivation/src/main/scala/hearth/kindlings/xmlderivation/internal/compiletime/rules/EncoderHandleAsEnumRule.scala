package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlConfig
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as sealed trait/enum when possible") {
    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val StringT: Type[String] = Types.String

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a sealed trait/enum") >> {
        Enum.parse[A].toEither match {
          case Right(parsedEnum) =>
            deriveEnumEncoder[A](parsedEnum)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumEncoder[A: EncoderCtx](
        enumType: Enum[A]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val XmlConfigT: Type[XmlConfig] = Types.XmlConfig
      val childrenList = enumType.directChildren.toList

      // Pre-compute discriminator attribute name from config
      val discriminatorAttr: Option[String] = ectx.evaluatedConfig.flatMap(_.discriminatorAttribute)
      // When evaluatedConfig is not available, default to "type"
      val discriminatorAttrName: String = discriminatorAttr.getOrElse("type")
      val hasDiscriminator: Option[Boolean] = ectx.evaluatedConfig.map(_.discriminatorAttribute.isDefined)

      enumType
        .parMatchOn[MIO, scala.xml.Elem](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue)).map { caseElem =>
              val rawCaseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)

              // Apply constructorNameMapper when evaluatedConfig is available
              val mappedCaseName: String = ectx.evaluatedConfig match {
                case Some(cfg) => cfg.constructorNameMapper(rawCaseName)
                case None      => rawCaseName
              }

              hasDiscriminator match {
                case Some(true) =>
                  // Discriminator is statically known to be present
                  Expr.quote {
                    XmlDerivationUtils
                      .addDiscriminator(
                        Expr.splice(caseElem),
                        Expr.splice(Expr(discriminatorAttrName)),
                        Expr.splice(Expr(mappedCaseName))
                      )
                  }
                case Some(false) =>
                  // Discriminator is statically known to be absent — wrap with type name
                  Expr.quote {
                    XmlDerivationUtils.wrapWithTypeName(Expr.splice(Expr(mappedCaseName)), Expr.splice(caseElem))
                  }
                case None =>
                  // evaluatedConfig not available — use runtime check
                  Expr.quote {
                    val config = Expr.splice(ectx.config)
                    config.discriminatorAttribute match {
                      case scala.Some(attr) =>
                        XmlDerivationUtils.addDiscriminator(
                          Expr.splice(caseElem),
                          attr,
                          config.constructorNameMapper(Expr.splice(Expr(rawCaseName)))
                        )
                      case scala.None =>
                        XmlDerivationUtils.wrapWithTypeName(
                          config.constructorNameMapper(Expr.splice(Expr(rawCaseName))),
                          Expr.splice(caseElem)
                        )
                    }
                  }
              }
            }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(Rule.matched(result))
          case None         =>
            val err = EncoderDerivationError.UnsupportedType(Type[A].prettyPrint, List("Enum has no children"))
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

}
