package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

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

    private def deriveEnumEncoder[A: EncoderCtx](
        enumType: Enum[A]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      val childrenList = enumType.directChildren.toList

      enumType
        .parMatchOn[MIO, scala.xml.Elem](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue)).map { caseElem =>
              val caseName: String = childrenList
                .find { case (_, child) =>
                  import child.Underlying as ChildType
                  Type[EnumCase] <:< Type[ChildType]
                }
                .map(_._1)
                .getOrElse(Type[EnumCase].shortName)
              Expr.quote {
                XmlDerivationUtils.addDiscriminator(Expr.splice(caseElem), "type", Expr.splice(Expr(caseName)))
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
