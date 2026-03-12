package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.KindlingsXmlEncoder

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsXmlEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use implicit XmlEncoder for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.XmlEncoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val EncoderAT: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] = Types.XmlEncoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
        instanceExpr.upcast[hearth.kindlings.xmlderivation.XmlEncoder[A]]
      Log.info(s"Found implicit XML encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(publicInstance).encode(Expr.splice(ectx.value), Expr.splice(ectx.elementName))
        }))
    }

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit XmlEncoder instance: $reason"
        )
      )
  }

}
