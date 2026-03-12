package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EncoderUseCachedDefWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use cached XML encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    @scala.annotation.nowarn("msg=is never used")
    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] = {
      implicit val EncoderAT: Type[hearth.kindlings.xmlderivation.XmlEncoder[A]] = Types.XmlEncoder[A]
      val publicInstance: Expr[hearth.kindlings.xmlderivation.XmlEncoder[A]] =
        instance.upcast[hearth.kindlings.xmlderivation.XmlEncoder[A]]
      Log.info(s"Found cached XML encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(publicInstance).encode(Expr.splice(ectx.value), Expr.splice(ectx.elementName))
      }))
    }

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[String], Expr[hearth.kindlings.xmlderivation.XmlConfig]) => Expr[scala.xml.Elem]
    ): MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Found cached XML encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.elementName, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached XML encoder"))
  }

}
