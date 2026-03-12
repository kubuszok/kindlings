package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.YamlConfig
import org.virtuslab.yaml.Node

trait EncoderUseCachedDefWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseCachedDefWhenAvailableRule extends EncoderDerivationRule("use cached def when available") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to use cached encoder for ${Type[A].prettyPrint}") >>
        ectx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ectx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: EncoderCtx](
        instance: Expr[org.virtuslab.yaml.YamlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Found cached encoder instance for ${Type[A].prettyPrint}") >> MIO.pure(Rule.matched(Expr.quote {
        Expr.splice(instance).asNode(Expr.splice(ectx.value))
      }))

    private def callCachedHelper[A: EncoderCtx](
        helperCall: (Expr[A], Expr[YamlConfig]) => Expr[Node]
    ): MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Found cached encoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(helperCall(ectx.value, ectx.config))
      )

    private def yieldUnsupported[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached encoder"))
  }
}
