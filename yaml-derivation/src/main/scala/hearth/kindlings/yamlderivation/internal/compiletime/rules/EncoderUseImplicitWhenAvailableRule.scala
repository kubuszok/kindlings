package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.KindlingsYamlEncoder

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsYamlEncoder.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[org.virtuslab.yaml.Node]]] =
      Log.info(s"Attempting to use implicit YamlEncoder for ${Type[A].prettyPrint}") >> {
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.YamlEncoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[org.virtuslab.yaml.YamlEncoder[A]]
    ): MIO[Rule.Applicability[Expr[org.virtuslab.yaml.Node]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).asNode(Expr.splice(ectx.value))
        }))

    private def yieldUnsupported[A: EncoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[org.virtuslab.yaml.Node]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit YamlEncoder instance: $reason"
        )
      )
  }
}
