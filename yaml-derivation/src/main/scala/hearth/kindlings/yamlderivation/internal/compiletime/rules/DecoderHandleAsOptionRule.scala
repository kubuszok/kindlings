package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsOptionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOptionRule extends DecoderDerivationRule("handle as Option when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            @scala.annotation.nowarn("msg=is never used")
            implicit val NodeT: Type[Node] = DTypes.Node
            @scala.annotation.nowarn("msg=is never used")
            implicit val EitherCEInner: Type[Either[ConstructError, Inner]] = DTypes.DecoderResult[Inner]

            LambdaBuilder
              .of1[Node]("innerNode")
              .traverse { innerNodeExpr =>
                deriveDecoderRecursively[Inner](using dctx.nest[Inner](innerNodeExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConstructError, Inner]]
                Rule.matched(Expr.quote {
                  YamlDerivationUtils
                    .decodeOptionFromFn(
                      Expr.splice(dctx.node),
                      Expr.splice(decodeFn)
                    )
                    .asInstanceOf[Either[ConstructError, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
