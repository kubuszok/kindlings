package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.typesafe.config.{ConfigValue, ConfigValueFactory}

trait WriterHandleAsOptionRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsOptionRule extends WriterDerivationRule("handle as Option when possible") {
    implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveWriterRecursively[Inner](using wctx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[ConfigValue]
                Rule.matched(
                  isOption.value.fold[ConfigValue](wctx.value)(
                    onEmpty = Expr.quote(ConfigValueFactory.fromAnyRef(null)),
                    onSome = innerExpr =>
                      Expr.quote {
                        Expr.splice(lambda).apply(Expr.splice(innerExpr))
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
