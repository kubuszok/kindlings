package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.typesafe.config.ConfigValue

trait WriterHandleAsValueTypeRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsValueTypeRule extends WriterDerivationRule("handle as value type when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrappedExpr = isValueType.value.unwrap(wctx.value)
            for {
              innerResult <- deriveWriterRecursively[Inner](using wctx.nest(unwrappedExpr))
            } yield Rule.matched(innerResult)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
