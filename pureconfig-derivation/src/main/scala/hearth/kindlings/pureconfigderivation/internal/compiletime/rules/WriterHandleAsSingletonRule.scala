package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils

trait WriterHandleAsSingletonRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsSingletonRule extends WriterDerivationRule("handle as singleton when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            // Singleton case objects encode as an empty config object; the enclosing
            // enum rule wraps them with a discriminator / type-name key.
            MIO.pure(Rule.matched(Expr.quote {
              PureConfigDerivationUtils.emptyConfigObject
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
