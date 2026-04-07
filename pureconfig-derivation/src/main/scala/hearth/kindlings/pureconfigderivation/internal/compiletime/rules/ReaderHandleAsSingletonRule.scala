package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsSingletonRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsSingletonRule extends ReaderDerivationRule("handle as singleton when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
            // Singleton case objects don't carry data, so we just return the object
            // regardless of what the cursor contains. The enclosing rule (case-class /
            // enum) is responsible for ensuring the cursor structure matches.
            MIO.pure(Rule.matched(Expr.quote {
              Right(Expr.splice(sv.singletonExpr)): Either[ConfigReaderFailures, A]
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
