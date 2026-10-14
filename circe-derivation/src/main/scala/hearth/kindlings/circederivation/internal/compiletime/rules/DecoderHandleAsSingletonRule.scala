package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsSingletonRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
            @scala.annotation.nowarn("msg=is never used")
            implicit val HCursorT: Type[HCursor] = DTypes.HCursor
            // Singletons are encoded as `{}`, and circe's derived decoders require an object (bug #235).
            MIO.pure(Rule.matched(Expr.quote {
              CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).map(_ => Expr.splice(sv.singletonExpr))
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
