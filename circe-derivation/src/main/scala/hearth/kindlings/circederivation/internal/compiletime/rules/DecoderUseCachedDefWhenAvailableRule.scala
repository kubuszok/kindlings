package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.Configuration
import io.circe.{Decoder, DecodingFailure, HCursor}

trait DecoderUseCachedDefWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseCachedDefWhenAvailableRule extends DecoderDerivationRule("use cached def when available") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to use cached decoder for ${Type[A].prettyPrint}") >>
        dctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            dctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupported[A]
            }
        }

    private def callCachedInstance[A: DecoderCtx](
        instance: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found cached decoder instance for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(instance).apply(Expr.splice(dctx.cursor))
        })
      )

    private def callCachedHelper[A: DecoderCtx](
        helperCall: (Expr[HCursor], Expr[Configuration], Expr[Boolean]) => Expr[Any]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      Log.info(s"Found cached decoder helper for ${Type[A].prettyPrint}") >> MIO.pure(
        Rule.matched(Expr.quote {
          Expr.splice(helperCall(dctx.cursor, dctx.config, dctx.failFast)).asInstanceOf[Either[DecodingFailure, A]]
        })
      )
    }

    private def yieldUnsupported[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached decoder"))
  }
}
