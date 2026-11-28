package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.{KindlingsCodecAsObject, KindlingsDecoder}
import io.circe.{Decoder, DecodingFailure}

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsDecoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[KindlingsCodecAsObject.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[Decoder.type].unsortedMethods.collect {
        case method if method.name == "derived" || method.name.startsWith("decodeLiteral") =>
          method.asUntyped
      }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to use implicit Decoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val dec: Decoder[X] = KindlingsDecoder.derived[X]` would otherwise
        // find `dec` itself during macro expansion, generating code that calls itself infinitely).
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else {
          implicit val DecoderA: Type[Decoder[A]] = DTypes.Decoder[A]
          summonUserInstance[Decoder[A]](ignoredImplicits) match {
            case UserInstanceSearch.Found(instanceExpr) => cacheAndUse[A](instanceExpr)
            case UserInstanceSearch.Crashed(reason)     =>
              failOnCrashedUserInstance[Decoder[A], Rule.Applicability[Expr[Either[DecodingFailure, A]]]](reason)
            case UserInstanceSearch.NotFound(reason) =>
              // a user's generic instance that needs instances Kindlings derives (e.g. `Decoder[Page[Item]]` built
              // from a derived `Decoder[Item]`): the completed instance is a lazy val of the derived instance
              completeUserDecoder[A](dctx).flatMap {
                case Some(instanceExpr) =>
                  dctx
                    .cachedLazy[Decoder[A]]("cached-completed-decoder", s"decoder_${Type[A].shortName}", instanceExpr)
                    .flatMap(cacheAndUse[A](_))
                case None => yieldUnsupported[A](reason)
              }
          }
        }
      }

    private def cacheAndUse[A: DecoderCtx](
        instanceExpr: Expr[Decoder[A]]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(runDecoderInstance[A](instanceExpr)))

    private def yieldUnsupported[A: DecoderCtx](
        reason: String
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Decoder instance: $reason"
        )
      )
  }
}
