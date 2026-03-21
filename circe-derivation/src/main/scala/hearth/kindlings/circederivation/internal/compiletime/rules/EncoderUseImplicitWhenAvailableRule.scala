package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.{KindlingsCodecAsObject, KindlingsEncoder}
import io.circe.{Encoder, Json}

trait EncoderUseImplicitWhenAvailableRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderUseImplicitWhenAvailableRule extends EncoderDerivationRule("use implicit when available") {

    // Ignore ALL implicit/given methods from Kindlings companion objects (our auto-derivation).
    // For circe's own companions, only ignore specific auto-derivation methods (not built-in
    // instances like encodeInt, encodeString, etc.).
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsEncoder.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      } ++ Type.of[KindlingsCodecAsObject.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      } ++ Type.of[Encoder.type].methods.collect {
        case method if method.value.name == "derived" || method.value.name.startsWith("encodeLiteral") =>
          method.value.asUntyped
      }

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to use implicit Encoder for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val enc: Encoder[X] = KindlingsEncoder.derived[X]` would otherwise
        // find `enc` itself during macro expansion, generating code that calls itself infinitely).
        if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.Encoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheAndUse[A](instanceExpr)
            case Left(reason)        => yieldUnsupported[A](reason)
          }
      }

    private def cacheAndUse[A: EncoderCtx](
        instanceExpr: Expr[Encoder[A]]
    ): MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Found implicit encoder ${instanceExpr.prettyPrint}, using directly") >>
        MIO.pure(Rule.matched(Expr.quote {
          Expr.splice(instanceExpr).apply(Expr.splice(ectx.value))
        }))

    private def yieldUnsupported[A: EncoderCtx](reason: String): MIO[Rule.Applicability[Expr[Json]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit Encoder instance: $reason"
        )
      )
  }
}
