package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.AvroDecoder

trait AvroDecoderUseImplicitWhenAvailableRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    /** Methods to exclude from `summonExprIgnoring` to prevent infinite macro expansion when a user imports our own
      * auto-derivation methods (e.g. `AvroDecoder.derived`). The list intentionally only includes methods from the
      * kindlings `AvroDecoder` companion: avro-derivation depends solely on `org.apache.avro:avro` (the pure Java
      * library), which has no Scala-side implicit auto-derivation surface — there is no third-party library equivalent
      * (like circe-generic-auto or jsoniter-scala-macros) whose `derived`/`gen` methods could recurse via implicit
      * search. If avro4s or another auto-derivation library is ever added as a dependency, its derivation entry points
      * must be added here.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[AvroDecoder.type].methods.collect {
        case method if method.value.isImplicit => method.value.asUntyped
      }

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit AvroDecoder for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          DecTypes.AvroDecoder[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit decoder ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).decode(Expr.splice(dctx.avroValue))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit AvroDecoder instance: $reason"
                )
              )
          }
      }
  }
}
