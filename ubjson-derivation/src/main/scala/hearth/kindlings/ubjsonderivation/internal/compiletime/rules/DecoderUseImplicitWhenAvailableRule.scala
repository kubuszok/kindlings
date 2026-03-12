package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.KindlingsUBJsonValueCodec

trait DecoderUseImplicitWhenAvailableRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderUseImplicitWhenAvailableRule extends DecoderDerivationRule("use implicit when available") {

    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val ours = Type.of[KindlingsUBJsonValueCodec.type].methods.collect {
        case method if method.value.name == "derived" => method.value.asUntyped
      }
      ours
    }

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to use implicit UBJsonValueCodec for ${Type[A].prettyPrint}") >> {
        if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          CTypes.UBJsonValueCodec[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) =>
              Log.info(s"Found implicit codec ${instanceExpr.prettyPrint}, using directly") >>
                MIO.pure(Rule.matched(Expr.quote {
                  Expr.splice(instanceExpr).decode(Expr.splice(dctx.reader))
                }))
            case Left(reason) =>
              MIO.pure(
                Rule.yielded(
                  s"The type ${Type[A].prettyPrint} does not have an implicit UBJsonValueCodec instance: $reason"
                )
              )
          }
      }
  }
}
