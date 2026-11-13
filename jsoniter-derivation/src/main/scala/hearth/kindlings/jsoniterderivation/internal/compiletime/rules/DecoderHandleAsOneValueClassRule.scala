package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.annotations.transientField

trait DecoderHandleAsOneValueClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsOneValueClassRule
      extends DecoderDerivationRule("handle as one-value-class when inlineOneValueClasses is set") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      // `Some(flag)` with a compile-time config; `None` when only known at runtime: then both shapes are derived and
      // the codec checks `config.inlineOneValueClasses` when decoding.
      val staticInline = dctx.evaluatedConfig.map(_.inlineOneValueClasses)
      if (staticInline.contains(false))
        MIO.pure(Rule.yielded("inlineOneValueClasses is not enabled"))
      else
        oneValueClassField[A](dctx.evaluatedConfig) match {
          case Some((cc, fName, param)) =>
            import param.tpe.Underlying as Field
            deriveDecoderRecursively[Field](using dctx.nest[Field](dctx.reader)).flatMap { decodedField =>
              constructOneValueClass[A](cc, fName, decodedField.as_??) match {
                case Right(inlined) =>
                  if (staticInline.contains(true)) MIO.pure(Rule.matched(inlined))
                  else
                    DecoderHandleAsCaseClassRule[A].map {
                      case Rule.Applicability.Matched(asObject) =>
                        Rule.matched(Expr.quote {
                          if (Expr.splice(dctx.config).inlineOneValueClasses) Expr.splice(inlined)
                          else Expr.splice(asObject)
                        })
                      case yielded => yielded
                    }
                case Left(error) =>
                  val err =
                    CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                  Log.error(err.message) >> MIO.fail(err)
              }
            }
          case None =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a class with exactly one non-transient field"))
        }
    }
  }
}
