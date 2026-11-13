package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.annotations.transientField

trait EncoderHandleAsOneValueClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsOneValueClassRule
      extends EncoderDerivationRule("handle as one-value-class when inlineOneValueClasses is set") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] = {
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      // `Some(flag)` with a compile-time config; `None` when only known at runtime: then both shapes are derived and
      // the codec checks `config.inlineOneValueClasses` when encoding.
      val staticInline = ectx.evaluatedConfig.map(_.inlineOneValueClasses)
      if (staticInline.contains(false))
        MIO.pure(Rule.yielded("inlineOneValueClasses is not enabled"))
      else
        oneValueClassField[A](ectx.evaluatedConfig) match {
          case Some((cc, fName, param)) =>
            import param.tpe.Underlying as Field
            cc.fieldValuesAt(ectx.value).get(fName) match {
              case Some(fieldValue) =>
                val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).flatMap { inlined =>
                  if (staticInline.contains(true)) MIO.pure(Rule.matched(inlined))
                  else
                    EncoderHandleAsCaseClassRule[A].map {
                      case Rule.Applicability.Matched(asObject) =>
                        Rule.matched(Expr.quote {
                          if (Expr.splice(ectx.config).inlineOneValueClasses) Expr.splice(inlined)
                          else Expr.splice(asObject)
                        })
                      case yielded => yielded
                    }
                }
              case None =>
                MIO.pure(Rule.yielded(s"Could not access field $fName"))
            }
          case None =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a class with exactly one non-transient field"))
        }
    }
  }
}
