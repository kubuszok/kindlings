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
      val staticInline = ectx.evaluatedConfig.exists(_.inlineOneValueClasses)
      if (!staticInline)
        MIO.pure(Rule.yielded("inlineOneValueClasses is not enabled (requires compile-time evaluable config)"))
      else
        CaseClass.parse[A].toEither match {
          case Right(cc) =>
            val fields = cc.primaryConstructor.parameters.flatten.toList.filter { case (_, p) =>
              !hasAnnotationType[transientField](p)
            }
            fields match {
              case (fName, param) :: Nil if IsValueType.unapply(Type[A]).isEmpty =>
                import param.tpe.Underlying as Field
                val allFields = cc.caseFieldValuesAt(ectx.value).toList
                allFields.find(_._1 == fName) match {
                  case Some((_, fieldValue)) =>
                    val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map(Rule.matched)
                  case None =>
                    MIO.pure(Rule.yielded(s"Could not access field $fName"))
                }
              case _ =>
                MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} does not have exactly one non-transient field"))
            }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
    }
  }
}
