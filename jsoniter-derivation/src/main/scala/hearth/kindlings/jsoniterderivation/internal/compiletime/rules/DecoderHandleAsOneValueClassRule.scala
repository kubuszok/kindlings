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
      val staticInline = dctx.evaluatedConfig.exists(_.inlineOneValueClasses)
      if (!staticInline)
        MIO.pure(Rule.yielded("inlineOneValueClasses is not enabled"))
      else
        CaseClass.parse[A].toEither match {
          case Right(cc) =>
            val fields = cc.primaryConstructor.totalParameters.flatten.toList.filter { case (_, p) =>
              !hasAnnotationType[transientField](p)
            }
            fields match {
              case (fName, param) :: Nil if IsValueType.unapply(Type[A]).isEmpty =>
                import param.tpe.Underlying as Field
                deriveDecoderRecursively[Field](using dctx.nest[Field](dctx.reader)).flatMap { decodedField =>
                  cc.primaryConstructor.fold(
                    onInstance = _ => throw new RuntimeException("Constructor should not need instance"),
                    onTypes = _ => Map.empty,
                    onValues = _ => Map(fName -> decodedField.as_??)
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(Rule.matched(constructExpr.value.asInstanceOf[Expr[A]]))
                    case Left(error) =>
                      MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
                  }
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
