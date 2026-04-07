package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import org.ekrich.config.ConfigValueType

trait ReaderHandleAsOptionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsOptionRule extends ReaderDerivationRule("handle as Option when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
            @scala.annotation.nowarn("msg=is never used")
            implicit val EitherInnerT: Type[Either[ConfigDecodingError, Inner]] = RTypes.ReaderResult[Inner]
            @scala.annotation.nowarn("msg=is never used")
            implicit val OptionInnerT: Type[Option[Inner]] = RTypes.optionType[Inner]

            for {
              innerResult <- deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.value))
            } yield Rule.matched(Expr.quote {
              val v = Expr.splice(rctx.value)
              if (v == null || v.valueType == ConfigValueType.NULL)
                Right(None.asInstanceOf[A]): Either[ConfigDecodingError, A]
              else
                Expr.splice(innerResult).map((inner: Inner) => Some(inner).asInstanceOf[A])
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
