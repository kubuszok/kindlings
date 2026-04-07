package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsOptionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsOptionRule extends ReaderDerivationRule("handle as Option when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val EitherT: Type[Either[ConfigReaderFailures, A]] = RTypes.ReaderResult[A]
            @scala.annotation.nowarn("msg=is never used")
            implicit val EitherInnerT: Type[Either[ConfigReaderFailures, Inner]] = RTypes.ReaderResult[Inner]
            @scala.annotation.nowarn("msg=is never used")
            implicit val OptionInnerT: Type[Option[Inner]] = RTypes.optionType[Inner]

            for {
              innerResult <- deriveReaderRecursively[Inner](using rctx.nest[Inner](rctx.cursor))
            } yield Rule.matched(Expr.quote {
              val cur = Expr.splice(rctx.cursor)
              if (cur.isUndefined || cur.isNull)
                Right(None.asInstanceOf[A]): Either[ConfigReaderFailures, A]
              else
                Expr
                  .splice(innerResult)
                  .map((inner: Inner) => Some(inner).asInstanceOf[A])
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
