package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsMapRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object ReaderHandleAsMapRule extends ReaderDerivationRule("handle as map when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def decodeMapEntries[A: ReaderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = RTypes.String
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val EitherValueT: Type[Either[ConfigDecodingError, Value]] = RTypes.ReaderResult[Value]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else
        LambdaBuilder
          .of1[ConfigValue]("valueValue")
          .traverse { valueValueExpr =>
            deriveReaderRecursively[Value](using rctx.nest[Value](valueValueExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConfigDecodingError, Value]]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              SConfigDerivationUtils
                .decodeMapWith[Value, A](
                  Expr.splice(rctx.value),
                  SConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn)),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[Either[ConfigDecodingError, A]]
            })
          }
    }
  }
}
