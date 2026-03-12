package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsSingletonRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsSingletonRule extends DecoderDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(sv) =>
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
            MIO.pure(Rule.matched(Expr.quote {
              JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
              Expr.splice(sv.singletonExpr)
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }

}
