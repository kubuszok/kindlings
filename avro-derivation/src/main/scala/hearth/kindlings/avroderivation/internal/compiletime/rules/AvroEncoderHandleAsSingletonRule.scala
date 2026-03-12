package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.apache.avro.generic.GenericData

trait AvroEncoderHandleAsSingletonRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsSingletonRule extends EncoderDerivationRule("handle as singleton when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            deriveSelfContainedSchema[A](ectx.config).map { schemaExpr =>
              Rule.matched(Expr.quote {
                val record = new GenericData.Record(Expr.splice(schemaExpr))
                record: Any
              })
            }
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
