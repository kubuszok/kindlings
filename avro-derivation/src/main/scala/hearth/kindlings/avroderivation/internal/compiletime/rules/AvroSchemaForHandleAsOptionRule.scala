package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsOptionRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsOptionRule extends SchemaDerivationRule("handle as Option when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            for {
              innerSchema <- deriveSchemaRecursively[Inner](using sfctx.nest[Inner])
            } yield Rule.matched(Expr.quote {
              AvroDerivationUtils.createSafeUnion(
                AvroDerivationUtils.nullSchema,
                Expr.splice(innerSchema)
              )
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
