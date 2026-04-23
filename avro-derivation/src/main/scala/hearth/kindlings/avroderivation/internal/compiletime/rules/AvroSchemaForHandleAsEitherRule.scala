package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsEitherRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsEitherRule extends SchemaDerivationRule("handle as Either when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Either") >> {
        Type[A] match {
          case IsEither(isEither) =>
            import isEither.{LeftValue, RightValue}
            for {
              leftSchema <- deriveSchemaRecursively[LeftValue](using sfctx.nest[LeftValue])
              rightSchema <- deriveSchemaRecursively[RightValue](using sfctx.nest[RightValue])
            } yield Rule.matched(Expr.quote {
              AvroDerivationUtils.createSafeUnion(
                Expr.splice(leftSchema),
                Expr.splice(rightSchema)
              )
            })

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Either"))
        }
      }
  }
}
