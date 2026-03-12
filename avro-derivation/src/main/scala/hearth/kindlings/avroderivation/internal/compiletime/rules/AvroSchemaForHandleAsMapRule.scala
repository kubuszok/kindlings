package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.apache.avro.Schema

trait AvroSchemaForHandleAsMapRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object AvroSchemaForHandleAsMapRule extends SchemaDerivationRule("handle as map when possible") {
    implicit val SchemaT: Type[Schema] = SfTypes.Schema
    implicit val StringT: Type[String] = SfTypes.String

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapSchema[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapSchema[A: SchemaForCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Schema]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        for {
          valueSchema <- deriveSchemaRecursively[Value](using sfctx.nest[Value])
        } yield Rule.matched(Expr.quote {
          Schema.createMap(Expr.splice(valueSchema))
        })
      }
    }
  }
}
