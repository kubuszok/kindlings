package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema

trait SchemaHandleAsValueTypeRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  /** An `AnyVal` case class (not a primitive, named tuple or singleton). */
  private def anyValCaseClass[A: Type]: Option[CaseClass[A]] =
    if (Type[A] <:< Type.of[AnyVal] && !Type[A].isPrimitive && !Type[A].isNamedTuple)
      CaseClass.parse[A].toOption.filter(_ => SingletonValue.unapply(Type[A]).isEmpty)
    else None

  object SchemaHandleAsValueTypeRule extends SchemaDerivationRule("handle as value type when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrapped: MIO[Expr[Schema[A]]] =
              Log.info(s"Deriving Schema for value type ${Type[A].prettyPrint} as ${Type[Inner].prettyPrint}") >>
                deriveSchemaRecursively[Inner](using sctx.nest[Inner]).map { innerSchema =>
                  // A value type wrapping a collection or map is coded as that collection, but the decoders do not fill
                  // it in when the field is absent (the wrapper has no empty value), so unlike the collection it is
                  // required.
                  if (collectionCanBeEmpty[Inner].isDefined) {
                    @scala.annotation.nowarn("msg=is never used")
                    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
                    Expr.quote(
                      TapirSchemaUtils
                        .setFieldOptional(Expr.splice(innerSchema).asInstanceOf[Schema[Any]], false)
                        .asInstanceOf[Schema[A]]
                    )
                  } else Expr.quote(Expr.splice(innerSchema).asInstanceOf[Schema[A]])
                }
            // An `AnyVal` case class that the JSON library may code as an object (circe's `unwrapValueClasses = false`):
            // the unwrapped or the product schema, chosen at runtime (cached as a whole).
            (sctx.jsonCfg.unwrapsValueClasses, anyValCaseClass[A]) match {
              case (Some(unwrap), Some(caseClass)) =>
                implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
                for {
                  inner <- unwrapped
                  product <- buildCaseClassSchema[A](caseClass)
                  result <- setCachedAndGet[A](
                    sctx.cache,
                    Expr.quote(if (Expr.splice(unwrap)) Expr.splice(inner) else Expr.splice(product))
                  )
                } yield Rule.matched(result)
              case _ => unwrapped.map(Rule.matched)
            }
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
