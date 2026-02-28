package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.{MacroCommons, MacroCommonsScala2}
import hearth.std.StdExtensions
import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}

final class JsoniterJsonFieldConfigExtension extends JsonSchemaConfigExtension {

  @scala.annotation.nowarn("msg=is unchecked since it is eliminated by erasure")
  override protected def extendJsonConfig(ctx: MacroCommons & StdExtensions & JsonSchemaConfigs): Unit = {
    import ctx.*

    // Access Scala 2 context for annotation tree matching with preserved type info
    val sc2 = ctx.asInstanceOf[MacroCommonsScala2]
    import sc2.c.universe.{Apply as SApply, Literal as SLiteral, Constant as SConstant}

    // Pre-resolve ExprCodec[String] with explicit companion access to avoid
    // diverging implicit expansion caused by import ctx.* in Scala 2.
    val stringCodec = ExprCodec.StringExprCodec
    def mkStringExpr(s: String): Expr[String] = Expr.apply(s)(stringCodec)

    // Bootstrap Type[JsoniterConfig] from raw Scala 2 compiler type, bypassing
    // cross-quotes Type.of which causes SOE: on Scala 2, the Type.of[A] macro
    // expansion generates an implicit conversion (Type[T] → WeakTypeTag[T]) that
    // resolves Type[A] at runtime — creating an inescapable self-referential cycle
    // when the very Type[A] being defined IS the implicit in scope.
    // Using sc2.c.universe.typeOf[X] + UntypedType.toTyped avoids this entirely.
    implicit val ConfigT: Type[JsoniterConfig] =
      UntypedType.toTyped[JsoniterConfig](sc2.c.universe.typeOf[JsoniterConfig].asInstanceOf[UntypedType])

    // Annotation types only need UntypedType for comparison — get directly from
    // Scala 2 universe to avoid additional cross-quotes Type.of forward references.
    val fieldNameTpe: UntypedType = sc2.c.universe.typeOf[annotations.fieldName].asInstanceOf[UntypedType]
    val transientFieldTpe: UntypedType = sc2.c.universe.typeOf[annotations.transientField].asInstanceOf[UntypedType]

    Expr.summonImplicit[JsoniterConfig].toOption match {
      case Some(configExpr) =>
        ctx.JsonSchemaConfig.register(new ctx.JsonSchemaConfig {
          def libraryName: String = "jsoniter-scala"
          def configType: UntypedType = UntypedType.fromTyped[JsoniterConfig]

          def resolveFieldName(param: Parameter, scalaName: String): Expr[String] = {
            // On Scala 2, param.asUntyped.annotations strips types via c.untypecheck.
            // Access symbol.annotations directly for type-preserved annotation info.
            val sc2Param = param.asUntyped.asInstanceOf[sc2.UntypedParameter]
            val customName: Option[String] = sc2Param.symbol.annotations
              .find(ann => ann.tree.tpe.asInstanceOf[UntypedType] =:= fieldNameTpe)
              .flatMap { ann =>
                sc2.c.untypecheck(ann.tree) match {
                  case SApply(_, List(SLiteral(SConstant(value: String)))) => Some(value)
                  case _                                                   => None
                }
              }
            customName match {
              case Some(name) => mkStringExpr(name)
              case None       =>
                Expr.quote {
                  Expr.splice(configExpr).fieldNameMapper(Expr.splice(mkStringExpr(scalaName)))
                }
            }
          }

          def isTransientField(param: Parameter): Boolean = {
            val sc2Param = param.asUntyped.asInstanceOf[sc2.UntypedParameter]
            sc2Param.symbol.annotations.exists(ann => ann.tree.tpe.asInstanceOf[UntypedType] =:= transientFieldTpe)
          }

          def resolveConstructorName(scalaName: String): Expr[String] =
            Expr.quote {
              Expr.splice(configExpr).adtLeafClassNameMapper(Expr.splice(mkStringExpr(scalaName)))
            }

          def discriminatorFieldName: Expr[Option[String]] =
            Expr.quote(Expr.splice(configExpr).discriminatorFieldName)

          def enumAsStrings: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).enumAsStrings)

          def useDefaults: Expr[Boolean] =
            Expr.quote(false)

          def fieldsWithDefaultsAreOptional: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).transientDefault)

          def emptyFieldsAreOptional: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).transientEmpty)

          def mapsAreArrays: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).mapAsArray)

          def numericFieldsAsStrings: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).isStringified)
        })
      case None => () // no JsoniterConfig in scope
    }
  }
}
