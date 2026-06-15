package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.MacroCommons
import hearth.std.StdExtensions
import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}

final class JsoniterJsonFieldConfigExtension extends JsonSchemaConfigExtension {

  override protected def extendJsonConfig(ctx: MacroCommons & StdExtensions & JsonSchemaConfigs): Unit = {
    import ctx.*

    // Pre-resolve ExprCodec[String] with explicit companion access to avoid
    // diverging implicit expansion caused by import ctx.* in Scala 2. Harmless on
    // Scala 3, so this platform-agnostic file uses it on both platforms.
    val stringCodec = ExprCodec.StringExprCodec
    def mkStringExpr(s: String): Expr[String] = Expr.apply(s)(stringCodec)

    // Self-referential implicit val Type.of definitions now work in cross-quotes (Hearth issue #285).
    implicit val ConfigT: Type[JsoniterConfig] = Type.of[JsoniterConfig]
    implicit val FieldNameT: Type[annotations.fieldName] = Type.of[annotations.fieldName]
    implicit val TransientFieldT: Type[annotations.transientField] = Type.of[annotations.transientField]

    Expr.summonImplicit[JsoniterConfig].toOption match {
      case Some(configExpr) =>
        ctx.JsonSchemaConfig.register(new ctx.JsonSchemaConfig {
          def libraryName: String = "jsoniter-scala"
          def configClassName: String = "hearth.kindlings.jsoniterderivation.JsoniterConfig"
          def configType: UntypedType = UntypedType.fromTyped[JsoniterConfig]

          def resolveFieldName(param: Parameter, scalaName: String): Expr[String] = {
            // Native Hearth annotation API (Hearth issue #283): read the @fieldName(...) String literal.
            val customName: Option[String] = param
              .annotationsOfType[annotations.fieldName]
              .headOption
              .flatMap(ann => Annotations.decodedConstructorArguments(ann))
              .flatMap(_.headOption.flatMap(_.toOption))
              .collect { case s: String => s }
            customName match {
              case Some(name) => mkStringExpr(name)
              case None       =>
                Expr.quote {
                  Expr.splice(configExpr).fieldNameMapper(Expr.splice(mkStringExpr(scalaName)))
                }
            }
          }

          def isTransientField(param: Parameter): Boolean =
            param.hasAnnotationOfType[annotations.transientField]

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
