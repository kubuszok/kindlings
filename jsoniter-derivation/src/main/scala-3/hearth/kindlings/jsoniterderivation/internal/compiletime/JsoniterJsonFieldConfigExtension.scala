package hearth.kindlings.jsoniterderivation
package internal.compiletime

import hearth.{MacroCommons, MacroCommonsScala3}
import hearth.std.StdExtensions
import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}

final class JsoniterJsonFieldConfigExtension extends JsonSchemaConfigExtension {

  override protected def extendJsonConfig(ctx: MacroCommons & StdExtensions & JsonSchemaConfigs): Unit = {
    import ctx.*

    // Access Scala 3 quotes.reflect for annotation tree pattern matching
    val sc3 = ctx.asInstanceOf[MacroCommonsScala3]
    import sc3.quotes.reflect.{Apply as QApply, Literal as QLiteral, StringConstant as QStringConstant, Term as QTerm}

    // Bootstrap Type values by bypassing cross-quotes Type.of, which on both Scala 2
    // and 3 generates code that resolves implicit/given Type[A] — causing an
    // inescapable self-referential cycle when the Type[A] being defined IS the
    // implicit in scope. Use scala.quoted.Type.of directly (only needs Quotes).
    given scala.quoted.Quotes = sc3.quotes
    implicit val ConfigT: Type[JsoniterConfig] =
      scala.quoted.Type.of[JsoniterConfig].asInstanceOf[Type[JsoniterConfig]]
    implicit val FieldNameT: Type[annotations.fieldName] =
      scala.quoted.Type.of[annotations.fieldName].asInstanceOf[Type[annotations.fieldName]]
    implicit val TransientFieldT: Type[annotations.transientField] =
      scala.quoted.Type.of[annotations.transientField].asInstanceOf[Type[annotations.transientField]]

    Expr.summonImplicit[JsoniterConfig].toOption match {
      case Some(configExpr) =>
        val fieldNameTpe: UntypedType = UntypedType.fromTyped[annotations.fieldName]
        val transientFieldTpe: UntypedType = UntypedType.fromTyped[annotations.transientField]

        ctx.JsonSchemaConfig.register(new ctx.JsonSchemaConfig {
          def libraryName: String = "jsoniter-scala"
          def configType: UntypedType = UntypedType.fromTyped[JsoniterConfig]

          def resolveFieldName(param: Parameter, scalaName: String): Expr[String] = {
            val customName: Option[String] = param.asUntyped.annotations
              .asInstanceOf[List[QTerm]]
              .find(t => t.tpe.asInstanceOf[UntypedType] =:= fieldNameTpe)
              .flatMap {
                case QApply(_, List(QLiteral(QStringConstant(value)))) => Some(value)
                case _                                                 => None
              }
            customName match {
              case Some(name) => Expr(name)
              case None       =>
                Expr.quote {
                  Expr.splice(configExpr).fieldNameMapper(Expr.splice(Expr(scalaName)))
                }
            }
          }

          def isTransientField(param: Parameter): Boolean =
            param.asUntyped.annotations
              .asInstanceOf[List[QTerm]]
              .exists(t => t.tpe.asInstanceOf[UntypedType] =:= transientFieldTpe)

          def resolveConstructorName(scalaName: String): Expr[String] =
            Expr.quote {
              Expr.splice(configExpr).adtLeafClassNameMapper(Expr.splice(Expr(scalaName)))
            }

          def discriminatorFieldName: Expr[Option[String]] =
            Expr.quote(Expr.splice(configExpr).discriminatorFieldName)

          def enumAsStrings: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).enumAsStrings)

          def useDefaults: Expr[Boolean] =
            Expr.quote(false)
        })
      case None => () // no JsoniterConfig in scope
    }
  }
}
