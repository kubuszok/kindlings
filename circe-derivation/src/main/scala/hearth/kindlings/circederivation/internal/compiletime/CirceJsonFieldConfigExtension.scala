package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommons
import hearth.std.StdExtensions
import hearth.kindlings.jsonschemaconfigs.{JsonSchemaConfigExtension, JsonSchemaConfigs}

final class CirceJsonFieldConfigExtension extends JsonSchemaConfigExtension {

  override protected def extendJsonConfig(ctx: MacroCommons & StdExtensions & JsonSchemaConfigs): Unit = {
    import ctx.*

    // Pre-resolve ExprCodec[String] with explicit companion access to avoid
    // diverging implicit expansion caused by import ctx.* in Scala 2. Harmless on
    // Scala 3, so this platform-agnostic file uses it on both platforms.
    val stringCodec = ExprCodec.StringExprCodec
    def mkStringExpr(s: String): Expr[String] = Expr.apply(s)(stringCodec)

    // Self-referential implicit val Type.of definitions now work in cross-quotes (Hearth issue #285).
    implicit val ConfigT: Type[Configuration] = Type.of[Configuration]
    implicit val FieldNameT: Type[annotations.fieldName] = Type.of[annotations.fieldName]
    implicit val TransientFieldT: Type[annotations.transientField] = Type.of[annotations.transientField]

    val EncoderCtor: Type.Ctor1[io.circe.Encoder] = Type.Ctor1.of[io.circe.Encoder]
    // The same implicits as the encoder derivation ignores (Kindlings' and circe's own auto-derivation).
    lazy val ignoredEncoderImplicits: Seq[UntypedMethod] =
      Type.of[KindlingsEncoder.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[KindlingsCodecAsObject.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      } ++ Type.of[io.circe.Encoder.type].unsortedMethods.collect {
        case method if method.name == "derived" || method.name.startsWith("encodeLiteral") => method.asUntyped
      }

    Expr.summonImplicit[Configuration].toOption match {
      case Some(configExpr) =>
        ctx.JsonSchemaConfig.register(new ctx.JsonSchemaConfig {
          def libraryName: String = "circe"
          def configClassName: String = "hearth.kindlings.circederivation.Configuration"
          def configType: UntypedType = UntypedType.fromTyped[Configuration]

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
                  Expr.splice(configExpr).transformMemberNames(Expr.splice(mkStringExpr(scalaName)))
                }
            }
          }

          def isTransientField(param: Parameter): Boolean =
            param.hasAnnotationOfType[annotations.transientField]

          def resolveConstructorName(scalaName: String): Expr[String] =
            Expr.quote {
              Expr.splice(configExpr).transformConstructorNames(Expr.splice(mkStringExpr(scalaName)))
            }

          def discriminatorFieldName: Expr[Option[String]] =
            Expr.quote(Expr.splice(configExpr).discriminator)

          def enumAsStrings: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).enumAsStrings)

          def useDefaults: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).useDefaults)

          def fieldsWithDefaultsAreOptional: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).useDefaults)

          def emptyFieldsAreOptional: Expr[Boolean] =
            Expr.quote(false)

          def mapsAreArrays: Expr[Boolean] =
            Expr.quote(false)

          def numericFieldsAsStrings: Expr[Boolean] =
            Expr.quote(false)

          // A child that does not encode to a JSON object is written as `{"Name": value}` with a discriminator.
          override def wrapsNonObjectSubtypesWithDiscriminator: Boolean = true

          // Nested sealed hierarchies are flattened (like circe's ConfiguredEncoder), except for an intermediate with a
          // user-provided Encoder, which writes it (as is, when it is a Kindlings-derived sum encoder).
          override def flattensNestedSubtypes: Boolean = true

          override def nestedSubtypeInstance(subtype: ??): Option[Expr[Boolean]] = {
            import subtype.Underlying as Sub
            implicit val EncoderSub: Type[io.circe.Encoder[Sub]] = EncoderCtor[Sub]
            Type[io.circe.Encoder[Sub]].summonExprIgnoring(ignoredEncoderImplicits*).toOption.map { instance =>
              Expr.quote(
                hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
                  .isKindlingsSumEncoder(Expr.splice(instance))
              )
            }
          }

          // Without a discriminator every subtype is written as `{"Name": value}`.
          override def wrapsSubtypesWithoutDiscriminator: Expr[Boolean] = Expr.quote(true)

          override def unwrapsValueClasses: Option[Expr[Boolean]] =
            Some(Expr.quote(Expr.splice(configExpr).unwrapValueClasses))
        })
      case None => () // no circe Configuration in scope
    }
  }
}
