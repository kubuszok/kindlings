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
    implicit val NamedT: Type[annotations.named] = Type.of[annotations.named]

    def stringArg[Ann](anns: List[Expr[Ann]]): Option[String] =
      anns.headOption
        .flatMap(ann => Annotations.decodedConstructorArguments(ann))
        .flatMap(_.headOption.flatMap(_.toOption))
        .collect { case s: String => s }

    // As jsoniter-derivation's CodecMacrosImpl.isJavaEnumType: also Scala 3 enums extending java.lang.Enum
    def isJavaEnum[A: Type]: Boolean = Type[A].isJavaEnum || Type[A] <:< Type.of[java.lang.Enum[?]]

    // As jsoniter-derivation's CodecMacrosImpl.adtLeafFullName: the fully-qualified name of an ADT leaf, passed to
    // adtLeafClassNameMapper (`pkg.Outer.Leaf`, without `$`/`.type` suffixes and type arguments).
    def adtLeafFullName[A: Type, Child: Type](childName: String): String = {
      def clean(printed: String): String = {
        val noArgs = printed.indexOf('[') match {
          case -1 => printed
          case i  => printed.substring(0, i)
        }
        noArgs.stripSuffix(".type").stripSuffix("$")
      }
      val printed = clean(Type.plainPrint[Child])
      if (printed == childName || printed.endsWith("." + childName)) printed
      else clean(Type.plainPrint[A]) + "." + childName
    }

    Expr.summonImplicit[JsoniterConfig].toOption match {
      case Some(configExpr) =>
        ctx.JsonSchemaConfig.register(new ctx.JsonSchemaConfig {
          def libraryName: String = "jsoniter-scala"
          def configClassName: String = "hearth.kindlings.jsoniterderivation.JsoniterConfig"
          def configType: UntypedType = UntypedType.fromTyped[JsoniterConfig]

          def resolveFieldName(param: Parameter, scalaName: String): Expr[String] = {
            // Native Hearth annotation API (Hearth issue #283): read the @fieldName(...) String literal, or (as
            // jsoniter-scala, whose only field-rename annotation it is) @named(...).
            val customName: Option[String] =
              stringArg(param.annotationsOfType[annotations.fieldName])
                .orElse(stringArg(param.annotationsOfType[annotations.named]))
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

          // Case objects of all-case-object ADTs are JSON strings with enumAsStrings, and without a discriminator
          // unless circeLikeObjectEncoding (as in jsoniter-scala).
          def enumAsStrings: Expr[Boolean] =
            Expr.quote {
              val c = Expr.splice(configExpr)
              c.enumAsStrings || (c.discriminatorFieldName.isEmpty && !c.circeLikeObjectEncoding)
            }

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

          // As jsoniter-derivation's CodecMacrosImpl.enumLeafNameExpr: Java enum values use javaEnumValueNameMapper,
          // scala.Enumeration values their toString, other leaves @named or adtLeafClassNameMapper of their
          // fully-qualified names.
          override def resolveSubtypeName[Parent: Type, Child: Type](scalaName: String): Expr[String] =
            if (isJavaEnum[Parent])
              Expr.quote(Expr.splice(configExpr).javaEnumValueNameMapper(Expr.splice(mkStringExpr(scalaName))))
            else if (Type[Parent].isEnumeration)
              SingletonValue.unapply(Type[Child]) match {
                case Some(sv) => Expr.quote(Expr.splice(sv.singletonExpr).toString)
                case None     => mkStringExpr(scalaName)
              }
            else
              stringArg(Type[Child].annotationsOfType[annotations.named]) match {
                case Some(name) => mkStringExpr(name)
                case None       =>
                  val fullName = mkStringExpr(adtLeafFullName[Parent, Child](scalaName))
                  Expr.quote(Expr.splice(configExpr).adtLeafClassNameMapper(Expr.splice(fullName)))
              }

          override def flattensNestedSubtypes: Boolean = true

          // Java enum and Enumeration values are always strings (or Enumeration ids with useScalaEnumValueId)
          override def valueEnumsAsStrings: Expr[Boolean] = Expr.quote(true)

          override def enumerationValuesAsIds: Expr[Boolean] =
            Expr.quote(Expr.splice(configExpr).useScalaEnumValueId)

          // Case objects are `"Leaf"` without a discriminator (`{"Leaf":{}}` with circeLikeObjectEncoding)
          override def singletonSubtypesAsStrings: Expr[Boolean] =
            Expr.quote {
              val c = Expr.splice(configExpr)
              c.discriminatorFieldName.isEmpty && !c.circeLikeObjectEncoding
            }

          // Without a discriminator, leaves are `{"Leaf":{...}}`
          override def wrapsSubtypesWithoutDiscriminator: Expr[Boolean] = Expr.quote(true)

          override def tuplesAsArrays: Boolean = true

          // See JsoniterDerivationUtils.writeNestedOptionAsAdt
          override def nestedOptionsAsAdts: Option[NestedOptionEncoding] = Some(
            new NestedOptionEncoding(
              enabled = Expr.quote(!Expr.splice(configExpr).skipNestedOptionValues),
              someName = Expr.quote(Expr.splice(configExpr).adtLeafClassNameMapper("scala.Some")),
              noneName = Expr.quote(Expr.splice(configExpr).adtLeafClassNameMapper("scala.None")),
              valueFieldName = Expr.quote(Expr.splice(configExpr).fieldNameMapper("value"))
            )
          )
        })
      case None => () // no JsoniterConfig in scope
    }
  }
}
