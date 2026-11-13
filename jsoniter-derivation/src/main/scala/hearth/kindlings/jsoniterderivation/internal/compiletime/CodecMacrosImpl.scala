package hearth.kindlings.jsoniterderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import hearth.kindlings.jsoniterderivation.{annotations, JsoniterConfig, KindlingsJsonCodec, KindlingsJsonValueCodec}
import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonCodec,
  JsonKeyCodec,
  JsonReader,
  JsonReaderException,
  JsonValueCodec,
  JsonWriter
}

trait CodecMacrosImpl
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with hearth.kindlings.derivation.compiletime.MethodFolds
    with JsoniterDerivationPolicy
    with ProductTypes
    with rules.EncoderUseCachedDefWhenAvailableRuleImpl
    with rules.EncoderUseImplicitWhenAvailableRuleImpl
    with rules.EncoderHandleAsLiteralTypeRuleImpl
    with rules.EncoderHandleAsBuiltInRuleImpl
    with rules.EncoderHandleAsValueTypeRuleImpl
    with rules.EncoderHandleAsOptionRuleImpl
    with rules.EncoderHandleAsMapRuleImpl
    with rules.EncoderHandleAsCollectionRuleImpl
    with rules.EncoderHandleAsTupleRuleImpl
    with rules.EncoderHandleAsNamedTupleRuleImpl
    with rules.EncoderHandleAsOneValueClassRuleImpl
    with rules.EncoderHandleAsSingletonRuleImpl
    with rules.EncoderHandleAsCaseClassRuleImpl
    with rules.EncoderHandleAsEnumRuleImpl
    with rules.DecoderUseCachedDefWhenAvailableRuleImpl
    with rules.DecoderUseImplicitWhenAvailableRuleImpl
    with rules.DecoderHandleAsLiteralTypeRuleImpl
    with rules.DecoderHandleAsBuiltInRuleImpl
    with rules.DecoderHandleAsValueTypeRuleImpl
    with rules.DecoderHandleAsOptionRuleImpl
    with rules.DecoderHandleAsMapRuleImpl
    with rules.DecoderHandleAsCollectionRuleImpl
    with rules.DecoderHandleAsTupleRuleImpl
    with rules.DecoderHandleAsNamedTupleRuleImpl
    with rules.DecoderHandleAsOneValueClassRuleImpl
    with rules.DecoderHandleAsSingletonRuleImpl
    with rules.DecoderHandleAsCaseClassRuleImpl
    with rules.DecoderHandleAsEnumRuleImpl
    with rules.DecoderAbsentFieldSupport
    with rules.EncoderDerivationPolicyRuleImpl
    with rules.DecoderDerivationPolicyRuleImpl { this: MacroCommons & StdExtensions & AnnotationSupport =>

  // $COVERAGE-OFF$
  override protected def derivationPolicyTypeClassName: String = "KindlingsJsonValueCodec"
  // $COVERAGE-ON$

  override protected def derivationSettingsNamespace: String = "jsoniterDerivation"

  /** Evaluates the config at compile time when possible; `Some` enables the optimized codegen (no runtime config
    * checks). Overridden on Scala 3 to also follow immutable local `val`s (including the proxies Scala 3 creates for
    * non-inline arguments of inline methods, like `derived[A](using JsoniterConfig.default.withX)`).
    */
  protected def semiEvalConfig(configExpr: Expr[JsoniterConfig]): Option[JsoniterConfig] =
    configExpr.semiEval.toOption

  /** Element types of a tuple type (`TupleN`, and on Scala 3 also generic tuples: `EmptyTuple`, `H *: T`, match types
    * reducing to them, and tuples with more than 22 elements), or `None` for other types. Named tuples are not tuples
    * here (they are JSON objects).
    */
  protected def tupleElementTypes[A: Type]: Option[List[??]] =
    if (!Type[A].isTuple || Type[A].isNamedTuple) None
    else CaseClass.parse[A].toOption.map(_.primaryConstructor.totalParameters.flatten.toList.map(_._2.tpe))

  /** For a Scala 3 union type with `Null` (`T | Null`, jsoniter-scala's "nullable" types): `T`. Always `None` on Scala 2.
    * Such values are `null` or a `T`; as case class fields they are optional (absent is `null`) and `transientNull`
    * skips them when `null`.
    */
  protected def nullableUnionUnderlying[A: Type]: Option[??] = None

  /** Builds a tuple which is not a `TupleN` case class (Scala 3 generic tuples) from its elements. */
  protected def constructGenericTuple[A: Type](elements: List[Expr_??]): Option[Expr[A]] = {
    val _ = elements
    None
  }

  /** The default value of a constructor parameter. A default of a non-first parameter list may depend on the preceding
    * parameters: it is only available when `argumentValues` provides all of them (as in jsoniter-scala, which computes
    * such defaults from the decoded/encoded values).
    */
  private[compiletime] def parameterDefault(param: Parameter, argumentValues: Map[String, Expr_??]): Option[Expr_??] =
    if (!param.hasDefault) None
    else
      param.defaultValue.flatMap { method =>
        // untyped: the value parameters of a generic default method are only typed once its type arguments are applied
        val needed = method.asUntyped.parameters.flatMap(_.keys.toList)
        if (!needed.forall(argumentValues.contains)) None
        else
          foldInstanceFree(method, "Default value")(
            onTypes = _ => Map.empty,
            onValues = _ => argumentValues.filter { case (name, _) => needed.contains(name) }
          ).toOption
      }

  /** `inlineOneValueClasses`: a class with exactly one non-transient field (not a value class, those are handled by the
    * value type rules), with that field's name and parameter.
    */
  private[compiletime] def oneValueClassField[A: Type](
      evaluatedConfig: Option[JsoniterConfig]
  ): Option[(ProductType[A], String, Parameter)] =
    if (IsValueType.unapply(Type[A]).isDefined || Type[A].isTuple) None
    else
      ProductType.parse[A].toOption.flatMap { product =>
        product.params.filterNot { case (_, p) => isTransientParam[A](p, evaluatedConfig) } match {
          case (name, param) :: Nil => Some((product, name, param))
          case _                    => None
        }
      }

  /** Constructs a one-value class (see [[oneValueClassField]]) from its field's value and the defaults of its transient
    * parameters.
    */
  private[compiletime] def constructOneValueClass[A: Type](
      product: ProductType[A],
      fieldName: String,
      value: Expr_??
  ): Either[String, Expr[A]] = {
    val others = product.params.filterNot(_._1 == fieldName).foldLeft(Map(fieldName -> value)) {
      case (acc, (name, param)) => parameterDefault(param, acc).fold(acc)(d => acc + (name -> d))
    }
    foldInstanceFree(product.primaryConstructor, "Constructor")(onTypes = _ => Map.empty, onValues = _ => others)
      .map(_.value.asInstanceOf[Expr[A]])
  }

  /** `@transientField`, or `scala.transient` with `scalaTransientSupport` (only known with a compile-time config). */
  private[compiletime] def isTransientParam[A: Type](
      param: Parameter,
      evaluatedConfig: Option[JsoniterConfig]
  ): Boolean = {
    implicit val TransientFieldT: Type[annotations.transientField] = CTypes.TransientField
    hasAnnotationType[annotations.transientField](param) ||
    (evaluatedConfig.exists(_.scalaTransientSupport) && hasScalaTransient[A](param))
  }

  /** `scala.transient` on a parameter / its field. */
  private[compiletime] def hasScalaTransient[A: Type](param: Parameter): Boolean = {
    implicit val ScalaTransientT: Type[scala.transient] = CTypes.ScalaTransient
    // `scala.transient` targets fields (`@field`), so it is found on the case field rather than on the parameter.
    def onCaseField: Boolean = CaseClass.parse[A].toOption.exists { cc =>
      cc.caseFields.exists(f => f.name.trim == param.name && f.hasAnnotationOfType[scala.transient])
    } || Type[A].unsortedMethodsNamed(param.name).exists(_.hasAnnotationOfType[scala.transient])
    hasAnnotationType[scala.transient](param) || onCaseField
  }

  /** A `scala.transient` (and not `@transientField`) parameter with a config known only at runtime: transient only when
    * `config.scalaTransientSupport` is on, so the generic codegen checks the flag (skip on write, default on read).
    */
  private[compiletime] def isTransientAtRuntimeOnly[A: Type](
      param: Parameter,
      evaluatedConfig: Option[JsoniterConfig]
  ): Boolean = {
    implicit val TransientFieldT: Type[annotations.transientField] = CTypes.TransientField
    evaluatedConfig.isEmpty && !hasAnnotationType[annotations.transientField](param) && hasScalaTransient[A](param)
  }

  /** A literal (singleton) type's JSON key and value, e.g. `"2"` and `2` for the type `2` (jsoniter-scala writes
    * literal keys as their `toString`).
    */
  @scala.annotation.nowarn("msg=is never used")
  private[compiletime] def literalKey[K: Type]: Option[(String, Expr[K])] = {
    def of[U](codec: TypeCodec[U])(implicit ec: ExprCodec[U], ut: Type[U]): Option[(String, Expr[K])] =
      codec.fromType(Type[K]).map { e =>
        val v: U = e.value
        (v.toString, Expr.quote(Expr.splice(Expr(v)).asInstanceOf[K]))
      }
    implicit val StringT: Type[String] = CTypes.String
    implicit val IntT: Type[Int] = CTypes.Int
    implicit val LongT: Type[Long] = CTypes.Long
    implicit val DoubleT: Type[Double] = CTypes.Double
    implicit val FloatT: Type[Float] = CTypes.Float
    implicit val ShortT: Type[Short] = CTypes.Short
    implicit val ByteT: Type[Byte] = CTypes.Byte
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    implicit val CharT: Type[Char] = CTypes.Char
    of(Type.StringCodec)
      .orElse(of(Type.CharCodec))
      .orElse(of(Type.IntCodec))
      .orElse(of(Type.LongCodec))
      .orElse(of(Type.BooleanCodec))
      .orElse(of(Type.DoubleCodec))
      .orElse(of(Type.FloatCodec))
      .orElse(of(Type.ShortCodec))
      .orElse(of(Type.ByteCodec))
  }

  /** Java enums, including Scala 3 enums extending `java.lang.Enum` (which Hearth's `isJavaEnum` does not report). */
  private[compiletime] def isJavaEnumType[A: Type]: Boolean =
    Type[A].isJavaEnum || Type[A] <:< Type.of[java.lang.Enum[?]]

  /** The JSON name of an enum case / ADT leaf `Child` of `A`, as in jsoniter-scala: Java enum values use
    * `javaEnumValueNameMapper` (on the constant's name), `scala.Enumeration` values their `toString` (`Value("A")` is
    * `"A"`), and other ADT leaves `adtLeafClassNameMapper`.
    */
  @scala.annotation.nowarn("msg=is never used")
  private[compiletime] def enumLeafNameExpr[A: Type, Child: Type](
      childName: String,
      config: Expr[JsoniterConfig]
  ): Expr[String] = {
    implicit val StringT: Type[String] = CTypes.String
    if (isJavaEnumType[A]) Expr.quote(Expr.splice(config).javaEnumValueNameMapper(Expr.splice(Expr(childName))))
    else if (Type[A].isEnumeration)
      SingletonValue.unapply(Type[Child]) match {
        case Some(sv) => Expr.quote(Expr.splice(sv.singletonExpr).toString)
        case None     => Expr(childName)
      }
    else
      namedAnnotationOf[Child] match {
        case Some(name) => Expr(name)
        case None       =>
          Expr.quote(
            Expr.splice(config).adtLeafClassNameMapper(Expr.splice(Expr(adtLeafFullName[A, Child](childName))))
          )
      }
  }

  /** The fully-qualified name of ADT leaf `Child` of `A` passed to `adtLeafClassNameMapper`, as in jsoniter-scala
    * (`pkg.Outer.Leaf`, without `$`/`.type` suffixes and type arguments; Scala 3 enum cases as `pkg.Enum.Case`).
    */
  private[compiletime] def adtLeafFullName[A: Type, Child: Type](childName: String): String = {
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

  /** The leaves of an ADT / enum (nested sealed hierarchies flattened), and as in jsoniter-scala the type itself (last,
    * so that its subtypes match first) when it is a non-abstract sealed class.
    */
  private[compiletime] def adtLeaves[A: Type](enumm: Enum[A]): List[(String, ??<:[A])] = {
    val children: List[(String, ??<:[A])] = enumm.exhaustiveChildren match {
      case Some(ec) => ec.toList
      case None     => enumm.directChildren.toList
    }
    val isBaseALeaf = !Type[A].isAbstract && !Type[A].isTrait && !Type[A].isEnumeration && !isJavaEnumType[A] &&
      !children.exists(_._2.Underlying =:= Type[A]) && ProductType.asLeaf[A].isDefined
    if (isBaseALeaf) children :+ (Type[A].shortName -> Type[A].as_??<:[A]) else children
  }

  /** The JSON key override of a field: `@fieldName`, or (as jsoniter-scala, whose only field-rename annotation it is)
    * `@named`.
    */
  private[compiletime] def fieldNameOverride(param: Parameter): Option[String] = {
    implicit val FieldNameT: Type[annotations.fieldName] = CTypes.FieldName
    implicit val NamedT: Type[annotations.named] = CTypes.Named
    getAnnotationStringArg[annotations.fieldName](param).orElse(getAnnotationStringArg[annotations.named](param))
  }

  /** The `@named` override of an ADT leaf's name, if any. */
  private[compiletime] def namedAnnotationOf[Child: Type]: Option[String] = {
    implicit val NamedT: Type[annotations.named] = CTypes.Named
    getTypeAnnotationStringArg[annotations.named, Child]
  }

  /** Whether `A` extends a sealed trait/class, i.e. is an ADT leaf (`alwaysEmitDiscriminator` applies to it). */
  private[compiletime] def hasSealedParent[A: Type]: Boolean =
    Type[A].baseClasses.exists { parent =>
      import parent.Underlying as P
      !(Type[P] =:= Type[A]) && Type[P].isSealed
    }

  /** The (unmapped, fully-qualified) name of an ADT leaf, as passed to `adtLeafClassNameMapper`. */
  private[compiletime] def adtLeafName[A: Type]: String =
    adtLeafFullName[A, A](Type[A].shortName.stripSuffix(".type").stripSuffix("$"))

  /** The discriminator key that the codec of ADT leaf `A` writes and accepts when `alwaysEmitDiscriminator` is on, or a
    * `null` expression when it does not apply.
    */
  @scala.annotation.nowarn("msg=is never used")
  private[compiletime] def alwaysEmittedDiscriminatorKey[A: Type](
      config: Expr[JsoniterConfig],
      evaluatedConfig: Option[JsoniterConfig]
  ): Option[Expr[String]] = {
    implicit val StringT: Type[String] = CTypes.String
    if (!hasSealedParent[A]) None
    else
      evaluatedConfig match {
        case Some(c) =>
          if (c.alwaysEmitDiscriminator) c.discriminatorFieldName.map(name => Expr(name)) else None
        case None =>
          Some(Expr.quote {
            val c = Expr.splice(config)
            if (c.alwaysEmitDiscriminator) c.discriminatorFieldName.orNull else null
          })
      }
  }

  // Shared type representations — centralized here so all rules access them consistently.
  private[compiletime] object CTypes {

    def JsonCodec: Type.Ctor1[JsonCodec] = Type.Ctor1.of[JsonCodec]
    def JsonKeyCodec: Type.Ctor1[JsonKeyCodec] = Type.Ctor1.of[JsonKeyCodec]
    def JsonValueCodec: Type.Ctor1[JsonValueCodec] = Type.Ctor1.of[JsonValueCodec]
    def KindlingsJsonCodec: Type.Ctor1[KindlingsJsonCodec] = Type.Ctor1.of[KindlingsJsonCodec]
    def KindlingsJsonValueCodec: Type.Ctor1[KindlingsJsonValueCodec] =
      Type.Ctor1.of[KindlingsJsonValueCodec]
    val CodecLogDerivation: Type[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation] =
      Type.of[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation]
    val JsoniterConfig: Type.Lazy[JsoniterConfig] = Type.Lazy(Type.of[JsoniterConfig])
    val JsonReader: Type.Lazy[JsonReader] = Type.Lazy(Type.of[JsonReader])
    val JsonWriter: Type.Lazy[JsonWriter] = Type.Lazy(Type.of[JsonWriter])
    val String: Type.Lazy[String] = Type.Lazy(Type.of[String])
    val Unit: Type.Lazy[Unit] = Type.Lazy(Type.of[Unit])
    val Any: Type.Lazy[Any] = Type.Lazy(Type.of[Any])
    val ArrayAny: Type.Lazy[Array[Any]] = Type.Lazy(Type.of[Array[Any]])
    val ListString: Type.Lazy[List[String]] = Type.Lazy(Type.of[List[String]])
    val JsonReaderException: Type.Lazy[JsonReaderException] = Type.Lazy(Type.of[JsonReaderException])
    def EitherJsonReaderException[A: Type]: Type[Either[JsonReaderException, A]] =
      Type.of[Either[JsonReaderException, A]]
    val FieldName: Type.Lazy[annotations.fieldName] = Type.Lazy(Type.of[annotations.fieldName])
    val TransientField: Type.Lazy[annotations.transientField] = Type.Lazy(Type.of[annotations.transientField])
    val Stringified: Type.Lazy[annotations.stringified] = Type.Lazy(Type.of[annotations.stringified])
    val Named: Type.Lazy[annotations.named] = Type.Lazy(Type.of[annotations.named])
    val ScalaTransient: Type.Lazy[scala.transient] = Type.Lazy(Type.of[scala.transient])
    val Int: Type.Lazy[Int] = Type.Lazy(Type.of[Int])
    val Long: Type.Lazy[Long] = Type.Lazy(Type.of[Long])
    val Double: Type.Lazy[Double] = Type.Lazy(Type.of[Double])
    val Float: Type.Lazy[Float] = Type.Lazy(Type.of[Float])
    val Short: Type.Lazy[Short] = Type.Lazy(Type.of[Short])
    val Byte: Type.Lazy[Byte] = Type.Lazy(Type.of[Byte])
    val Boolean: Type.Lazy[Boolean] = Type.Lazy(Type.of[Boolean])
    val Char: Type.Lazy[Char] = Type.Lazy(Type.of[Char])
    val BigDecimal: Type.Lazy[BigDecimal] = Type.Lazy(Type.of[BigDecimal])
    val BigInt: Type.Lazy[BigInt] = Type.Lazy(Type.of[BigInt])
    val Product: Type.Lazy[Product] = Type.Lazy(Type.of[Product])
    val Instant: Type.Lazy[java.time.Instant] = Type.Lazy(Type.of[java.time.Instant])
    val LocalDate: Type.Lazy[java.time.LocalDate] = Type.Lazy(Type.of[java.time.LocalDate])
    val LocalTime: Type.Lazy[java.time.LocalTime] = Type.Lazy(Type.of[java.time.LocalTime])
    val LocalDateTime: Type.Lazy[java.time.LocalDateTime] = Type.Lazy(Type.of[java.time.LocalDateTime])
    val OffsetDateTime: Type.Lazy[java.time.OffsetDateTime] = Type.Lazy(Type.of[java.time.OffsetDateTime])
    val ZonedDateTime: Type.Lazy[java.time.ZonedDateTime] = Type.Lazy(Type.of[java.time.ZonedDateTime])
    val Duration: Type.Lazy[java.time.Duration] = Type.Lazy(Type.of[java.time.Duration])
    val MonthDay: Type.Lazy[java.time.MonthDay] = Type.Lazy(Type.of[java.time.MonthDay])
    val OffsetTime: Type.Lazy[java.time.OffsetTime] = Type.Lazy(Type.of[java.time.OffsetTime])
    val Period: Type.Lazy[java.time.Period] = Type.Lazy(Type.of[java.time.Period])
    val Year: Type.Lazy[java.time.Year] = Type.Lazy(Type.of[java.time.Year])
    val YearMonth: Type.Lazy[java.time.YearMonth] = Type.Lazy(Type.of[java.time.YearMonth])
    val ZoneId: Type.Lazy[java.time.ZoneId] = Type.Lazy(Type.of[java.time.ZoneId])
    val ZoneOffset: Type.Lazy[java.time.ZoneOffset] = Type.Lazy(Type.of[java.time.ZoneOffset])
    val UUID: Type.Lazy[java.util.UUID] = Type.Lazy(Type.of[java.util.UUID])
    // Java boxed primitives
    val JavaByte: Type.Lazy[java.lang.Byte] = Type.Lazy(Type.of[java.lang.Byte])
    val JavaShort: Type.Lazy[java.lang.Short] = Type.Lazy(Type.of[java.lang.Short])
    val JavaInteger: Type.Lazy[java.lang.Integer] = Type.Lazy(Type.of[java.lang.Integer])
    val JavaLong: Type.Lazy[java.lang.Long] = Type.Lazy(Type.of[java.lang.Long])
    val JavaFloat: Type.Lazy[java.lang.Float] = Type.Lazy(Type.of[java.lang.Float])
    val JavaDouble: Type.Lazy[java.lang.Double] = Type.Lazy(Type.of[java.lang.Double])
    val JavaBoolean: Type.Lazy[java.lang.Boolean] = Type.Lazy(Type.of[java.lang.Boolean])
    val JavaCharacter: Type.Lazy[java.lang.Character] = Type.Lazy(Type.of[java.lang.Character])
    // BitSet types
    val ImmutableBitSet: Type.Lazy[scala.collection.immutable.BitSet] =
      Type.Lazy(Type.of[scala.collection.immutable.BitSet])
    val MutableBitSet: Type.Lazy[scala.collection.mutable.BitSet] = Type.Lazy(Type.of[scala.collection.mutable.BitSet])
    val CollectionBitSet: Type.Lazy[scala.collection.BitSet] = Type.Lazy(Type.of[scala.collection.BitSet])
  }

  // Entrypoints

  /** Derive a combined JsonValueCodec for type A.
    *
    * To avoid Scala 3 cross-splice staging issues, all derivation (encode, decode, nullValue) is performed in a single
    * MIO.scoped/runSafe call using LambdaBuilder. The resulting function expressions are then spliced into the final
    * Expr.quote without creating new expressions inside sibling splices.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[KindlingsJsonValueCodec[A]] = {
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val KindlingsCodecA: Type[KindlingsJsonValueCodec[A]] = CTypes.KindlingsJsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    deriveCodecFromCtxAndAdaptForEntrypoint[A, KindlingsJsonValueCodec[A]](
      "KindlingsJsonValueCodec.derived",
      configExpr
    ) { case (encodeFn, decodeFn, nullValueExpr) =>
      Expr.quote {
        hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationFactories.codecInstance[A](
          Expr.splice(nullValueExpr),
          (in: JsonReader, default: A) => {
            val _ = default
            if (Expr.splice(configExpr).encodingOnly)
              throw new NotImplementedError() // as jsoniter-scala (`???`): "an implementation is missing"
            // as jsoniter-scala: a JSON `null` is the `default` argument (when it is not `null`)
            JsoniterDerivationUtils.readNullAsDefaultArgument[A](in, default, Expr.splice(nullAsDefaultApplies[A]))(r =>
              Expr.splice(decodeFn(Expr.quote(r), configExpr))
            )
          },
          (x: A, out: JsonWriter) =>
            if (Expr.splice(configExpr).decodingOnly)
              throw new NotImplementedError() // as jsoniter-scala (`???`): "an implementation is missing"
            else
              Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
        )
      }
    }
  }

  // Inline encode/decode entrypoints

  /** Derive an inline writeToString expression for type A.
    *
    * Checks for a user-provided implicit JsonValueCodec[A] first. If found, uses it directly. Otherwise derives only
    * the encoder and creates a stub codec.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineWriteToString[A: Type](valueExpr: Expr[A], configExpr: Expr[JsoniterConfig]): Expr[String] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonValueCodec.writeToString: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonValueCodec.writeToString[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    summonJsonValueCodecCached[A] match {
      case Right(codecExpr) =>
        Expr.quote {
          writeToString[A](Expr.splice(valueExpr))(Expr.splice(codecExpr))
        }
      case Left(_) =>
        deriveEncoderOnlyFromCtxAndAdaptForEntrypoint[A, String]("KindlingsJsonValueCodec.writeToString", configExpr) {
          encodeFn =>
            Expr.quote {
              writeToString[A](Expr.splice(valueExpr))(new JsonValueCodec[A] {
                def nullValue: A = null.asInstanceOf[A]
                def decodeValue(in: JsonReader, default: A): A =
                  throw new UnsupportedOperationException("encode-only codec")
                def encodeValue(x: A, out: JsonWriter): Unit =
                  Expr.splice(encodeFn(Expr.quote(x), Expr.quote(out), configExpr))
              })
            }
        }
    }
  }

  /** Derive an inline readFromString expression for type A.
    *
    * Checks for a user-provided implicit JsonValueCodec[A] first. If found, uses it directly. Otherwise derives only
    * the decoder and nullValue, creating a stub codec.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveInlineReadFromString[A: Type](
      jsonExpr: Expr[String],
      configExpr: Expr[JsoniterConfig]
  ): Expr[Either[JsonReaderException, A]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val CodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val EitherT: Type[Either[JsonReaderException, A]] = CTypes.EitherJsonReaderException[A]
    implicit val JsonReaderExceptionT: Type[JsonReaderException] = CTypes.JsonReaderException

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonValueCodec.readFromString: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonValueCodec.readFromString[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    summonJsonValueCodecCached[A] match {
      case Right(codecExpr) =>
        Expr.quote {
          try Right(readFromString[A](Expr.splice(jsonExpr))(Expr.splice(codecExpr)))
          catch { case e: JsonReaderException => Left(e) }
        }
      case Left(_) =>
        deriveDecoderOnlyFromCtxAndAdaptForEntrypoint[A, Either[JsonReaderException, A]](
          "KindlingsJsonValueCodec.readFromString",
          configExpr
        ) { case (decodeFn, nullValueExpr) =>
          Expr.quote {
            try
              Right(readFromString[A](Expr.splice(jsonExpr))(new JsonValueCodec[A] {
                def nullValue: A = Expr.splice(nullValueExpr)
                def decodeValue(in: JsonReader, default: A): A = {
                  val _ = default
                  Expr.splice(decodeFn(Expr.quote(in), configExpr))
                }
                def encodeValue(x: A, out: JsonWriter): Unit =
                  throw new UnsupportedOperationException("decode-only codec")
              }))
            catch { case e: JsonReaderException => Left(e) }
          }
        }
    }
  }

  /** Derive a standalone JsonKeyCodec for type A.
    *
    * Uses the existing deriveKeyEncoding/deriveKeyDecoding methods with minimal stub contexts. Only succeeds for types
    * with a natural key representation: built-in primitives, value types wrapping those, and sealed traits/enums of
    * case objects.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveKeyCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[JsonKeyCodec[A]] = {
    implicit val JsonKeyCodecA: Type[JsonKeyCodec[A]] = CTypes.JsonKeyCodec[A]
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonCodec.deriveKeyCodec: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonCodec.deriveKeyCodec[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving key codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None

          // Derive key encoding using a stub EncoderCtx
          val keyEncMIO: MIO[Option[Expr[(A, JsonWriter) => Unit]]] = {
            val stubValue = Expr.quote(null.asInstanceOf[A])
            val stubWriter = Expr.quote(null.asInstanceOf[JsonWriter])
            implicit val ctx: EncoderCtx[A] =
              EncoderCtx.from(stubValue, stubWriter, configExpr, cache, selfType, semiEvalConfig(configExpr))
            EncoderHandleAsMapRule.deriveKeyEncoding[A]
          }

          // Derive key decoding using a stub DecoderCtx
          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] =
              DecoderCtx.from(stubReader, configExpr, cache, selfType, semiEvalConfig(configExpr))
            DecoderHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (keyEncOpt, keyDecOpt) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- keyEncMIO.parTuple(keyDecMIO)
            } yield result
          }

          val (encFn, decFn) = (keyEncOpt, keyDecOpt) match {
            case (Some(enc), Some(dec)) => (enc, dec)
            case _                      =>
              Environment.reportErrorAndAbort(
                s"KindlingsJsonCodec.deriveKeyCodec: Cannot derive JsonKeyCodec for ${Type[A].prettyPrint}.\n" +
                  "Key codecs can only be derived for: primitive types (Int, Long, Double, Float, Short, Boolean, BigDecimal, BigInt),\n" +
                  "value types wrapping those primitives, and sealed traits/enums of case objects."
              )
          }

          val vals = runSafe(cache.get)
          val resultExpr = Expr.quote {
            new JsonKeyCodec[A] {
              def decodeKey(in: JsonReader): A = Expr.splice(decFn).apply(Expr.splice(Expr.quote(in)))
              def encodeKey(x: A, out: JsonWriter): Unit =
                Expr.splice(encFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
            }
          }
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final key codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsJsonCodec.deriveKeyCodec",
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  /** Derive a combined JsonCodec (value + key) for type A. Used only by Scala 2 macro bridge. Scala 3 avoids splice
    * isolation by calling separate inline methods.
    */
  @scala.annotation.nowarn("msg=is never used")
  def deriveJsonCodecTypeClass[A: Type](configExpr: Expr[JsoniterConfig]): Expr[KindlingsJsonCodec[A]] = {
    implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val JsonValueCodecA: Type[JsonValueCodec[A]] = CTypes.JsonValueCodec[A]
    implicit val JsonKeyCodecA: Type[JsonKeyCodec[A]] = CTypes.JsonKeyCodec[A]
    implicit val KindlingsJsonCodecA: Type[KindlingsJsonCodec[A]] = CTypes.KindlingsJsonCodec[A]

    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"KindlingsJsonCodec.derived: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          "Provide an explicit type parameter, e.g.: KindlingsJsonCodec.derived[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )

    Log
      .namedScope(
        s"Deriving combined json codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)
          val evConfig: Option[JsoniterConfig] = semiEvalConfig(configExpr)

          // Validate config for contradictory flag combinations at compile time
          validateConfigOrAbort(evConfig, "KindlingsJsonCodec.derived")

          // Value codec parts — same as deriveCodecTypeClass
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring json codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined json codec encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring json codec decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined json codec decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          // Key codec parts
          val keyEncMIO: MIO[Option[Expr[(A, JsonWriter) => Unit]]] = {
            val stubValue = Expr.quote(null.asInstanceOf[A])
            val stubWriter = Expr.quote(null.asInstanceOf[JsonWriter])
            implicit val ctx: EncoderCtx[A] =
              EncoderCtx.from(stubValue, stubWriter, configExpr, cache, selfType, evConfig)
            EncoderHandleAsMapRule.deriveKeyEncoding[A]
          }

          val keyDecMIO: MIO[Option[Expr[JsonReader => A]]] = {
            implicit val StringT: Type[String] = CTypes.String
            val stubReader = Expr.quote(null.asInstanceOf[JsonReader])
            implicit val ctx: DecoderCtx[A] = DecoderCtx.from(stubReader, configExpr, cache, selfType, evConfig)
            DecoderHandleAsMapRule.deriveKeyDecoding[A]
          }

          val (((encFn, decFn), nullVal), (keyEncOpt, keyDecOpt)) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO.parTuple(decMIO).parTuple(nullMIO).parTuple(keyEncMIO.parTuple(keyDecMIO))
            } yield result
          }

          val (keyEncFn, keyDecFn) = (keyEncOpt, keyDecOpt) match {
            case (Some(enc), Some(dec)) => (enc, dec)
            case _                      =>
              Environment.reportErrorAndAbort(
                s"KindlingsJsonCodec.derived: Cannot derive JsonKeyCodec for ${Type[A].prettyPrint}.\n" +
                  "Key codecs can only be derived for: primitive types (Int, Long, Double, Float, Short, Boolean, BigDecimal, BigInt),\n" +
                  "value types wrapping those primitives, and sealed traits/enums of case objects.\n" +
                  "If you only need value encoding/decoding, use KindlingsJsonValueCodec.derived instead."
              )
          }

          val vals = runSafe(cache.get)
          val validation: Expr[Unit] =
            if (evConfig.isDefined) Expr.quote(())
            else Expr.quote(JsoniterConfig.validate(Expr.splice(configExpr)))
          val resultExpr = Expr.quote {
            Expr.splice(validation)
            hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationFactories.jsonCodecInstance[A](
              Expr.splice(nullVal),
              (in: JsonReader, default: A) => {
                val _ = default
                if (Expr.splice(configExpr).encodingOnly)
                  throw new NotImplementedError() // as jsoniter-scala (`???`): "an implementation is missing"
                JsoniterDerivationUtils.readNullAsDefaultArgument[A](in, default, Expr.splice(nullAsDefaultApplies[A]))(
                  r => Expr.splice(decFn(Expr.quote(r), configExpr))
                )
              },
              (x: A, out: JsonWriter) =>
                if (Expr.splice(configExpr).decodingOnly)
                  throw new NotImplementedError() // as jsoniter-scala (`???`): "an implementation is missing"
                else Expr.splice(encFn(Expr.quote(x), Expr.quote(out), configExpr)),
              (in: JsonReader) => Expr.splice(keyDecFn).apply(Expr.splice(Expr.quote(in))),
              (x: A, out: JsonWriter) =>
                Expr.splice(keyEncFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
            )
          }
          vals.toValDefs.use(_ => resultExpr)
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final combined json codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        "KindlingsJsonCodec.derived",
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  // Handles logging, error reporting and prepending "cached" defs and vals to the result.

  def deriveEncoderOnlyFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[JsoniterConfig]
  )(
      provideCtxAndAdapt: (
          (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving encoder-only for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
        implicit val UnitT: Type[Unit] = CTypes.Unit

        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None
          val evConfig: Option[JsoniterConfig] = semiEvalConfig(configExpr)

          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring encoder-only encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined encoder-only encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          val encFn = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(encFn)
          vals.toValDefs.use(_ => validatedAtRuntime(evConfig, configExpr, resultExpr))
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final encoder-only result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def deriveDecoderOnlyFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[JsoniterConfig]
  )(
      provideCtxAndAdapt: (
          (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A],
          Expr[A]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving decoder-only for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = None
          val evConfig: Option[JsoniterConfig] = semiEvalConfig(configExpr)

          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring decoder-only decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined decoder-only decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          val (decFn, nullVal) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- decMIO.parTuple(nullMIO)
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(decFn, nullVal)
          vals.toValDefs.use(_ => validatedAtRuntime(evConfig, configExpr, resultExpr))
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final decoder-only result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def deriveCodecFromCtxAndAdaptForEntrypoint[A: Type, Out: Type](
      macroName: String,
      configExpr: Expr[JsoniterConfig]
  )(
      provideCtxAndAdapt: (
          (Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit],
          (Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A],
          Expr[A]
      ) => Expr[Out]
  ): Expr[Out] = {
    if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] || Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
      Environment.reportErrorAndAbort(
        s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.\n" +
          s"Provide an explicit type parameter, e.g.: $macroName[MyType](...)\n" +
          "or add a type ascription to the result variable."
      )
    Log
      .namedScope(
        s"Deriving codec for ${Type[A].prettyPrint} at: ${Environment.currentPosition.prettyPrint}"
      ) {
        implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
        implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
        implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
        implicit val UnitT: Type[Unit] = CTypes.Unit

        // Three separate MIO values for encoder, decoder and null value, combined with parTuple
        // for parallel error aggregation. Each derivation uses ValDefBuilder to cache its body
        // as a def in the shared ValDefsCache. The returned Scala-level functions generate
        // method-call expressions that are safe to use in any Expr.splice context (no cross-splice
        // staging issues on Scala 3).
        MIO.scoped { runSafe =>
          val cache = ValDefsCache.mlocal
          val selfType: Option[??] = Some(Type[A].as_??)

          // semiEval: evaluate config at compile time for optimized codegen.
          // KNOWN ISSUE (hearth): semiEval fails on Inlined(...) wrapping module field access
          // like JsoniterConfig.default. Pending fix in hearth 0.3.1.
          val evConfig: Option[JsoniterConfig] = semiEvalConfig(configExpr)

          // Validate config for contradictory flag combinations at compile time
          validateConfigOrAbort(evConfig, macroName)

          // Encoder: cache as def, derive body inside, extract function from cache
          val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
            val defBuilder =
              ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](s"codec_encode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec encode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
                  rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined codec encode body for ${Type[A].prettyPrint}")
              fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
            } yield fn.get
          }

          // Decoder: same pattern with ofDef2
          val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]] = {
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, A](s"codec_decode_${Type[A].shortName}")
            for {
              _ <- Log.info(s"Forward-declaring codec decode body for ${Type[A].prettyPrint}")
              _ <- cache.forwardDeclare("codec-decode-body", defBuilder)
              _ <- MIO.scoped { rs =>
                rs(cache.buildCachedWith("codec-decode-body", defBuilder) { case (_, (r, c)) =>
                  rs(deriveDecoderRecursively[A](using DecoderCtx.from(r, c, cache, selfType, evConfig)))
                })
              }
              _ <- Log.info(s"Defined codec decode body for ${Type[A].prettyPrint}")
              fn <- cache.get2Ary[JsonReader, JsoniterConfig, A]("codec-decode-body")
            } yield fn.get
          }

          // Null value
          val nullMIO: MIO[Expr[A]] = deriveNullValue[A]

          // Combine with parTuple (parallel error aggregation)
          val ((encFn, decFn), nullVal) = runSafe {
            for {
              _ <- ensureStandardExtensionsLoaded()
              result <- encMIO.parTuple(decMIO).parTuple(nullMIO)
            } yield result
          }

          val vals = runSafe(cache.get)
          val resultExpr = provideCtxAndAdapt(encFn, decFn, nullVal)
          vals.toValDefs.use(_ => validatedAtRuntime(evConfig, configExpr, resultExpr))
        }
      }
      .flatTap { result =>
        Log.info(s"Derived final codec result: ${result.prettyPrint}")
      }
      .runToExprOrFail(
        macroName,
        infoRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        errorRendering = if (shouldWeLogCodecDerivation) RenderFrom(Log.Level.Info) else DontRender,
        timeout = derivationTimeout
      )(renderDerivationErrorMessage)
  }

  def shouldWeLogCodecDerivation: Boolean = {
    implicit val LogDerivation: Type[KindlingsJsonValueCodec.LogDerivation] = CTypes.CodecLogDerivation
    def logDerivationImported = Expr.summonImplicit[KindlingsJsonValueCodec.LogDerivation].isDefined

    def logDerivationSetGlobally = (for {
      data <- Environment.typedSettings.toOption
      jsoniterDerivation <- data.get("jsoniterDerivation")
      shouldLog <- jsoniterDerivation.get("logDerivation").flatMap(_.asBoolean)
    } yield shouldLog).getOrElse(false)

    logDerivationImported || logDerivationSetGlobally
  }

  private def renderDerivationErrorMessage(errorLogs: String, errors: NonEmptyVector[Throwable]): String = {
    val errorsRendered = errors
      .map { e =>
        e.getMessage.split("\n").toList match {
          case head :: tail => (("  - " + head) :: tail.map("    " + _)).mkString("\n")
          case _            => "  - " + e.getMessage
        }
      }
      .mkString("\n")
    val hint =
      "Enable debug logging with: import hearth.kindlings.jsoniterderivation.debug.logDerivationForKindlingsJsonValueCodec or scalac option -Xmacro-settings:jsoniterDerivation.logDerivation=true"
    if (errorLogs.nonEmpty)
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |and the following logs:
         |$errorLogs
         |$hint""".stripMargin
    else
      s"""Macro derivation failed with the following errors:
         |$errorsRendered
         |$hint""".stripMargin
  }

  // Config conflict validation — reports compile-time errors or warnings when the config is
  // statically known (via semiEval) and contains contradictory flag combinations.
  //
  // - decodingOnly + encodingOnly: hard error (codec is completely unusable)
  // - requireCollectionFields + transientEmpty: warning (well-defined but likely unintended)
  // - requireDefaultFields + transientDefault: warning (well-defined but likely unintended)

  // Config conflict validation (compile-time configs only), with jsoniter-scala's messages.
  private def validateConfigOrAbort(evConfig: Option[JsoniterConfig], macroName: String): Unit =
    evConfig.flatMap(JsoniterConfig.configError).foreach { msg =>
      Environment.reportErrorAndAbort(s"$macroName: $msg")
    }

  /** With a config known only at runtime, the contradictory settings are checked when the codec is created. */
  @scala.annotation.nowarn("msg=is never used")
  private def validatedAtRuntime[Out: Type](
      evConfig: Option[JsoniterConfig],
      configExpr: Expr[JsoniterConfig],
      result: Expr[Out]
  ): Expr[Out] =
    if (evConfig.isDefined) result
    else
      Expr.quote {
        JsoniterConfig.validate(Expr.splice(configExpr))
        Expr.splice(result)
      }

  /** Fails derivation when keys of a case class collide (after `fieldNameMapper` / `@fieldName`), also with the
    * discriminator of an ADT leaf, as jsoniter-scala does. Only checked when the names are known at compile time.
    */
  private[compiletime] def checkFieldNameCollisions[A: Type](names: List[String]): MIO[Unit] = {
    val collisions = names.groupBy(identity).collect { case (n, ns) if ns.size > 1 => n }.toList.sorted
    if (collisions.isEmpty) MIO.pure(())
    else {
      val err = CodecDerivationError.Collision(
        s"Duplicated JSON key(s) defined for '${Type[A].prettyPrint}': ${collisions.mkString("'", "', '", "'")}. " +
          "Keys are derived from field names of the class that are mapped by the 'JsoniterConfig.fieldNameMapper' " +
          "function or can be overridden by '@fieldName' annotation(s). Result keys should be unique and should not " +
          "match with a key for the discriminator field that is specified by the 'JsoniterConfig.discriminatorFieldName' option."
      )
      Log.error(err.message) >> MIO.fail(err)
    }
  }

  /** Fails derivation when ADT leaves (or Java enum values) map to the same name, as jsoniter-scala does. */
  private[compiletime] def checkDiscriminatorValueCollisions[A: Type](names: List[String]): MIO[Unit] = {
    val collisions = names.groupBy(identity).collect { case (n, ns) if ns.size > 1 => n }.toList.sorted
    if (collisions.isEmpty) MIO.pure(())
    else {
      val err = CodecDerivationError.Collision(
        s"Duplicated discriminator defined for ADT base '${Type[A].prettyPrint}': ${collisions.mkString("'", "', '", "'")}. " +
          "Values for leaf classes of ADT that are returned by the 'JsoniterConfig.adtLeafClassNameMapper' function " +
          s"(or '@named' annotations, or 'javaEnumValueNameMapper' for Java enums) should be unique. Names: ${names.mkString(", ")}"
      )
      Log.error(err.message) >> MIO.fail(err)
    }
  }

  // Null value derivation

  @scala.annotation.nowarn("msg=is never used")
  /** Whether a top-level JSON `null` may decode as `decodeValue`'s `default` (reference types; not primitives). */
  @scala.annotation.nowarn("msg=is never used")
  private def nullAsDefaultApplies[A: Type]: Expr[Boolean] = {
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    Expr(Type[A] <:< Type.of[AnyRef])
  }

  /** The codec's `nullValue` (also passed as `default` to the decoders of top-level values), as jsoniter-scala's
    * `genNullValue`: `None` for `Option`s, empty collections/maps/arrays, zero for primitives and their boxes, a value
    * class wrapping its value's `nullValue`, `null` otherwise.
    */
  @scala.annotation.nowarn("msg=is never used")
  // (lazy: it inspects IsCollection & co., which must run after the standard extensions are loaded)
  def deriveNullValue[A: Type]: MIO[Expr[A]] = MIO.pure(()).map(_ => nullValueOf[A])

  @scala.annotation.nowarn("msg=is never used")
  private def nullValueOf[A: Type]: Expr[A] =
    if (Type[A] =:= Type.of[Boolean]) Expr.quote(false.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Byte]) Expr.quote(0.toByte.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Short]) Expr.quote(0.toShort.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Int]) Expr.quote(0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Long]) Expr.quote(0L.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Float]) Expr.quote(0.0f.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Double]) Expr.quote(0.0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Char]) Expr.quote('\u0000'.asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaBoolean) Expr.quote(java.lang.Boolean.FALSE.asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaByte) Expr.quote(java.lang.Byte.valueOf(0.toByte).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaShort) Expr.quote(java.lang.Short.valueOf(0.toShort).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaInteger) Expr.quote(java.lang.Integer.valueOf(0).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaLong) Expr.quote(java.lang.Long.valueOf(0L).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaFloat) Expr.quote(java.lang.Float.valueOf(0.0f).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaDouble) Expr.quote(java.lang.Double.valueOf(0.0).asInstanceOf[A])
    else if (Type[A] =:= CTypes.JavaCharacter) Expr.quote(java.lang.Character.valueOf('\u0000').asInstanceOf[A])
    else if (Type[A] =:= CTypes.String || nullableUnionUnderlying[A].isDefined) Expr.quote(null.asInstanceOf[A])
    else
      Type[A] match {
        case IsOption(_)                                             => Expr.quote(scala.None.asInstanceOf[A])
        case IsValueType(isValueType) if Type[A] <:< Type.of[AnyVal] =>
          import isValueType.Underlying as Inner
          isValueType.value.wrap match {
            case _: CtorLikeOf.PlainValue[?, ?] =>
              isValueType.value.wrap.apply(nullValueOf[Inner]).asInstanceOf[Expr[A]]
            case _ => Expr.quote(null.asInstanceOf[A])
          }
        case _ =>
          if (Type[A] <:< CTypes.ImmutableBitSet || Type[A] =:= CTypes.CollectionBitSet)
            Expr.quote(scala.collection.immutable.BitSet.empty.asInstanceOf[A])
          else if (Type[A] <:< CTypes.MutableBitSet) Expr.quote(new scala.collection.mutable.BitSet().asInstanceOf[A])
          else emptyCollectionOf[A].getOrElse(Expr.quote(null.asInstanceOf[A]))
      }

  /** Cache keys / def names of helpers derived in the different stringified modes (see `EncoderCtx.stringified`). */
  private[compiletime] def stringifiedKeySuffix(stringified: Option[Boolean]): String = stringified match {
    case Some(true)  => "-stringified"
    case Some(false) => "-plain"
    case None        => ""
  }
  private[compiletime] def stringifiedNamePrefix(stringified: Option[Boolean]): String = stringified match {
    case Some(true)  => "stringified_"
    case Some(false) => "plain_"
    case None        => ""
  }

  // Encoder Context

  final case class EncoderCtx[A](
      tpe: Type[A],
      value: Expr[A],
      writer: Expr[JsonWriter],
      config: Expr[JsoniterConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[JsoniterConfig] = None,
      // As in jsoniter-scala, `config.isStringified` applies from the root value down to the first case class / ADT
      // (`None`: inherited from the config); a case class field is stringified only by its own `@stringified`
      // (`Some(true)`, also inside options, collections, map values and tuples) and `Some(false)` otherwise.
      stringified: Option[Boolean] = None
  ) {

    /** Whether numbers/booleans are written as strings: `Some` when known at compile time. */
    def stringifiedMode: Option[Boolean] = stringified.orElse(evaluatedConfig.map(_.isStringified))

    private def helperKey: String = "cached-encode-method" + stringifiedKeySuffix(stringified)

    /** [[stringifiedMode]] as an expression (read from the config at runtime when not known at compile time). */
    def stringifiedExpr: Expr[Boolean] = {
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      stringifiedMode match {
        case Some(b) => Expr(b)
        case None    => Expr.quote(Expr.splice(config).isStringified)
      }
    }

    def nest[B: Type](newValue: Expr[B]): EncoderCtx[B] = copy[B](
      tpe = Type[B],
      value = newValue
    )

    def nestInCache(
        newValue: Expr[A],
        newWriter: Expr[JsonWriter],
        newConfig: Expr[JsoniterConfig]
    ): EncoderCtx[A] = copy(
      value = newValue,
      writer = newWriter,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[JsonValueCodec[B]]]] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      cache.get0Ary[JsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[JsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      Log.info(s"Caching JsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[JsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[B], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      cache.get3Ary[B, JsonWriter, JsoniterConfig, Unit](helperKey)
    }
    def setHelper[B: Type](
        helper: (Expr[B], Expr[JsonWriter], Expr[JsoniterConfig]) => MIO[Expr[Unit]]
    ): MIO[Unit] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      val defBuilder =
        ValDefBuilder.ofDef3[B, JsonWriter, JsoniterConfig, Unit](
          s"encode_${stringifiedNamePrefix(stringified)}${Type[B].shortName}"
        )
      for {
        _ <- Log.info(s"Forward-declaring encode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare(helperKey, defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith(helperKey, defBuilder) { case (_, (value, writer, config)) =>
            runSafe(helper(value, writer, config))
          })
        }
        _ <- Log.info(s"Defined encode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"encode[${tpe.prettyPrint}](value = ${value.prettyPrint}, writer = ${writer.prettyPrint}, config = ${config.prettyPrint})"
  }
  object EncoderCtx {

    def from[A: Type](
        value: Expr[A],
        writer: Expr[JsonWriter],
        config: Expr[JsoniterConfig],
        cache: MLocal[ValDefsCache],
        derivedType: Option[??],
        evaluatedConfig: Option[JsoniterConfig] = None
    ): EncoderCtx[A] = EncoderCtx(
      tpe = Type[A],
      value = value,
      writer = writer,
      config = config,
      cache = cache,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def ectx[A](implicit A: EncoderCtx[A]): EncoderCtx[A] = A

  implicit def currentEncoderValueType[A: EncoderCtx]: Type[A] = ectx.tpe

  abstract class EncoderDerivationRule(val name: String) extends Rule {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]]
  }

  // Encoder derivation

  def deriveEncoderRecursively[A: EncoderCtx]: MIO[Expr[Unit]] =
    // Cache ALL types (including built-ins, options, collections) as helper defs.
    // Without this, built-in types are re-derived for every field reference, causing
    // O(n*m) compilation time for large type graphs (n types × m fields).
    // The forward-declaration also breaks recursive self-references (e.g., Node → List[Node]).
    ectx.getHelper[A].flatMap {
      case Some(helperCall) =>
        Log.info(s"Using cached encoder for ${Type[A].prettyPrint}") >>
          MIO.pure(helperCall(ectx.value, ectx.writer, ectx.config))
      case None =>
        for {
          _ <- ectx.setHelper[A] { (value, writer, config) =>
            deriveEncoderViaRules[A](using ectx.nestInCache(value, writer, config))
          }
          helper <- ectx.getHelper[A]
        } yield helper.get(ectx.value, ectx.writer, ectx.config)
    }

  private[compiletime] def deriveEncoderViaRules[A: EncoderCtx]: MIO[Expr[Unit]] =
    Log
      .namedScope(s"Deriving encoder for type ${Type[A].prettyPrint}") {
        Rules(
          // Note: EncoderUseCachedDefWhenAvailableRule / DecoderUseCachedDefWhenAvailableRule are NOT
          // included here because caching is handled by derive*Recursively (which wraps in setHelper).
          EncoderHandleAsLiteralTypeRule,
          EncoderUseImplicitWhenAvailableRule,
          EncoderDerivationPolicyRule,
          EncoderHandleAsBuiltInRule,
          EncoderHandleAsValueTypeRule,
          EncoderHandleAsOptionRule,
          // EncoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone EncoderHandleAsMapRule is no longer in the chain (its object is kept for
          // deriveMapEntries / deriveKeyEncoding).
          EncoderHandleAsCollectionRule,
          EncoderHandleAsTupleRule,
          EncoderHandleAsNamedTupleRule,
          EncoderHandleAsOneValueClassRule,
          EncoderHandleAsSingletonRule,
          EncoderHandleAsCaseClassRule,
          EncoderHandleAsEnumRule
        )(rule => Log.namedScope(s"encoder rule:${rule.name}")(rule[A])).flatMap {
          case Right(result) =>
            Log.info(s"Derived encoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }

  // Decoder Context

  final case class DecoderCtx[A](
      tpe: Type[A],
      reader: Expr[JsonReader],
      config: Expr[JsoniterConfig],
      cache: MLocal[ValDefsCache],
      derivedType: Option[??],
      evaluatedConfig: Option[JsoniterConfig] = None,
      // See EncoderCtx.stringified.
      stringified: Option[Boolean] = None
  ) {

    /** Whether numbers/booleans are read from strings: `Some` when known at compile time. */
    def stringifiedMode: Option[Boolean] = stringified.orElse(evaluatedConfig.map(_.isStringified))

    private def helperKey: String = "cached-decode-method" + stringifiedKeySuffix(stringified)

    /** [[stringifiedMode]] as an expression (read from the config at runtime when not known at compile time). */
    def stringifiedExpr: Expr[Boolean] = {
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      stringifiedMode match {
        case Some(b) => Expr(b)
        case None    => Expr.quote(Expr.splice(config).isStringified)
      }
    }

    def nest[B: Type](newReader: Expr[JsonReader]): DecoderCtx[B] = copy[B](
      tpe = Type[B],
      reader = newReader
    )

    def nestInCache(
        newReader: Expr[JsonReader],
        newConfig: Expr[JsoniterConfig]
    ): DecoderCtx[A] = copy(
      reader = newReader,
      config = newConfig
    )

    def getInstance[B: Type]: MIO[Option[Expr[JsonValueCodec[B]]]] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      cache.get0Ary[JsonValueCodec[B]]("cached-codec-instance")
    }
    def setInstance[B: Type](instance: Expr[JsonValueCodec[B]]): MIO[Unit] = {
      implicit val CodecB: Type[JsonValueCodec[B]] = CTypes.JsonValueCodec[B]
      Log.info(s"Caching JsonValueCodec instance for ${Type[B].prettyPrint}") >>
        cache.buildCachedWith(
          "cached-codec-instance",
          ValDefBuilder.ofLazy[JsonValueCodec[B]](s"codec_${Type[B].shortName}")
        )(_ => instance)
    }

    def getHelper[B: Type]: MIO[Option[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[B]]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      cache.get2Ary[JsonReader, JsoniterConfig, B](helperKey)
    }
    def setHelper[B: Type](
        helper: (Expr[JsonReader], Expr[JsoniterConfig]) => MIO[Expr[B]]
    ): MIO[Unit] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val ConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      val defBuilder =
        ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, B](
          s"decode_${stringifiedNamePrefix(stringified)}${Type[B].shortName}"
        )
      for {
        _ <- Log.info(s"Forward-declaring decode helper for ${Type[B].prettyPrint}")
        _ <- cache.forwardDeclare(helperKey, defBuilder)
        _ <- MIO.scoped { runSafe =>
          runSafe(cache.buildCachedWith(helperKey, defBuilder) { case (_, (reader, config)) =>
            runSafe(helper(reader, config))
          })
        }
        _ <- Log.info(s"Defined decode helper for ${Type[B].prettyPrint}")
      } yield ()
    }

    override def toString: String =
      s"decode[${tpe.prettyPrint}](reader = ${reader.prettyPrint}, config = ${config.prettyPrint})"
  }
  object DecoderCtx {

    def from[A: Type](
        reader: Expr[JsonReader],
        config: Expr[JsoniterConfig],
        cache: MLocal[ValDefsCache],
        derivedType: Option[??],
        evaluatedConfig: Option[JsoniterConfig] = None
    ): DecoderCtx[A] = DecoderCtx(
      tpe = Type[A],
      reader = reader,
      config = config,
      cache = cache,
      derivedType = derivedType,
      evaluatedConfig = evaluatedConfig
    )
  }

  def dctx[A](implicit A: DecoderCtx[A]): DecoderCtx[A] = A

  implicit def currentDecoderValueType[A: DecoderCtx]: Type[A] = dctx.tpe

  abstract class DecoderDerivationRule(val name: String) extends Rule {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]]
  }

  // Decoder derivation

  // Guard to ensure loadStandardExtensions() is called at most once per expansion,
  // since it is not idempotent (each call re-registers all providers).
  private var standardExtensionsLoaded: Boolean = false
  private def ensureStandardExtensionsLoaded(): MIO[Unit] =
    if (standardExtensionsLoaded) MIO.pure(())
    else
      Environment.loadStandardExtensions().toMIO(allowFailures = false).map { _ =>
        standardExtensionsLoaded = true
        ()
      }

  // Shared ignored implicits for summonExprIgnoring — prevents infinite macro expansion
  // when summoning finds library auto-derivation methods (e.g., KindlingsJsonCodec.derived).
  private[compiletime] lazy val codecIgnoredImplicits: Seq[UntypedMethod] =
    Type.of[KindlingsJsonValueCodec.type].unsortedMethods.collect {
      case method if method.isImplicit => method.asUntyped
    } ++ Type.of[KindlingsJsonCodec.type].unsortedMethods.collect {
      case method if method.isImplicit => method.asUntyped
    }

  // Cache for summonExprIgnoring results — avoids re-summoning the same type.
  // Key: UntypedType, Value: Right(expr) on success, Left(reason) on failure.
  private val summonCodecCache: scala.collection.mutable.Map[UntypedType, Either[String, Any]] =
    scala.collection.mutable.Map.empty

  /** Summon JsonValueCodec[A] with caching. Returns cached result on repeated calls for the same type. */
  private[compiletime] def summonJsonValueCodecCached[A: Type]: Either[String, Expr[JsonValueCodec[A]]] =
    summonCodecCache
      .getOrElseUpdate(
        Type[A].asUntyped,
        CTypes.JsonValueCodec[A].summonExprIgnoring(codecIgnoredImplicits*).toEither
      )
      .asInstanceOf[Either[String, Expr[JsonValueCodec[A]]]]

  // Debug counter to detect runaway decoder derivation — fail gracefully instead of OOM
  private var decoderDerivationCounter: Int = 0
  private val decoderDerivationLimit: Int = 500

  def deriveDecoderRecursively[A: DecoderCtx]: MIO[Expr[A]] = {
    decoderDerivationCounter += 1
    if (decoderDerivationCounter > decoderDerivationLimit)
      MIO.fail(
        new RuntimeException(
          s"Derivation limit ($decoderDerivationLimit) exceeded at decoder for ${Type[A].prettyPrint}. " +
            "This likely indicates an infinite derivation loop."
        )
      )
    else
      dctx.getHelper[A].flatMap {
        case Some(helperCall) =>
          Log.info(s"Using cached decoder for ${Type[A].prettyPrint}") >>
            MIO.pure(helperCall(dctx.reader, dctx.config))
        case None =>
          for {
            _ <- dctx.setHelper[A] { (reader, config) =>
              deriveDecoderViaRules[A](using dctx.nestInCache(reader, config))
            }
            helper <- dctx.getHelper[A]
          } yield helper.get(dctx.reader, dctx.config)
      }
  }

  private[compiletime] def deriveDecoderViaRules[A: DecoderCtx]: MIO[Expr[A]] =
    Log
      .namedScope(s"Deriving decoder for type ${Type[A].prettyPrint}") {
        Rules(
          // Note: EncoderUseCachedDefWhenAvailableRule / DecoderUseCachedDefWhenAvailableRule are NOT
          // included here because caching is handled by derive*Recursively (which wraps in setHelper).
          DecoderHandleAsLiteralTypeRule,
          DecoderUseImplicitWhenAvailableRule,
          DecoderDerivationPolicyRule,
          DecoderHandleAsBuiltInRule,
          DecoderHandleAsValueTypeRule,
          DecoderHandleAsOptionRule,
          // DecoderHandleAsCollectionRule now handles maps too (single IsCollection parse + IsMapOf dispatch),
          // so the standalone DecoderHandleAsMapRule is no longer in the chain (its object is kept for
          // decodeMapEntries / deriveKeyDecoding).
          DecoderHandleAsCollectionRule,
          DecoderHandleAsTupleRule,
          DecoderHandleAsNamedTupleRule,
          DecoderHandleAsOneValueClassRule,
          DecoderHandleAsSingletonRule,
          DecoderHandleAsCaseClassRule,
          DecoderHandleAsEnumRule
        )(rule => Log.namedScope(s"decoder rule:${rule.name}")(rule[A])).flatMap {
          case Right(result) =>
            Log.info(s"Derived decoder for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
              MIO.pure(result)
          case Left(reasons) =>
            val reasonsStrings = reasons.toListMap.view.map { case (rule, reasons) =>
              if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
              else
                s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
            }.toList
            val err = CodecDerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings)
            Log.error(err.message) >> MIO.fail(err)
        }
      }
}
