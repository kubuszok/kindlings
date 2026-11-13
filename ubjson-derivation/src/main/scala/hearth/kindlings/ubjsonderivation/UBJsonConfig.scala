package hearth.kindlings.ubjsonderivation

// Defaults as in jsoniter-derivation's JsoniterConfig (wrapper-style ADTs, all fields written, permissive parsing); see
// docs/user-guide/ubjson-derivation.md.
final case class UBJsonConfig(
    fieldNameMapper: String => String = identity,
    // As in jsoniter-scala: receives the fully-qualified name of the leaf class/object (e.g. `pkg.Outer.Leaf`); the
    // default keeps its simple name.
    adtLeafClassNameMapper: String => String = UBJsonConfig.simpleClassName,
    discriminatorFieldName: Option[String] = None,
    skipUnexpectedFields: Boolean = true,
    enumAsStrings: Boolean = false,
    transientDefault: Boolean = false,
    transientEmpty: Boolean = false,
    transientNone: Boolean = false,
    requireCollectionFields: Boolean = false,
    requireDefaultFields: Boolean = false,
    checkFieldDuplication: Boolean = false,
    bigDecimalPrecision: Int = 34,
    bigDecimalScaleLimit: Int = 6178,
    bigDecimalDigitsLimit: Int = 308,
    mapMaxInsertNumber: Int = Int.MaxValue,
    setMaxInsertNumber: Int = Int.MaxValue,
    // As in jsoniter-scala: without a discriminator, case objects are wrapped empty objects (`{"Leaf":{}}`) instead of
    // strings (`"Leaf"`).
    circeLikeObjectEncoding: Boolean = false,
    // As in jsoniter-scala: Java enum values are named by this mapper (applied to the constant's name).
    javaEnumValueNameMapper: String => String = identity,
    // As in jsoniter-scala: the digit limit of decoded `BigInt`s (DoS protection).
    bigIntDigitsLimit: Int = 308,
    // As in jsoniter-scala: when `false`, the discriminator field may appear anywhere in the object (the decoder marks
    // the object start, finds the discriminator and re-reads the object from the mark).
    requireDiscriminatorFirst: Boolean = true,
    // As in jsoniter-scala: `scala.Enumeration` values are encoded as their ids (`Int`s) instead of their names.
    useScalaEnumValueId: Boolean = false,
    // As in jsoniter-scala: an `Option` nested in an `Option` is encoded as any other `Option` (`Some(None)` is `null`,
    // decoded back as `Some(None)`); otherwise it is encoded as the ADT `Option` (`{"Some":{"value":...}}` / `"None"`).
    skipNestedOptionValues: Boolean = false,
    // As in jsoniter-scala: `scala.transient` on a field works like `@transientField` (also with a runtime config).
    scalaTransientSupport: Boolean = false,
    // As in jsoniter-scala: fields of nullable types (Scala 3 `T | Null`) holding `null` are not written.
    transientNull: Boolean = false
) {

  // As in jsoniter-scala, the name mappers may be partial functions: names they are not defined at stay unchanged
  // (`withFieldNameMapper { case "a" => "b" }`).
  def withFieldNameMapper(f: String => String): UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.totalize(f))
  def withFieldNameMapper(f: PartialFunction[String, String]): UBJsonConfig =
    copy(fieldNameMapper = UBJsonConfig.totalize(f))
  def withAdtLeafClassNameMapper(f: String => String): UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.totalize(f))
  def withAdtLeafClassNameMapper(f: PartialFunction[String, String]): UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.totalize(f))
  def withJavaEnumValueNameMapper(f: String => String): UBJsonConfig =
    copy(javaEnumValueNameMapper = UBJsonConfig.totalize(f))
  def withJavaEnumValueNameMapper(f: PartialFunction[String, String]): UBJsonConfig =
    copy(javaEnumValueNameMapper = UBJsonConfig.totalize(f))
  def withSnakeCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.enforce_snake_case)
  def withKebabCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.`enforce-kebab-case`)
  def withPascalCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.EnforcePascalCase)
  def withScreamingSnakeCaseFieldNames: UBJsonConfig = copy(fieldNameMapper = UBJsonConfig.screamingSnakeCase)
  def withSnakeCaseAdtLeafClassNames: UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.simpleClassName.andThen(UBJsonConfig.enforce_snake_case))
  def withKebabCaseAdtLeafClassNames: UBJsonConfig =
    copy(adtLeafClassNameMapper = UBJsonConfig.simpleClassName.andThen(UBJsonConfig.`enforce-kebab-case`))
  def withDiscriminator(field: String): UBJsonConfig = copy(discriminatorFieldName = Some(field))
  def withDiscriminatorFieldName(field: Option[String]): UBJsonConfig = copy(discriminatorFieldName = field)
  def withSkipUnexpectedFields(skip: Boolean): UBJsonConfig = copy(skipUnexpectedFields = skip)
  def withEnumAsStrings: UBJsonConfig = copy(enumAsStrings = true)
  def withTransientDefault: UBJsonConfig = copy(transientDefault = true)
  def withTransientEmpty: UBJsonConfig = copy(transientEmpty = true)
  def withTransientNone: UBJsonConfig = copy(transientNone = true)
  def withRequireCollectionFields: UBJsonConfig = copy(requireCollectionFields = true)
  def withRequireDefaultFields: UBJsonConfig = copy(requireDefaultFields = true)
  def withCheckFieldDuplication: UBJsonConfig = copy(checkFieldDuplication = true)
  def withBigDecimalPrecision(precision: Int): UBJsonConfig = copy(bigDecimalPrecision = precision)
  def withBigDecimalScaleLimit(scaleLimit: Int): UBJsonConfig = copy(bigDecimalScaleLimit = scaleLimit)
  def withBigDecimalDigitsLimit(digitsLimit: Int): UBJsonConfig = copy(bigDecimalDigitsLimit = digitsLimit)
  def withMapMaxInsertNumber(max: Int): UBJsonConfig = copy(mapMaxInsertNumber = max)
  def withSetMaxInsertNumber(max: Int): UBJsonConfig = copy(setMaxInsertNumber = max)
  def withCirceLikeObjectEncoding: UBJsonConfig = copy(circeLikeObjectEncoding = true)
  def withBigIntDigitsLimit(digitsLimit: Int): UBJsonConfig = copy(bigIntDigitsLimit = digitsLimit)
  def withRequireDiscriminatorFirst(require: Boolean): UBJsonConfig = copy(requireDiscriminatorFirst = require)
  def withUseScalaEnumValueId: UBJsonConfig = copy(useScalaEnumValueId = true)
  def withSkipNestedOptionValues: UBJsonConfig = copy(skipNestedOptionValues = true)
  def withScalaTransientSupport: UBJsonConfig = copy(scalaTransientSupport = true)
  def withTransientNull: UBJsonConfig = copy(transientNull = true)

  // Written out by hand (it is what the compiler would synthesize): Scala 2 does not synthesize a case class `copy`
  // when the class declares any other `copy`, and the deprecated overload below is needed for binary compatibility.
  def copy(
      fieldNameMapper: String => String = this.fieldNameMapper,
      adtLeafClassNameMapper: String => String = this.adtLeafClassNameMapper,
      discriminatorFieldName: Option[String] = this.discriminatorFieldName,
      skipUnexpectedFields: Boolean = this.skipUnexpectedFields,
      enumAsStrings: Boolean = this.enumAsStrings,
      transientDefault: Boolean = this.transientDefault,
      transientEmpty: Boolean = this.transientEmpty,
      transientNone: Boolean = this.transientNone,
      requireCollectionFields: Boolean = this.requireCollectionFields,
      requireDefaultFields: Boolean = this.requireDefaultFields,
      checkFieldDuplication: Boolean = this.checkFieldDuplication,
      bigDecimalPrecision: Int = this.bigDecimalPrecision,
      bigDecimalScaleLimit: Int = this.bigDecimalScaleLimit,
      bigDecimalDigitsLimit: Int = this.bigDecimalDigitsLimit,
      mapMaxInsertNumber: Int = this.mapMaxInsertNumber,
      setMaxInsertNumber: Int = this.setMaxInsertNumber,
      circeLikeObjectEncoding: Boolean = this.circeLikeObjectEncoding,
      javaEnumValueNameMapper: String => String = this.javaEnumValueNameMapper,
      bigIntDigitsLimit: Int = this.bigIntDigitsLimit,
      requireDiscriminatorFirst: Boolean = this.requireDiscriminatorFirst,
      useScalaEnumValueId: Boolean = this.useScalaEnumValueId,
      skipNestedOptionValues: Boolean = this.skipNestedOptionValues,
      scalaTransientSupport: Boolean = this.scalaTransientSupport,
      transientNull: Boolean = this.transientNull
  ): UBJsonConfig = new UBJsonConfig(
    fieldNameMapper,
    adtLeafClassNameMapper,
    discriminatorFieldName,
    skipUnexpectedFields,
    enumAsStrings,
    transientDefault,
    transientEmpty,
    transientNone,
    requireCollectionFields,
    requireDefaultFields,
    checkFieldDuplication,
    bigDecimalPrecision,
    bigDecimalScaleLimit,
    bigDecimalDigitsLimit,
    mapMaxInsertNumber,
    setMaxInsertNumber,
    circeLikeObjectEncoding,
    javaEnumValueNameMapper,
    bigIntDigitsLimit,
    requireDiscriminatorFirst,
    useScalaEnumValueId,
    skipNestedOptionValues,
    scalaTransientSupport,
    transientNull
  )

  /** The `copy` of Kindlings 0.3.2 (without the fields added since); the other fields keep their values. */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use copy with named arguments",
    "0.3.4"
  )
  def copy(
      fieldNameMapper: String => String,
      adtLeafClassNameMapper: String => String,
      discriminatorFieldName: Option[String],
      skipUnexpectedFields: Boolean,
      enumAsStrings: Boolean,
      transientDefault: Boolean,
      transientEmpty: Boolean,
      transientNone: Boolean,
      requireCollectionFields: Boolean,
      requireDefaultFields: Boolean,
      checkFieldDuplication: Boolean,
      bigDecimalPrecision: Int,
      bigDecimalScaleLimit: Int,
      bigDecimalDigitsLimit: Int,
      mapMaxInsertNumber: Int,
      setMaxInsertNumber: Int
  ): UBJsonConfig = new UBJsonConfig(
    fieldNameMapper,
    adtLeafClassNameMapper,
    discriminatorFieldName,
    skipUnexpectedFields,
    enumAsStrings,
    transientDefault,
    transientEmpty,
    transientNone,
    requireCollectionFields,
    requireDefaultFields,
    checkFieldDuplication,
    bigDecimalPrecision,
    bigDecimalScaleLimit,
    bigDecimalDigitsLimit,
    mapMaxInsertNumber,
    setMaxInsertNumber,
    this.circeLikeObjectEncoding,
    this.javaEnumValueNameMapper,
    this.bigIntDigitsLimit,
    this.requireDiscriminatorFirst,
    this.useScalaEnumValueId,
    this.skipNestedOptionValues,
    this.scalaTransientSupport,
    this.transientNull
  )

  /** The constructor of Kindlings 0.3.2 (without the fields added since, which get their defaults). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use the primary constructor",
    "0.3.4"
  )
  def this(
      fieldNameMapper: String => String,
      adtLeafClassNameMapper: String => String,
      discriminatorFieldName: Option[String],
      skipUnexpectedFields: Boolean,
      enumAsStrings: Boolean,
      transientDefault: Boolean,
      transientEmpty: Boolean,
      transientNone: Boolean,
      requireCollectionFields: Boolean,
      requireDefaultFields: Boolean,
      checkFieldDuplication: Boolean,
      bigDecimalPrecision: Int,
      bigDecimalScaleLimit: Int,
      bigDecimalDigitsLimit: Int,
      mapMaxInsertNumber: Int,
      setMaxInsertNumber: Int
  ) = this(
    fieldNameMapper,
    adtLeafClassNameMapper,
    discriminatorFieldName,
    skipUnexpectedFields,
    enumAsStrings,
    transientDefault,
    transientEmpty,
    transientNone,
    requireCollectionFields,
    requireDefaultFields,
    checkFieldDuplication,
    bigDecimalPrecision,
    bigDecimalScaleLimit,
    bigDecimalDigitsLimit,
    mapMaxInsertNumber,
    setMaxInsertNumber,
    false,
    identity[String],
    308,
    true,
    false,
    false,
    false,
    false
  )
}
object UBJsonConfig extends UBJsonConfigCompanionCompat {

  implicit val default: UBJsonConfig = UBJsonConfig()

  /** The `apply` of Kindlings 0.3.2 (without the fields added since, which get their defaults). */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use apply with named arguments",
    "0.3.4"
  )
  def apply(
      fieldNameMapper: String => String,
      adtLeafClassNameMapper: String => String,
      discriminatorFieldName: Option[String],
      skipUnexpectedFields: Boolean,
      enumAsStrings: Boolean,
      transientDefault: Boolean,
      transientEmpty: Boolean,
      transientNone: Boolean,
      requireCollectionFields: Boolean,
      requireDefaultFields: Boolean,
      checkFieldDuplication: Boolean,
      bigDecimalPrecision: Int,
      bigDecimalScaleLimit: Int,
      bigDecimalDigitsLimit: Int,
      mapMaxInsertNumber: Int,
      setMaxInsertNumber: Int
  ): UBJsonConfig = new UBJsonConfig(
    fieldNameMapper,
    adtLeafClassNameMapper,
    discriminatorFieldName,
    skipUnexpectedFields,
    enumAsStrings,
    transientDefault,
    transientEmpty,
    transientNone,
    requireCollectionFields,
    requireDefaultFields,
    checkFieldDuplication,
    bigDecimalPrecision,
    bigDecimalScaleLimit,
    bigDecimalDigitsLimit,
    mapMaxInsertNumber,
    setMaxInsertNumber
  )

  // The name mappers of Kindlings 0.3.2: package-private, but public (with static forwarders) in the bytecode.
  @deprecated("kept for binary compatibility with Kindlings 0.3.x; use enforce_snake_case", "0.3.4")
  private[ubjsonderivation] val snakeCase: String => String = s => enforce_snake_case(s)
  @deprecated("kept for binary compatibility with Kindlings 0.3.x; use `enforce-kebab-case`", "0.3.4")
  private[ubjsonderivation] val kebabCase: String => String = s => `enforce-kebab-case`(s)
  @deprecated("kept for binary compatibility with Kindlings 0.3.x; use EnforcePascalCase", "0.3.4")
  private[ubjsonderivation] val pascalCase: String => String = s => EnforcePascalCase(s)

  /** jsoniter-scala's error for contradictory settings, if any. Checked at compile time for configs known then, and
    * otherwise when a codec is created (`IllegalArgumentException`).
    */
  def configError(config: UBJsonConfig): Option[String] =
    if (config.requireCollectionFields && config.transientEmpty)
      Some("'requireCollectionFields' and 'transientEmpty' cannot be 'true' simultaneously")
    else if (config.requireDefaultFields && config.transientDefault)
      Some("'requireDefaultFields' and 'transientDefault' cannot be 'true' simultaneously")
    else if (config.circeLikeObjectEncoding && config.discriminatorFieldName.isDefined)
      Some("'discriminatorFieldName' should be 'None' when 'circeLikeObjectEncoding' is 'true'")
    else None

  /** Throws `IllegalArgumentException` for a contradictory config (see [[configError]]). */
  def validate(config: UBJsonConfig): Unit =
    configError(config).foreach(msg => throw new IllegalArgumentException(msg))

  /** A name mapper applied as jsoniter-scala applies them (`mapper.lift(name).getOrElse(name)`): a `PartialFunction`
    * keeps the names it is not defined at.
    */
  def totalize(f: String => String): String => String = f match {
    case pf: PartialFunction[String, String] @unchecked => s => pf.applyOrElse(s, identity[String])
    case total                                          => total
  }

  /** The simple name of a fully-qualified class name (the default `adtLeafClassNameMapper`), as jsoniter-scala's
    * `JsonCodecMaker.simpleClassName`.
    */
  lazy val simpleClassName: String => String = { fullClassName =>
    val lastComponent = fullClassName.substring(Math.max(fullClassName.lastIndexOf('.') + 1, 0))
    var localPrefixIndex = 0
    while (lastComponent.startsWith("_$", localPrefixIndex)) localPrefixIndex += 2
    lastComponent.substring(localPrefixIndex)
  }

  /** jsoniter-scala's `JsonCodecMaker.enforceCamelCase`: `snake_case`/`kebab-case`/`PascalCase` to `camelCase`. */
  lazy val enforceCamelCase: String => String = s => enforceCamelOrPascalCase(s, toPascal = false)

  /** jsoniter-scala's `JsonCodecMaker.EnforcePascalCase`. */
  lazy val EnforcePascalCase: String => String = s => enforceCamelOrPascalCase(s, toPascal = true)

  /** jsoniter-scala's `JsonCodecMaker.enforce_snake_case`: `"HTTPServer"` is `"http_server"`, `"field1"` is `"field_1"`
    * (non-alphabetic characters are separated).
    */
  lazy val enforce_snake_case: String => String = s => enforceSnakeOrKebabCaseWithSeparatedNonAlphabetic(s, '_')

  /** jsoniter-scala's `JsonCodecMaker.enforce_snake_case2`: as [[enforce_snake_case]], but `"field1"` is `"field1"`. */
  lazy val enforce_snake_case2: String => String = s => enforceSnakeOrKebabCaseWithJoinedNonAlphabetic(s, '_')

  /** jsoniter-scala's ``JsonCodecMaker.`enforce-kebab-case` ``. */
  lazy val `enforce-kebab-case`: String => String = s => enforceSnakeOrKebabCaseWithSeparatedNonAlphabetic(s, '-')

  /** jsoniter-scala's ``JsonCodecMaker.`enforce-kebab-case2` ``. */
  lazy val `enforce-kebab-case2`: String => String = s => enforceSnakeOrKebabCaseWithJoinedNonAlphabetic(s, '-')

  /** `SCREAMING_SNAKE_CASE`: [[enforce_snake_case]] upper-cased. */
  lazy val screamingSnakeCase: String => String = s => enforce_snake_case(s).toUpperCase

  private def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String = {
    import java.lang.Character.{isLowerCase, isUpperCase, toLowerCase, toUpperCase}
    val len = s.length
    val sb = new java.lang.StringBuilder(len)
    if (s.indexOf('_') < 0 && s.indexOf('-') < 0) {
      if (len > 0) {
        val firstChar = s.charAt(0)
        sb.append(if (toPascal) toUpperCase(firstChar) else toLowerCase(firstChar))
        var i = 0
        while (i < len && isUpperCase(s.charAt(i))) i += 1
        if (i > 1 && i < len && isLowerCase(s.charAt(i))) i -= 1
        val limit = Math.max(i, 1)
        i = 1
        while (i < limit) {
          sb.append(toLowerCase(s.charAt(i)))
          i += 1
        }
        while (i < len) {
          sb.append(s.charAt(i))
          i += 1
        }
      }
    } else {
      var i = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          sb.append(if (isPrecedingDash) toUpperCase(ch) else toLowerCase(ch))
          false
        }
      }
    }
    sb.toString
  }

  private def enforceSnakeOrKebabCaseWithSeparatedNonAlphabetic(s: String, separator: Char): String = {
    import java.lang.Character.{isLowerCase, toLowerCase}
    val len = s.length
    val sb = new java.lang.StringBuilder(len << 1)
    var i = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && isLowerCase(s.charAt(i))) { val _ = sb.append(separator) }
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  private def enforceSnakeOrKebabCaseWithJoinedNonAlphabetic(s: String, separator: Char): String = {
    import java.lang.Character.{isAlphabetic, isLowerCase, isUpperCase, toLowerCase}
    val len = s.length
    val sb = new java.lang.StringBuilder(len << 1)
    var i = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        if (i > 1 && i < len && !isAlphabetic(s.charAt(i).toInt)) isPrecedingNotUpperCased
        else {
          sb.append(separator)
          false
        }
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && isLowerCase(s.charAt(i))) { val _ = sb.append(separator) }
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }
}
