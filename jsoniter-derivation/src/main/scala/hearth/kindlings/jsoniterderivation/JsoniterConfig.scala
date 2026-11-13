package hearth.kindlings.jsoniterderivation

// NOTE on default differences from jsoniter-scala's CodecMakerConfig:
//
// Several defaults intentionally differ from upstream to provide less surprising behavior
// out of the box. Users migrating from jsoniter-scala's `make` macro should review these:
//
// | Config field            | jsoniter-scala   | Kindlings        | Rationale                                      |
// |-------------------------|------------------|------------------|-------------------------------------------------|
// | transientDefault        | true             | false            | All fields present in output by default;        |
// |                         |                  |                  | avoids silent field omission surprising users.  |
// | transientEmpty          | true             | false            | Empty collections are emitted by default;       |
// |                         |                  |                  | opt-in via withTransientEmpty.                  |
// | transientNone           | true             | false            | None fields are emitted (as null) by default;   |
// |                         |                  |                  | opt-in via withTransientNone.                   |
// | checkFieldDuplication   | true             | false            | Permissive parsing by default; opt-in to strict |
// |                         |                  |                  | duplicate rejection when needed.                |
// | discriminatorFieldName  | Some("type")     | None             | Wrapper-style ADT encoding by default; explicit |
// |                         |                  |                  | opt-in to discriminator field via                |
// |                         |                  |                  | withDiscriminator("type").                       |
// | mapMaxInsertNumber      | 1024             | Int.MaxValue     | No map size limit by default; set explicitly    |
// |                         |                  |                  | via withMapMaxInsertNumber for DoS protection.  |
// | setMaxInsertNumber      | 1024             | Int.MaxValue     | No set size limit by default (only sets are     |
// |                         |                  |                  | limited, as upstream); set explicitly via        |
// |                         |                  |                  | withSetMaxInsertNumber for DoS protection.       |
final case class JsoniterConfig(
    fieldNameMapper: String => String = identity,
    // As in jsoniter-scala: receives the fully-qualified name of the leaf class/object (e.g. `pkg.Outer.Leaf`); the
    // default keeps its simple name.
    adtLeafClassNameMapper: String => String = JsoniterConfig.simpleClassName,
    // NOTE: jsoniter-scala defaults to Some("type"); Kindlings defaults to None (wrapper-style encoding).
    // Use .withDiscriminator("type") to match jsoniter-scala behavior.
    discriminatorFieldName: Option[String] = None,
    skipUnexpectedFields: Boolean = true,
    enumAsStrings: Boolean = false,
    mapAsArray: Boolean = false,
    isStringified: Boolean = false,
    decodingOnly: Boolean = false,
    encodingOnly: Boolean = false,
    circeLikeObjectEncoding: Boolean = false,
    useScalaEnumValueId: Boolean = false,
    // NOTE: jsoniter-scala defaults to true; Kindlings defaults to false so that all fields
    // (including those equal to their default value) are present in output by default.
    transientDefault: Boolean = false,
    transientEmpty: Boolean = false,
    transientNone: Boolean = false,
    requireCollectionFields: Boolean = false,
    requireDefaultFields: Boolean = false,
    // NOTE: jsoniter-scala defaults to true; Kindlings defaults to false for permissive parsing.
    // Use .withCheckFieldDuplication to reject duplicate JSON keys.
    checkFieldDuplication: Boolean = false,
    bigDecimalPrecision: Int = 34,
    bigDecimalScaleLimit: Int = 6178,
    bigDecimalDigitsLimit: Int = 308,
    // NOTE: jsoniter-scala defaults to 1024; Kindlings defaults to Int.MaxValue (no limit).
    // Set explicitly via .withMapMaxInsertNumber(1024) for DoS protection.
    mapMaxInsertNumber: Int = Int.MaxValue,
    // NOTE: jsoniter-scala defaults to 1024; Kindlings defaults to Int.MaxValue (no limit).
    // Set explicitly via .withSetMaxInsertNumber(1024) for DoS protection.
    setMaxInsertNumber: Int = Int.MaxValue,
    bitSetValueLimit: Int = 1024,
    javaEnumValueNameMapper: String => String = identity,
    skipNestedOptionValues: Boolean = false,
    alwaysEmitDiscriminator: Boolean = false,
    inlineOneValueClasses: Boolean = false,
    // As in jsoniter-scala: when `false`, the discriminator field may appear anywhere in the object (the decoder marks the
    // object start, finds the discriminator and re-reads the object from the mark).
    requireDiscriminatorFirst: Boolean = true,
    // As in jsoniter-scala: the digit limit of decoded `BigInt`s (DoS protection).
    bigIntDigitsLimit: Int = 308,
    // As in jsoniter-scala: `scala.transient` on a field works like `@transientField`. Honoured when the config is known
    // at compile time (a runtime config cannot change which fields a codec has).
    scalaTransientSupport: Boolean = false,
    // As in jsoniter-scala: fields with `null` values are not written.
    transientNull: Boolean = false
) {

  // As in jsoniter-scala, the name mappers may be partial functions: names they are not defined at stay unchanged
  // (`withFieldNameMapper { case "a" => "b" }`).
  def withFieldNameMapper(f: String => String): JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.totalize(f))
  def withFieldNameMapper(f: PartialFunction[String, String]): JsoniterConfig =
    copy(fieldNameMapper = JsoniterConfig.totalize(f))
  def withAdtLeafClassNameMapper(f: String => String): JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.totalize(f))
  def withAdtLeafClassNameMapper(f: PartialFunction[String, String]): JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.totalize(f))
  def withSnakeCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.enforce_snake_case)
  def withKebabCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.`enforce-kebab-case`)
  def withPascalCaseFieldNames: JsoniterConfig = copy(fieldNameMapper = JsoniterConfig.EnforcePascalCase)
  def withScreamingSnakeCaseFieldNames: JsoniterConfig =
    copy(fieldNameMapper = JsoniterConfig.screamingSnakeCase)
  def withSnakeCaseAdtLeafClassNames: JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.simpleClassName.andThen(JsoniterConfig.enforce_snake_case))
  def withKebabCaseAdtLeafClassNames: JsoniterConfig =
    copy(adtLeafClassNameMapper = JsoniterConfig.simpleClassName.andThen(JsoniterConfig.`enforce-kebab-case`))
  def withDiscriminator(field: String): JsoniterConfig = copy(discriminatorFieldName = Some(field))

  /** jsoniter-scala's `withDiscriminatorFieldName`: `None` for wrapper-style ADTs. */
  def withDiscriminatorFieldName(field: Option[String]): JsoniterConfig = copy(discriminatorFieldName = field)
  def withSkipUnexpectedFields(skip: Boolean): JsoniterConfig = copy(skipUnexpectedFields = skip)
  def withEnumAsStrings: JsoniterConfig = copy(enumAsStrings = true)
  def withMapAsArray: JsoniterConfig = copy(mapAsArray = true)
  def withStringified: JsoniterConfig = copy(isStringified = true)
  def withDecodingOnly: JsoniterConfig = copy(decodingOnly = true)
  def withEncodingOnly: JsoniterConfig = copy(encodingOnly = true)
  def withCirceLikeObjectEncoding: JsoniterConfig = copy(circeLikeObjectEncoding = true)
  def withUseScalaEnumValueId: JsoniterConfig = copy(useScalaEnumValueId = true)
  def withTransientDefault: JsoniterConfig = copy(transientDefault = true)
  def withTransientEmpty: JsoniterConfig = copy(transientEmpty = true)
  def withTransientNone: JsoniterConfig = copy(transientNone = true)
  def withRequireCollectionFields: JsoniterConfig = copy(requireCollectionFields = true)
  def withRequireDefaultFields: JsoniterConfig = copy(requireDefaultFields = true)
  def withCheckFieldDuplication: JsoniterConfig = copy(checkFieldDuplication = true)
  def withBigDecimalPrecision(precision: Int): JsoniterConfig = copy(bigDecimalPrecision = precision)
  def withBigDecimalScaleLimit(scaleLimit: Int): JsoniterConfig =
    copy(bigDecimalScaleLimit = scaleLimit)
  def withBigDecimalDigitsLimit(digitsLimit: Int): JsoniterConfig =
    copy(bigDecimalDigitsLimit = digitsLimit)
  def withMapMaxInsertNumber(max: Int): JsoniterConfig = copy(mapMaxInsertNumber = max)
  def withSetMaxInsertNumber(max: Int): JsoniterConfig = copy(setMaxInsertNumber = max)
  def withBitSetValueLimit(limit: Int): JsoniterConfig = copy(bitSetValueLimit = limit)
  def withJavaEnumValueNameMapper(f: String => String): JsoniterConfig =
    copy(javaEnumValueNameMapper = JsoniterConfig.totalize(f))
  def withJavaEnumValueNameMapper(f: PartialFunction[String, String]): JsoniterConfig =
    copy(javaEnumValueNameMapper = JsoniterConfig.totalize(f))
  def withSkipNestedOptionValues: JsoniterConfig = copy(skipNestedOptionValues = true)
  def withAlwaysEmitDiscriminator: JsoniterConfig = copy(alwaysEmitDiscriminator = true)
  def withInlineOneValueClasses: JsoniterConfig = copy(inlineOneValueClasses = true)
  def withRequireDiscriminatorFirst(require: Boolean): JsoniterConfig = copy(requireDiscriminatorFirst = require)
  def withBigIntDigitsLimit(digitsLimit: Int): JsoniterConfig = copy(bigIntDigitsLimit = digitsLimit)
  def withScalaTransientSupport: JsoniterConfig = copy(scalaTransientSupport = true)
  def withTransientNull: JsoniterConfig = copy(transientNull = true)
}
object JsoniterConfig {

  implicit val default: JsoniterConfig = JsoniterConfig()

  /** jsoniter-scala's error for contradictory settings, if any. Checked at compile time for configs known then, and
    * otherwise when a codec is created (`IllegalArgumentException`).
    */
  def configError(config: JsoniterConfig): Option[String] =
    if (config.decodingOnly && config.encodingOnly)
      Some("'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously")
    else if (config.requireCollectionFields && config.transientEmpty)
      Some("'requireCollectionFields' and 'transientEmpty' cannot be 'true' simultaneously")
    else if (config.requireDefaultFields && config.transientDefault)
      Some("'requireDefaultFields' and 'transientDefault' cannot be 'true' simultaneously")
    else if (config.circeLikeObjectEncoding && config.discriminatorFieldName.isDefined)
      Some("'discriminatorFieldName' should be 'None' when 'circeLikeObjectEncoding' is 'true'")
    else if (config.alwaysEmitDiscriminator && config.discriminatorFieldName.isEmpty)
      Some("'discriminatorFieldName' should not be 'None' when 'alwaysEmitDiscriminator' is 'true'")
    else None

  /** Throws `IllegalArgumentException` for a contradictory config (see [[configError]]). */
  def validate(config: JsoniterConfig): Unit =
    configError(config).foreach(msg => throw new IllegalArgumentException(msg))

  /** A name mapper applied as jsoniter-scala applies them (`mapper.lift(name).getOrElse(name)`): a `PartialFunction`
    * keeps the names it is not defined at.
    */
  def totalize(f: String => String): String => String = f match {
    case pf: PartialFunction[String, String] @unchecked => s => pf.applyOrElse(s, identity[String])
    case total                                          => total
  }

  val jsoniterScalaDefaults: JsoniterConfig = JsoniterConfig(
    discriminatorFieldName = Some("type"),
    transientDefault = true,
    transientEmpty = true,
    transientNone = true,
    checkFieldDuplication = true,
    mapMaxInsertNumber = 1024,
    setMaxInsertNumber = 1024
  )

  // Presets equivalent to jsoniter-scala's `JsonCodecMaker.makeXxx` entry points: `KindlingsJsonValueCodec.derived[A]`
  // with one of them is upstream's `JsonCodecMaker.makeXxx[A]` (built on [[jsoniterScalaDefaults]], i.e. upstream's
  // `CodecMakerConfig` defaults, with exactly the settings each `makeXxx` changes).

  /** `JsonCodecMaker.makeWithoutDiscriminator` */
  lazy val makeWithoutDiscriminator: JsoniterConfig = jsoniterScalaDefaults.copy(discriminatorFieldName = None)

  /** `JsonCodecMaker.makeWithRequiredCollectionFields` */
  lazy val makeWithRequiredCollectionFields: JsoniterConfig =
    jsoniterScalaDefaults.copy(transientEmpty = false, requireCollectionFields = true)

  /** `JsonCodecMaker.makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName` */
  lazy val makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName: JsoniterConfig =
    makeWithRequiredCollectionFields.copy(discriminatorFieldName = Some("name"))

  /** `JsonCodecMaker.makeWithRequiredDefaultFields` */
  lazy val makeWithRequiredDefaultFields: JsoniterConfig =
    jsoniterScalaDefaults.copy(transientDefault = false, requireDefaultFields = true)

  /** `JsonCodecMaker.makeWithSkipNestedOptionValues` */
  lazy val makeWithSkipNestedOptionValues: JsoniterConfig = jsoniterScalaDefaults.copy(skipNestedOptionValues = true)

  /** `JsonCodecMaker.makeCirceLike` */
  lazy val makeCirceLike: JsoniterConfig = jsoniterScalaDefaults.copy(
    transientEmpty = false,
    transientDefault = false,
    transientNone = false,
    discriminatorFieldName = None,
    circeLikeObjectEncoding = true
  )

  /** `JsonCodecMaker.makeCirceLikeSnakeCased` */
  lazy val makeCirceLikeSnakeCased: JsoniterConfig = makeCirceLike.copy(
    adtLeafClassNameMapper = simpleClassName.andThen(enforce_snake_case),
    fieldNameMapper = enforce_snake_case,
    javaEnumValueNameMapper = enforce_snake_case
  )

  /** `JsonCodecMaker.makeOpenapiLike` (recursive types are always allowed in Kindlings) */
  lazy val makeOpenapiLike: JsoniterConfig =
    jsoniterScalaDefaults.copy(transientEmpty = false, transientDefault = false, requireCollectionFields = true)

  /** `JsonCodecMaker.makeOpenapiLike(discriminatorFieldName)` */
  def makeOpenapiLike(discriminatorFieldName: String): JsoniterConfig =
    makeOpenapiLike.copy(requireDiscriminatorFirst = false, discriminatorFieldName = Some(discriminatorFieldName))

  /** `JsonCodecMaker.makeOpenapiLike(discriminatorFieldName, adtLeafClassNameMapper)`: the mapper receives simple class
    * names.
    */
  def makeOpenapiLike(
      discriminatorFieldName: String,
      adtLeafClassNameMapper: PartialFunction[String, String]
  ): JsoniterConfig =
    makeOpenapiLike(discriminatorFieldName).copy(adtLeafClassNameMapper =
      simpleClassName.andThen(adtLeafClassNameMapper)
    )

  /** `JsonCodecMaker.makeOpenapiLikeWithoutDiscriminator` */
  lazy val makeOpenapiLikeWithoutDiscriminator: JsoniterConfig = makeOpenapiLike.copy(discriminatorFieldName = None)

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
