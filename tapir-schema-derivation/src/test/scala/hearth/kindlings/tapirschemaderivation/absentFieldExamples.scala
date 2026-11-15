package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import sttp.tapir.Schema

// Types for AbsentFieldSchemaSpec: a field is optional in the derived tapir Schema exactly when the decoder derived with
// the same JSON library config accepts it missing from the JSON object.

/** A value type wrapping a collection: coded as the collection, but decoders do not fill it in when absent. */
final case class AbsTags(values: List[String]) extends AnyVal

/** One field of every kind the absent-field semantics distinguish. */
final case class AbsAll(
    plain: Int,
    text: String,
    name: String = "x",
    size: Int = 1,
    tags: List[String],
    counts: Map[String, Int],
    vec: Vector[Int],
    set: Set[Int],
    listWithDefault: List[Int] = List(1),
    opt: Option[Int],
    optWithDefault: Option[Int] = Some(1),
    wrappedTags: AbsTags,
    wrappedTagsWithDefault: AbsTags = AbsTags(List("d"))
)
object AbsAll {
  val sample: AbsAll = AbsAll(
    plain = 1,
    text = "t",
    name = "n",
    size = 2,
    tags = List("a"),
    counts = Map("k" -> 1),
    vec = Vector(1),
    set = Set(1),
    listWithDefault = List(2),
    opt = Some(1),
    optWithDefault = Some(2),
    wrappedTags = AbsTags(List("w")),
    wrappedTagsWithDefault = AbsTags(List("v"))
  )
}

/** A schema and a jsoniter codec derived with the same JsoniterConfig. */
final case class AbsJsoniterDerived(
    name: String,
    config: JsoniterConfig,
    schema: Schema[AbsAll],
    codec: JsonValueCodec[AbsAll]
)

object AbsJsoniterDefault {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived =
    AbsJsoniterDerived(
      "default",
      config,
      KindlingsSchema.derived[AbsAll].schema,
      KindlingsJsonValueCodec.derived[AbsAll]
    )
}

object AbsJsoniterRequireDefaults {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived = AbsJsoniterDerived(
    "requireDefaultFields",
    config,
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsJsonValueCodec.derived[AbsAll]
  )
}

object AbsJsoniterRequireCollections {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived = AbsJsoniterDerived(
    "requireCollectionFields",
    config,
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsJsonValueCodec.derived[AbsAll]
  )
}

object AbsJsoniterRequireBoth {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields.withRequireDefaultFields
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived = AbsJsoniterDerived(
    "requireCollectionFields + requireDefaultFields",
    config,
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsJsonValueCodec.derived[AbsAll]
  )
}

object AbsJsoniterTransient {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withTransientDefault.withTransientEmpty.withTransientNone
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived = AbsJsoniterDerived(
    "transientDefault + transientEmpty + transientNone",
    config,
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsJsonValueCodec.derived[AbsAll]
  )
}

/** A schema and a circe codec derived with the same circe Configuration. */
final case class AbsCirceDerived(
    name: String,
    schema: Schema[AbsAll],
    encoder: io.circe.Encoder[AbsAll],
    decoder: io.circe.Decoder[AbsAll]
)

object AbsCirceDefault {
  implicit val config: Configuration = Configuration.default
  val derived: AbsCirceDerived = AbsCirceDerived(
    "circe default",
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsEncoder.derived[AbsAll],
    KindlingsDecoder.derived[AbsAll]
  )
}

object AbsCirceWithDefaults {
  implicit val config: Configuration = Configuration.default.withDefaults
  val derived: AbsCirceDerived = AbsCirceDerived(
    "circe withDefaults",
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsEncoder.derived[AbsAll],
    KindlingsDecoder.derived[AbsAll]
  )
}

/** A config known only at runtime. */
object AbsJsoniterRuntime {
  @volatile var runtimeConfig: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
  implicit def config: JsoniterConfig = runtimeConfig
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: AbsJsoniterDerived = AbsJsoniterDerived(
    "runtime requireCollectionFields",
    config,
    KindlingsSchema.derived[AbsAll].schema,
    KindlingsJsonValueCodec.derived[AbsAll]
  )
}
