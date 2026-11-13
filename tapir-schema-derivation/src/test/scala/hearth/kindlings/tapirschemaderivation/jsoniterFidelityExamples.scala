package hearth.kindlings.tapirschemaderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.jsoniterderivation.annotations.named
import sttp.tapir.Schema

// Types whose tapir Schemas (derived with a JsoniterConfig) are compared with what the jsoniter codecs (derived with the
// same config) write - see JsoniterSchemaFidelitySpec.

/** A mixed ADT: case classes and case objects, with `@named` leaves. */
sealed trait FidMixed
object FidMixed {
  case object Origin extends FidMixed
  final case class Point(x: Int, y: Int) extends FidMixed
  @named("renamed-leaf") final case class Renamed(value: String) extends FidMixed
  @named("nobody") case object Nobody extends FidMixed
}

/** An ADT of case objects only. */
sealed trait FidColor
object FidColor {
  case object Red extends FidColor
  case object Green extends FidColor
}

/** A nested sealed hierarchy (jsoniter flattens it to its leaves). */
sealed trait FidVehicle
sealed trait FidMotorVehicle extends FidVehicle
final case class FidTruck(load: Int) extends FidMotorVehicle
case object FidBike extends FidVehicle

object FidSize extends Enumeration {
  val Small: Value = Value
  val Big: Value = Value("BIG")
}

final case class FidHolder(
    mixed: FidMixed,
    color: FidColor,
    vehicle: FidVehicle,
    size: FidSize.Value,
    tuple: (Int, String),
    tuples: List[(Int, Int)],
    nested: Option[Option[Int]],
    @named("renamed_field") renamedField: Int
)

object FidHolder {

  val samples: List[FidHolder] = List(
    FidHolder(FidMixed.Origin, FidColor.Red, FidTruck(1), FidSize.Small, (1, "a"), List((1, 2)), None, 1),
    FidHolder(FidMixed.Point(1, 2), FidColor.Green, FidBike, FidSize.Big, (2, "b"), Nil, Some(None), 2),
    FidHolder(FidMixed.Renamed("r"), FidColor.Red, FidBike, FidSize.Big, (3, "c"), List((3, 4)), Some(Some(5)), 3),
    FidHolder(FidMixed.Nobody, FidColor.Green, FidTruck(2), FidSize.Small, (4, "d"), Nil, Some(Some(0)), 4)
  )
}

/** A schema and a codec derived with the same JsoniterConfig. */
final case class FidDerived(name: String, schema: Schema[FidHolder], codec: JsonValueCodec[FidHolder])

object FidDefault {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("default", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

object FidDiscriminator {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("discriminator", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

object FidCirceLike {
  implicit val config: JsoniterConfig = JsoniterConfig.makeCirceLike
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("circe-like", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

object FidEnumAsStrings {
  implicit val config: JsoniterConfig = JsoniterConfig.default.withDiscriminator("kind").withEnumAsStrings
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("enumAsStrings", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

object FidFullNames {
  // identity: the leaves are named by their fully-qualified names
  implicit val config: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAdtLeafClassNameMapper((s: String) => s).withUseScalaEnumValueId
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("full names", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

object FidSkipNestedOptions {
  implicit val config: JsoniterConfig =
    JsoniterConfig.default.withSkipNestedOptionValues.withSnakeCaseFieldNames.withKebabCaseAdtLeafClassNames
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("skipNestedOptionValues", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}

/** A config known only at runtime. */
object FidRuntime {
  @volatile var runtimeConfig: JsoniterConfig =
    JsoniterConfig.default.withFieldNameMapper { case "renamedField" => "never-used"; case "x" => "X" }
  implicit def config: JsoniterConfig = runtimeConfig
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]
  val derived: FidDerived =
    FidDerived("runtime", KindlingsSchema.derived[FidHolder].schema, KindlingsJsonValueCodec.derived[FidHolder])
}
