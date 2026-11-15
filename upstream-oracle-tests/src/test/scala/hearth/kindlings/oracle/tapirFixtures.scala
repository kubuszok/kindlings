package hearth.kindlings.oracle

import sttp.tapir.Schema.annotations.{default, description, encodedName, format, validate}
import sttp.tapir.Validator

// Shared fixtures for the tapir oracle: each is derived both by Kindlings and by tapir's own `Schema.derived`.

final case class OPerson(name: String, age: Int, nickname: Option[String])

final case class OPrimitives(
    b: Boolean,
    by: Byte,
    s: Short,
    i: Int,
    l: Long,
    f: Float,
    d: Double,
    str: String,
    bd: BigDecimal,
    bi: BigInt,
    uuid: java.util.UUID,
    instant: java.time.Instant,
    date: java.time.LocalDate,
    duration: java.time.Duration
)

final case class OCollections(
    tags: List[String],
    ids: Set[Int],
    scores: Map[String, Double],
    matrix: Vector[List[Int]],
    maybeTags: Option[List[String]]
)

final case class ODefaults(name: String = "anon", count: Int = 0, nickname: Option[String] = None)

final case class OWrapper(value: String) extends AnyVal
final case class OUsesWrapper(wrapped: OWrapper, maybeWrapped: Option[OWrapper])

sealed trait OShape
final case class OCircle(radius: Double) extends OShape
final case class ORect(width: Double, height: Double) extends OShape
case object OEmpty extends OShape

final case class ODrawing(title: String, shapes: List[OShape])

final case class OTree(value: Int, children: List[OTree])

@description("An annotated product")
final case class OAnnotated(
    @description("the identifier") @encodedName("ID") id: Long,
    @validate(Validator.min(1)) count: Int,
    @format("email") email: String,
    @default("unknown") label: String
)

final case class OBox[A](value: A, items: List[A])
final case class OUsesBox(person: OBox[OPerson], number: OBox[Int])

final case class OCamelCased(firstName: String, lastNameOfPerson: String, httpURL: String)

sealed trait OEvent
final case class OUserCreated(userName: String) extends OEvent
final case class OUserDeleted(userId: Long) extends OEvent

// Maps keyed by a family of case objects / by a type with a circe KeyEncoder: tapir has no derivation for them, its
// users declare `Schema.schemaForMap[K, V](keyToString)`.
sealed trait OKeyColor
case object OKeyRed extends OKeyColor
case object OKeyBlue extends OKeyColor

final case class OKeyCustom(id: Int)
object OKeyCustom {
  implicit val keyEncoder: io.circe.KeyEncoder[OKeyCustom] = io.circe.KeyEncoder.instance(k => s"custom-${k.id}")
}

final case class OMapKeys(colors: Map[OKeyColor, Int], custom: Map[OKeyCustom, String])
