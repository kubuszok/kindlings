package hearth.kindlings.integrationtests

import neotype.*

type NeotypeEmail = NeotypeEmail.Type
object NeotypeEmail extends Newtype[String] {
  override inline def validate(input: String): Boolean | String = input.contains("@")
}

type NeotypeAge = NeotypeAge.Type
object NeotypeAge extends Subtype[Int] {
  override inline def validate(input: Int): Boolean | String = input >= 0
}

// No `validate` override: every value is accepted.
type NeotypeNickname = NeotypeNickname.Type
object NeotypeNickname extends Newtype[String]

// Companion-provided instances must win over the IsValueType provider's unwrapping.
type NeotypeCustomId = NeotypeCustomId.Type
object NeotypeCustomId extends Newtype[Int] {
  implicit val encoder: io.circe.Encoder[Type] =
    io.circe.Encoder.encodeString.contramap(id => s"id-${unwrap(id)}")
  implicit val decoder: io.circe.Decoder[Type] =
    io.circe.Decoder.decodeString.emap(s => s.stripPrefix("id-").toIntOption.toRight("bad id").flatMap(make))
}

case class NeotypePerson(email: NeotypeEmail, age: NeotypeAge, nickname: NeotypeNickname)
case class WithNeotypeOption(value: Option[NeotypeAge])
case class WithNeotypeCustomId(id: NeotypeCustomId)
case class WithNeotypeKeyMap(values: Map[NeotypeEmail, Int])
case class WithNeotypeIntKeyMap(values: Map[NeotypeAge, Int])

// Plain surrogate for testing neotype validation rejection via binary codecs
case class PlainNeotypePerson(email: String, age: Int, nickname: String)
