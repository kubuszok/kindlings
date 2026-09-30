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

case class NeotypePerson(email: NeotypeEmail, age: NeotypeAge, nickname: NeotypeNickname)
case class WithNeotypeOption(value: Option[NeotypeAge])

// Plain surrogate for testing neotype validation rejection via binary codecs
case class PlainNeotypePerson(email: String, age: Int, nickname: String)
