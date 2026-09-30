package hearth.kindlings.integrationtests

import io.estatico.newtype.macros.{newsubtype, newtype}

// @newtype expands to a type alias + companion, and Scala 2 has no top-level type aliases.
object newtypeExamples {

  @newtype case class NewtypeUserId(value: Int)
  @newtype case class NewtypeUsername(value: String)
  @newsubtype case class NewtypeScore(value: Int)
  @newtype case class NewtypeTags[A](values: List[A])

  // Companion-provided instances must win over the IsValueType provider's unwrapping.
  @newtype case class NewtypeCustomId(value: Int)
  object NewtypeCustomId {
    implicit val encoder: io.circe.Encoder[NewtypeCustomId] =
      io.circe.Encoder.encodeString.contramap(id => s"id-${id.value}")
    implicit val decoder: io.circe.Decoder[NewtypeCustomId] =
      io.circe.Decoder.decodeString.emap(s =>
        s.stripPrefix("id-").toIntOption.map(NewtypeCustomId(_)).toRight("bad id")
      )
  }
}

import newtypeExamples.*

case class NewtypeUser(id: NewtypeUserId, name: NewtypeUsername, score: NewtypeScore, tags: NewtypeTags[String])
case class WithNewtypeOption(value: Option[NewtypeUserId])
case class WithNewtypeCustomId(id: NewtypeCustomId)
case class WithNewtypeKeyMap(values: Map[NewtypeUsername, Int])
case class WithNewtypeIntKeyMap(values: Map[NewtypeUserId, Int])

object NewtypeUser {

  val example: NewtypeUser =
    NewtypeUser(NewtypeUserId(1), NewtypeUsername("alice"), NewtypeScore(42), NewtypeTags(List("a", "b")))
}
