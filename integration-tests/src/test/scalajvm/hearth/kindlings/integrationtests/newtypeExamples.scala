package hearth.kindlings.integrationtests

import io.estatico.newtype.macros.{newsubtype, newtype}

// @newtype expands to a type alias + companion, and Scala 2 has no top-level type aliases.
object newtypeExamples {

  @newtype case class NewtypeUserId(value: Int)
  @newtype case class NewtypeUsername(value: String)
  @newsubtype case class NewtypeScore(value: Int)
  @newtype case class NewtypeTags[A](values: List[A])
}

import newtypeExamples.*

case class NewtypeUser(id: NewtypeUserId, name: NewtypeUsername, score: NewtypeScore, tags: NewtypeTags[String])
case class WithNewtypeOption(value: Option[NewtypeUserId])

object NewtypeUser {

  val example: NewtypeUser =
    NewtypeUser(NewtypeUserId(1), NewtypeUsername("alice"), NewtypeScore(42), NewtypeTags(List("a", "b")))
}
