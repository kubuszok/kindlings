package hearth.kindlings.benchmarks

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

object OriginalCirceInstances {

  implicit val simpleCCEncoder: Encoder[SimpleCC] = deriveEncoder[SimpleCC]
  implicit val simpleCCDecoder: Decoder[SimpleCC] = deriveDecoder[SimpleCC]

  implicit val addressEncoder: Encoder[Address] = deriveEncoder[Address]
  implicit val addressDecoder: Decoder[Address] = deriveDecoder[Address]
  implicit val personEncoder: Encoder[Person] = deriveEncoder[Person]
  implicit val personDecoder: Decoder[Person] = deriveDecoder[Person]

  implicit val fooEncoder: Encoder[SimpleADT.Foo] = deriveEncoder[SimpleADT.Foo]
  implicit val fooDecoder: Decoder[SimpleADT.Foo] = deriveDecoder[SimpleADT.Foo]
  implicit val barEncoder: Encoder[SimpleADT.Bar] = deriveEncoder[SimpleADT.Bar]
  implicit val barDecoder: Decoder[SimpleADT.Bar] = deriveDecoder[SimpleADT.Bar]
  implicit val bazEncoder: Encoder[SimpleADT.Baz.type] = deriveEncoder[SimpleADT.Baz.type]
  implicit val bazDecoder: Decoder[SimpleADT.Baz.type] = deriveDecoder[SimpleADT.Baz.type]
  implicit val simpleADTEncoder: Encoder[SimpleADT] = deriveEncoder[SimpleADT]
  implicit val simpleADTDecoder: Decoder[SimpleADT] = deriveDecoder[SimpleADT]

  implicit val userCreatedEncoder: Encoder[Event.UserCreated] = deriveEncoder[Event.UserCreated]
  implicit val userCreatedDecoder: Decoder[Event.UserCreated] = deriveDecoder[Event.UserCreated]
  implicit val userUpdatedEncoder: Encoder[Event.UserUpdated] = deriveEncoder[Event.UserUpdated]
  implicit val userUpdatedDecoder: Decoder[Event.UserUpdated] = deriveDecoder[Event.UserUpdated]
  implicit val userDeletedEncoder: Encoder[Event.UserDeleted] = deriveEncoder[Event.UserDeleted]
  implicit val userDeletedDecoder: Decoder[Event.UserDeleted] = deriveDecoder[Event.UserDeleted]
  implicit val eventEncoder: Encoder[Event] = deriveEncoder[Event]
  implicit val eventDecoder: Decoder[Event] = deriveDecoder[Event]
}
