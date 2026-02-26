package hearth.kindlings.avroderivation

enum Fruit {
  case Apple, Banana, Cherry
}

enum Vehicle {
  case Car(make: String, year: Int)
  case Bike(gears: Int)
}

object AvroOpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class AvroUserWithOpaque(id: AvroOpaqueTypes.UserId, name: String)

// Literal type test types
case class AvroWithLiteralString(tag: "hello", name: String)
case class AvroWithLiteralInt(code: 42, name: String)
case class AvroWithLiteralBoolean(flag: true, name: String)

// Union type test types
type StringOrInt = String | Int
case class Parrot(name: String, vocabulary: Int)
case class Hamster(name: String, wheelSize: Double)
type ParrotOrHamster = Parrot | Hamster
