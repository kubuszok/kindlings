package hearth.kindlings.jsoniterderivation

enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

object JsoniterOpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class JsoniterUserWithOpaque(id: JsoniterOpaqueTypes.UserId, name: String)

// Literal type test types
case class JsoniterWithLiteralString(tag: "hello", name: String)
case class JsoniterWithLiteralInt(code: 42, name: String)
case class JsoniterWithLiteralBoolean(flag: true, name: String)

// Union type test types
type StringOrInt = String | Int
case class Parrot(name: String, vocabulary: Int)
case class Hamster(name: String, wheelSize: Double)
type ParrotOrHamster = Parrot | Hamster
