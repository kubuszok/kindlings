package hearth.kindlings.circederivation

// Scala 3 enum with parameterized cases (each case is a case class)
enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

enum Color {
  case Red, Green, Blue
}

object OpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class UserWithOpaque(id: OpaqueTypes.UserId, name: String)

// Literal type test types
case class WithLiteralString(tag: "hello", name: String)
case class WithLiteralInt(code: 42, name: String)
case class WithLiteralBoolean(flag: true, name: String)

// Union type test types
type StringOrInt = String | Int
case class Parrot(name: String, vocabulary: Int)
case class Hamster(name: String, wheelSize: Double)
type ParrotOrHamster = Parrot | Hamster
