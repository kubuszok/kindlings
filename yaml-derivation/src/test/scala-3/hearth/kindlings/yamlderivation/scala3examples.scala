package hearth.kindlings.yamlderivation

enum Fruit {
  case Apple(weight: Double)
  case Banana(length: Double)
}

object YamlOpaqueTypes {
  opaque type UserId = Int
  object UserId {
    def apply(value: Int): UserId = value
    extension (id: UserId) def value: Int = id
  }
}

case class YamlUserWithOpaque(id: YamlOpaqueTypes.UserId, name: String)

// Literal type test types
case class YamlWithLiteralString(tag: "hello", name: String)
case class YamlWithLiteralInt(code: 42, name: String)
case class YamlWithLiteralBoolean(flag: true, name: String)

// Union type test types
type StringOrInt = String | Int
case class Parrot(name: String, vocabulary: Int)
case class Hamster(name: String, wheelSize: Double)
type ParrotOrHamster = Parrot | Hamster
