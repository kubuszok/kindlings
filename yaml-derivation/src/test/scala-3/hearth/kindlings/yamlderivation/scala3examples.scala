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
