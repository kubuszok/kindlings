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
