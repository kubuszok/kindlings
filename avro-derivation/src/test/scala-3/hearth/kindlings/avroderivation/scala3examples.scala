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
