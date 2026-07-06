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

// Issue #80: @avroNamespace on Scala 3 enum
@annotations.avroNamespace("com.example.fruit")
enum NamespacedFruit {
  case Apple, Banana, Cherry
}

@annotations.avroNamespace("com.example.meal")
case class MealWithNamespacedFruit(fruit: NamespacedFruit)

// Option[Scala 3 enum] — must flatten or wrap correctly
case class WithOptionalFruit(fruit: Option[Fruit])
case class WithOptionalVehicle(vehicle: Option[Vehicle])

// `derives` placed directly on a generic enum (value case carrying the type param + a singleton).
// The `derives`-generated companion given `derived$AvroEncoder: AvroEncoder[Updatable3[A]]` is-a
// `AvroSchemaFor[Updatable3[A]]`; deriving the instance must NOT summon that given for its own schema.
object DerivesOnGenericEnum {
  given AvroConfig = AvroConfig.default

  final case class Content3(text: String) derives AvroEncoder, AvroDecoder

  enum Updatable3[+A] derives AvroEncoder, AvroDecoder {
    case Set3(value: A)
    case Keep3
  }

  final case class Record3(field: Updatable3[Content3]) derives AvroEncoder, AvroDecoder
}
