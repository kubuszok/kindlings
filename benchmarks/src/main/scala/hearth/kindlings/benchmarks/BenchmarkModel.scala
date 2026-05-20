package hearth.kindlings.benchmarks

case class SimpleCC(name: String, age: Int, active: Boolean)

sealed trait SimpleADT
object SimpleADT {
  final case class Foo(value: String) extends SimpleADT
  final case class Bar(value: Int) extends SimpleADT
  case object Baz extends SimpleADT
}

case class SimpleCCBox[A](name: String, age: Int, value: A)

case class Address(street: String, city: String, zip: String, country: String)

case class Person(
    name: String,
    age: Int,
    email: Option[String],
    addresses: List[Address],
    tags: Map[String, String],
    scores: Vector[Double]
)

sealed trait Event
object Event {
  final case class UserCreated(person: Person, timestamp: Long) extends Event
  final case class UserUpdated(personId: String, changes: Map[String, String]) extends Event
  final case class UserDeleted(personId: String, reason: Option[String]) extends Event
}

case class IntPair(x: Int, y: Int)

object BenchmarkData {

  val simpleCC: SimpleCC = SimpleCC("Alice", 30, active = true)
  val simpleADT: SimpleADT = SimpleADT.Foo("hello")

  val address: Address = Address("123 Main St", "Springfield", "62704", "US")
  val person: Person = Person(
    name = "Alice",
    age = 30,
    email = Some("alice@example.com"),
    addresses = List(address, Address("456 Oak Ave", "Shelbyville", "62705", "US")),
    tags = Map("role" -> "admin", "dept" -> "engineering", "level" -> "senior"),
    scores = Vector(95.5, 87.3, 92.1, 88.7, 91.2)
  )

  val event: Event = Event.UserCreated(person, 1700000000000L)

  val simpleCCBox: SimpleCCBox[Int] = SimpleCCBox("Alice", 30, 42)

  val personPair: (Person, Person) = (person, person.copy(name = "Bob", age = 31))

  val intPairA: IntPair = IntPair(10, 20)
  val intPairB: IntPair = IntPair(3, 7)
}
