package hearth.kindlings.oracle.optics

// Shared fixtures for the optics oracle: the same paths are modified with Kindlings' optics and with quicklens.

final case class OpStreet(name: String, number: Int)
final case class OpAddress(street: OpStreet, city: String)

final case class OpPerson(
    name: String,
    age: Int,
    address: OpAddress,
    nicknames: List[String],
    scores: Vector[Int],
    tags: Map[String, Int],
    maybeAddress: Option[OpAddress],
    result: Either[String, Int],
    seq: Seq[OpStreet]
)

sealed trait OpAnimal { def name: String }
final case class OpDog(name: String, age: Int) extends OpAnimal
final case class OpCat(name: String, indoor: Boolean) extends OpAnimal

final case class OpZoo(main: OpAnimal, animals: List[OpAnimal], keeper: Option[OpPerson])

/** Not a case class: modified through its hand-written `copy`. */
final class OpVec(val x: Int, val y: Int) {
  def copy(x: Int = x, y: Int = y): OpVec = new OpVec(x, y)
  def show: String = s"OpVec($x, $y)"
}

final case class OpScale(factor: Int)

/** A case class with a second, implicit parameter list: `copy` resolves it at the call site. */
final case class OpScaled(value: Int)(implicit val scale: OpScale) {
  def scaledValue: Int = value * scale.factor
}

final case class OpNode(value: Int, next: Option[OpNode])

object OpFixtures {

  val nodes: OpNode = OpNode(1, Some(OpNode(2, Some(OpNode(3, None)))))

  val vec: OpVec = new OpVec(1, 2)

  def items: Array[OpStreet] = Array(OpStreet("x", 1), OpStreet("y", 2), OpStreet("z", 3))

  val person: OpPerson = OpPerson(
    name = "ann",
    age = 30,
    address = OpAddress(OpStreet("main", 1), "berlin"),
    nicknames = List("annie", "an", "bee"),
    scores = Vector(10, 20, 30),
    tags = Map("a" -> 1, "b" -> 2),
    maybeAddress = Some(OpAddress(OpStreet("side", 2), "paris")),
    result = Right(41),
    seq = Seq(OpStreet("x", 1), OpStreet("y", 2))
  )

  val personWithoutOptions: OpPerson = person.copy(maybeAddress = None, result = Left("error"), tags = Map.empty)

  val zoo: OpZoo = OpZoo(OpDog("rex", 3), List(OpDog("rex", 3), OpCat("tom", true)), Some(person))
  val catZoo: OpZoo = OpZoo(OpCat("tom", false), Nil, None)

  /** The outcome of a modification: its result, or the class of the exception it throws. */
  def outcome[A](a: => A): Either[String, A] =
    try Right(a)
    catch { case e: Exception => Left(e.getClass.getName) }
}
