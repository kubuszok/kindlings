package hearth.kindlings.oracle.catsderivation

// Fixtures shared by the cats-derivation (Kindlings) and kittens (upstream) sides of `CatsDerivationOracleSpec`.

final case class CtPerson(name: String, age: Int)
final case class CtNested(id: Long, person: CtPerson, tags: List[String], nick: Option[String])
final case class CtNoFields()

// Declaration order (ZCircle, ARect, MOrigin) differs from the alphabetical one, so subtype ordering is observable.
sealed trait CtShape
final case class CtZCircle(radius: Double) extends CtShape
final case class CtARect(width: Double, height: Double) extends CtShape
case object CtMOrigin extends CtShape

sealed trait CtTree
final case class CtLeaf(value: Int) extends CtTree
final case class CtNode(left: CtTree, right: CtTree) extends CtTree

final case class CtStats(count: Int, total: Long, names: List[String], best: Option[Int])
final case class CtCounts(a: Int, b: Long, m: Map[String, Int])

// `Set[Int]` has a `PartialOrder` but no `Order` in cats.
final case class CtPartial(set: Set[Int], n: Int)
sealed trait CtPShape
final case class CtPCircle(set: Set[Int]) extends CtPShape
final case class CtPSquare(n: Int) extends CtPShape

// Exactly one subtype (the case object) forms an `Empty`: nothing provides an `Empty[UUID]`.
sealed trait CtMaybe
case object CtNothing extends CtMaybe
final case class CtJust(id: java.util.UUID) extends CtMaybe

// Polymorphic fixtures.
final case class CtBox[A](value: A)
final case class CtPair[A](first: A, second: A, label: String)
final case class CtTagged[A](items: List[A], opt: Option[A], head: A, nested: List[Option[A]])
sealed trait CtIList[A]
final case class CtINil[A]() extends CtIList[A]
final case class CtICons[A](head: A, tail: CtIList[A]) extends CtIList[A]
final case class CtNel[A](head: A, tail: List[A])
final case class CtNelPlus[A](first: Option[A], middle: CtNel[A], last: List[A])
final case class CtPred[A](run: A => Boolean, size: A => Int)
final case class CtCodec[A](encode: A => String, decode: String => A)
final case class CtLists[A](list: List[A], opt: Option[A])
final case class CtProd[A](a: A, list: List[A])
final case class CtTriple[A](a: A, b: A, c: A)
