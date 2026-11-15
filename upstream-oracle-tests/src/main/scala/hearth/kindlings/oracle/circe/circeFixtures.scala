package hearth.kindlings.oracle.circe

// Fixtures of the circe oracle (prefixed `Ci`). They are top-level and have no companion instances, so that the
// Kindlings side and the upstream side each derive them with their own configuration. They live in the main sources so
// that the tests always see them from the classpath, like users deriving instances for types of another module: Hearth
// used to report the subtypes of such types in another order (fixed in Hearth 1bba7e2b, see
// docs/research/hearth-bug-children-order-from-classpath.md), and this keeps that case covered.

final case class CiPerson(firstName: String, age: Int, nickName: Option[String])

final case class CiDefaults(name: String, count: Int = 7, tags: List[String] = List("x"), opt: Option[Int] = Some(1))

final case class CiNested(
    person: CiPerson,
    people: List[CiPerson],
    byName: Map[String, CiPerson],
    intKeys: Map[Int, Long],
    optOpt: Option[Option[Int]],
    pair: (Int, String)
)

final case class CiUserId(value: Int) extends AnyVal

final case class CiWithId(id: CiUserId, ids: List[CiUserId], maybe: Option[CiUserId])

final case class CiNames(HTTPCode: Int, someURLValue: String, x: Boolean, already_snake: Int)

final case class CiEmptyClass()

sealed trait CiShape
final case class CiCircle(radius: Double) extends CiShape
final case class CiRectangle(widthCm: Double, heightCm: Double) extends CiShape
case object CiNoShape extends CiShape

/** A nested hierarchy: `CiPet` is an intermediate sealed trait. */
sealed trait CiAnimal
sealed trait CiPet extends CiAnimal
final case class CiDog(name: String) extends CiPet
final case class CiCat(lives: Int) extends CiPet
final case class CiWolf(packSize: Int) extends CiAnimal
case object CiUnknownAnimal extends CiAnimal

/** A sum of case objects (an enumeration). */
sealed trait CiColor
case object CiRed extends CiColor
case object CiGreen extends CiColor
case object CiDarkBlue extends CiColor

final case class CiTree(value: Int, children: List[CiTree])

sealed trait CiExpr
final case class CiLit(v: Int) extends CiExpr
final case class CiAdd(left: CiExpr, right: CiExpr) extends CiExpr

/** A product holding sums (wrapped/discriminated inside fields). */
final case class CiHolder(shape: CiShape, animals: List[CiAnimal], color: Option[CiColor])

/** For `deriveUnwrapped*`: a single-field case class that is not an `AnyVal`. */
final case class CiWrapper(inner: String)

/** A nested hierarchy whose intermediate sealed trait has a hand-written instance (in its companion, so both sides see
  * it): written as a string under the intermediate's name.
  */
sealed trait CiMedia
sealed trait CiBook extends CiMedia
object CiBook {
  implicit val codec: io.circe.Codec[CiBook] = io.circe.Codec.from(
    io.circe.Decoder.decodeString.map(CiNovel(_)),
    io.circe.Encoder.encodeString.contramap[CiBook] { case CiNovel(title) => title }
  )
}
final case class CiNovel(title: String) extends CiBook
final case class CiFilm(minutes: Int) extends CiMedia
