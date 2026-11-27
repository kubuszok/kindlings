package hearth.kindlings.oracle.scalacheck

import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed

final case class ScPerson(name: String, age: Int, tags: List[Int], nick: Option[String])
sealed trait ScShape
final case class ScCircle(radius: Int) extends ScShape
final case class ScRect(width: Int, height: Int) extends ScShape
case object ScOrigin extends ScShape
sealed trait ScTree
final case class ScLeaf(value: Int) extends ScTree
final case class ScNode(left: ScTree, right: ScTree) extends ScTree
final case class ScWrapper(shape: ScShape, people: List[ScPerson])
// Mirrors scalacheck-derived's `CaseClassWithContainers` (core/src/test/scala/io/github/martinhh/test_classes.scala)
final case class ScContainers(
    set: Set[Int],
    list: List[Boolean],
    option: Option[String],
    either: Either[String, Double],
    map: Map[Int, String],
    vector: Vector[ScShape]
)

/** What both sides of the oracle provide. */
trait ScInstances {
  def arbPerson: Arbitrary[ScPerson]
  def arbShape: Arbitrary[ScShape]
  def arbTree: Arbitrary[ScTree]
  def arbWrapper: Arbitrary[ScWrapper]
  def arbContainers: Arbitrary[ScContainers]
  def shrinkPerson: Shrink[ScPerson]
  def shrinkShape: Shrink[ScShape]
  def shrinkTree: Shrink[ScTree]
  def shrinkWrapper: Shrink[ScWrapper]
  def cogenPerson: Cogen[ScPerson]
  def cogenShape: Cogen[ScShape]
  def cogenTree: Cogen[ScTree]
}

/** Kindlings' scalacheck-derivation vs scalacheck-shapeless (Scala 2.13) / scalacheck-derived (Scala 3). Generators are
  * random, so this compares properties of what they generate (subtype coverage, size bounds, recursion termination,
  * behaviour at size 0); `Shrink` and `Cogen` are deterministic, so their results are compared for fixed values.
  */
final class ScalaCheckOracleSpec extends MacroSuite {
  import ScalaCheckOracleSpec.*

  private val k: ScInstances = ScKindlingsSide
  private val u: ScInstances = ScUpstreamSide

  private def compare[R](label: String)(observe: ScInstances => R): List[String] = {
    def run(i: ScInstances): Any =
      try observe(i)
      catch { case e: Throwable => s"threw ${e.getClass.getSimpleName}" }
    val kr = run(k)
    val ur = run(u)
    Oracle.diff(kr, ur).map(d => s"$label ${d.stripPrefix("$")}")
  }

  /** Like `compare`, but reports a differing result as a single line (for long, documented differences). */
  private def same[R](label: String)(observe: ScInstances => R): List[String] =
    if (compare(label)(observe).isEmpty) Nil else List(s"$label: kindlings and upstream differ")

  private def check(differences: List[String], expected: List[String]): Unit =
    assertEquals(differences, expected, s"Kindlings vs upstream differences:\n${differences.mkString("\n")}")

  test("Arbitrary: subtype coverage, size bounds, recursion termination") {
    check(
      compare("shape subtypes")(i => samples(i.arbShape.arbitrary, 30).map(_.getClass.getSimpleName).toSet) ++
        compare("tree subtypes")(i => samples(i.arbTree.arbitrary, 30).map(_.getClass.getSimpleName).toSet) ++
        compare("wrapper shape subtypes")(i =>
          samples(i.arbWrapper.arbitrary, 30).map(_.shape.getClass.getSimpleName).toSet
        ) ++
        compare("person tags within size")(i =>
          sizes.map(n => samples(i.arbPerson.arbitrary, n).forall(_.tags.size <= n))
        ) ++
        compare("person name within size")(i =>
          sizes.map(n => samples(i.arbPerson.arbitrary, n).forall(_.name.length <= n))
        ) ++
        compare("generates at every size")(i =>
          sizes.map { n =>
            (
              n,
              samples(i.arbPerson.arbitrary, n).nonEmpty,
              samples(i.arbShape.arbitrary, n).nonEmpty,
              samples(i.arbTree.arbitrary, n).nonEmpty,
              samples(i.arbWrapper.arbitrary, n).nonEmpty
            )
          }
        ) ++
        compare("tree terminates")(i =>
          List(10, 100).map(n => samples(i.arbTree.arbitrary, n).forall(depth(_) < 1000))
        ),
      ScPlatformExpectations.arbitrary
    )
  }

  // Both upstreams check that a derived generator produces exactly what the equivalent hand-written `Gen` (built from
  // ScalaCheck's own instances) produces for the same seeds: scalacheck-derived's ArbitraryDerivingSuite ("Generates
  // same values as non-derived Gen") and scalacheck-shapeless' ArbitraryTests (`compareArbitrary`). Here the reference
  // is the upstream derivation itself. Size 0 is left out: see "generates at every size" above.
  test("Arbitrary: same values as upstream for the same seeds") {
    def values[A](gen: Gen[A]): List[Option[A]] =
      for { size <- sizes.filter(_ > 0); i <- (0 until 50).toList } yield gen(
        Gen.Parameters.default.withSize(size),
        Seed(i.toLong)
      )
    check(
      same("person values")(i => values(i.arbPerson.arbitrary)) ++
        same("shape values")(i => values(i.arbShape.arbitrary)) ++
        same("wrapper values")(i => values(i.arbWrapper.arbitrary)) ++
        same("containers values")(i => values(i.arbContainers.arbitrary)),
      ScPlatformExpectations.values
    )
  }

  test("Shrink: streams for fixed values") {
    def shrinks[A](s: Shrink[A], a: A): List[A] = s.shrink(a).take(40).toList
    check(
      compare("shrink person")(i => personValues.map(shrinks(i.shrinkPerson, _))) ++
        compare("shrink tree")(i => treeValues.map(shrinks(i.shrinkTree, _))) ++
        same("shrink shape")(i => shapeValues.map(shrinks(i.shrinkShape, _))) ++
        same("shrink wrapper")(i => wrapperValues.map(shrinks(i.shrinkWrapper, _))) ++
        compare("shrink shape keeps the subtype")(i =>
          shapeValues.map(v => shrinks(i.shrinkShape, v).forall(_.getClass == v.getClass))
        ),
      ScPlatformExpectations.shrink
    )
  }

  test("Cogen: equal values perturb equally, different values differently") {
    def pattern[A](c: Cogen[A], values: List[A]): List[Boolean] =
      for { x <- values; y <- values } yield c.perturb(seed, x) == c.perturb(seed, y)
    check(
      compare("cogen person pattern")(i => pattern(i.cogenPerson, personValues ++ personValues.map(_.copy()))) ++
        compare("cogen shape pattern")(i => pattern(i.cogenShape, shapeValues)) ++
        compare("cogen tree pattern")(i => pattern(i.cogenTree, treeValues)) ++
        compare("cogen person seeds")(i => personValues.map(i.cogenPerson.perturb(seed, _))) ++
        same("cogen shape seeds")(i => shapeValues.map(i.cogenShape.perturb(seed, _))) ++
        same("cogen tree seeds")(i => treeValues.map(i.cogenTree.perturb(seed, _))),
      ScPlatformExpectations.cogen
    )
  }
}
object ScalaCheckOracleSpec {

  val seed: Seed = Seed(42L)
  val sizes: List[Int] = List(0, 1, 2, 5, 20)

  /** Up to 300 values generated with fixed seeds at `size` (failed generations are skipped). */
  def samples[A](gen: Gen[A], size: Int): List[A] =
    (0 until 300).toList.flatMap(i => gen(Gen.Parameters.default.withSize(size), Seed(i.toLong)))

  def depth(t: ScTree): Int = t match {
    case ScLeaf(_)    => 1
    case ScNode(l, r) => 1 + math.max(depth(l), depth(r))
  }

  val personValues: List[ScPerson] = List(
    ScPerson("ab", 10, List(1, 2), Some("x")),
    ScPerson("", 0, Nil, None),
    ScPerson("z", -3, List(5), None)
  )
  val shapeValues: List[ScShape] = List(ScCircle(4), ScCircle(0), ScRect(2, 3), ScOrigin)
  val treeValues: List[ScTree] =
    List(ScLeaf(2), ScLeaf(0), ScNode(ScLeaf(2), ScLeaf(0)), ScNode(ScNode(ScLeaf(1), ScLeaf(1)), ScLeaf(3)))
  val wrapperValues: List[ScWrapper] = List(
    ScWrapper(ScRect(1, 2), List(ScPerson("a", 1, Nil, None))),
    ScWrapper(ScOrigin, Nil)
  )
}

/** Kindlings: `DeriveArbitrary.derived` / `DeriveShrink.derived` / `DeriveCogen.derived`. */
object ScKindlingsSide extends ScInstances {
  import hearth.kindlings.scalacheckderivation.{DeriveArbitrary, DeriveCogen, DeriveShrink}

  val arbPerson: Arbitrary[ScPerson] = DeriveArbitrary.derived[ScPerson]
  val arbShape: Arbitrary[ScShape] = DeriveArbitrary.derived[ScShape]
  val arbTree: Arbitrary[ScTree] = DeriveArbitrary.derived[ScTree]
  val arbWrapper: Arbitrary[ScWrapper] = DeriveArbitrary.derived[ScWrapper]
  val arbContainers: Arbitrary[ScContainers] = DeriveArbitrary.derived[ScContainers]
  val shrinkPerson: Shrink[ScPerson] = DeriveShrink.derived[ScPerson]
  val shrinkShape: Shrink[ScShape] = DeriveShrink.derived[ScShape]
  val shrinkTree: Shrink[ScTree] = DeriveShrink.derived[ScTree]
  val shrinkWrapper: Shrink[ScWrapper] = DeriveShrink.derived[ScWrapper]
  val cogenPerson: Cogen[ScPerson] = DeriveCogen.derived[ScPerson]
  val cogenShape: Cogen[ScShape] = DeriveCogen.derived[ScShape]
  val cogenTree: Cogen[ScTree] = DeriveCogen.derived[ScTree]
}
