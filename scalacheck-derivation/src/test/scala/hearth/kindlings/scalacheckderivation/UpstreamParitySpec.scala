package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed

/** Behaviour aligned with ScalaCheck's own instances and with scalacheck-shapeless / scalacheck-derived (the upstream
  * oracle in `upstream-oracle-tests` compares the derivations directly).
  */
object UpstreamParitySpec {
  sealed trait UpShape
  final case class UpCircle(radius: Int) extends UpShape
  final case class UpRect(width: Int, height: Int) extends UpShape
  case object UpOrigin extends UpShape

  sealed trait UpTree
  final case class UpLeaf(value: Int) extends UpTree
  final case class UpNode(left: UpTree, right: UpTree) extends UpTree

  final case class UpPerson(name: String, tags: List[Int], nick: Option[String])
  final case class UpNested(groups: List[List[Int]])
}

@scala.annotation.nowarn("msg=deprecated")
final class UpstreamParitySpec extends munit.FunSuite {
  import UpstreamParitySpec.*

  private val seed = Seed(42L)

  private def depth(t: UpTree): Int = t match {
    case UpLeaf(_)    => 1
    case UpNode(l, r) => 1 + math.max(depth(l), depth(r))
  }

  test("sealed trait with case-class children derived inline (no implicit instances for the children)") {
    // Used to emit every cached child definition twice; the unused duplicates failed Scala 2 builds with -Werror.
    val arb = DeriveArbitrary.derived[UpShape]
    val shrink = DeriveShrink.derived[UpShape]
    val cogen = DeriveCogen.derived[UpShape]
    val samples = (0 until 200).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(10), Seed(i.toLong)))
    // By pattern match: `getSimpleName` of an object differs per platform (`UpOrigin$` on the JVM, `UpOrigin` on Native).
    val subtypes = samples.map {
      case _: UpCircle => "UpCircle"
      case _: UpRect   => "UpRect"
      case UpOrigin    => "UpOrigin"
    }.toSet
    assertEquals(subtypes, Set("UpCircle", "UpRect", "UpOrigin"))
    assertEquals(shrink.shrink(UpCircle(2)).toList, List(UpCircle(1), UpCircle(-1), UpCircle(0)))
    assertNotEquals(cogen.perturb(seed, UpCircle(1)), cogen.perturb(seed, UpOrigin))
  }

  test("recursive sealed traits terminate at every size without a Recursive marker") {
    val arb = DeriveArbitrary.derived[UpTree]
    List(0, 1, 5, 30, 100).foreach { size =>
      val samples = (0 until 300).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(size), Seed(i.toLong)))
      assertEquals(samples.size, 300, s"size $size")
      assert(samples.forall(depth(_) < 1000), s"size $size")
    }
    // At size 0 only the non-recursive case is generated.
    val atZero = (0 until 50).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(0), Seed(i.toLong)))
    assert(atZero.forall(_.isInstanceOf[UpLeaf]))
  }

  test("collections shrink with ScalaCheck's own algorithm") {
    val shrink = DeriveShrink.derived[UpPerson]
    val value = UpPerson("ab", List(1, 2, 3), Some("x"))
    val expectedTags = Shrink.shrink(List(1, 2, 3)).toList.map(tags => value.copy(tags = tags))
    val tagShrinks = shrink.shrink(value).toList.filter(p => p.name == value.name && p.nick == value.nick)
    assertEquals(tagShrinks, expectedTags)
  }

  test("Cogen perturbs like ScalaCheck's instances and the upstream derivations") {
    val person = DeriveCogen.derived[UpPerson]
    val value = UpPerson("ab", List(1, 2), Some("x"))
    // Case classes: every field in order, then `Cogen[Unit]` (scalacheck-shapeless and scalacheck-derived).
    val expected = Cogen.perturb(
      Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, value.name), value.tags), value.nick),
      ()
    )
    assertEquals(person.perturb(seed, value), expected)
    assertEquals(
      person.perturb(seed, value.copy(nick = None)),
      Cogen.perturb(Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, "ab"), List(1, 2)), Option.empty[String]), ())
    )

    // Collections end with `next`, so different groupings of the same elements perturb differently.
    val nested = DeriveCogen.derived[UpNested]
    assertNotEquals(
      nested.perturb(seed, UpNested(List(List(1), Nil))),
      nested.perturb(seed, UpNested(List(Nil, List(1))))
    )

    // Sealed traits: the case's Cogen, then the case's index (scalacheck-derived) - in declaration order on Scala 3,
    // by name on Scala 2 (UpCircle, UpOrigin, UpRect: the same in every build, see feature-parity.md).
    val isScala3 = scala.util.Try(Class.forName("scala.runtime.TupleXXL")).isSuccess
    val shape = DeriveCogen.derived[UpShape]
    assertEquals(shape.perturb(seed, UpCircle(3)), Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, 3), ()), 0))
    assertEquals(shape.perturb(seed, UpOrigin), Cogen.perturb(Cogen.perturb(seed, ()), if (isScala3) 2 else 1))
  }
}
