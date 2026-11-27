package hearth.kindlings.oracle.catsderivation

import alleycats.ConsK
import cats.kernel.Order
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

sealed trait CtOne
final case class CtOnly(x: Int, y: String) extends CtOne

/** Type classes kittens derives only on Scala 2: ConsK, and Order for a sealed trait with exactly one subtype (kittens
  * 2's only supported sum shape for Order).
  */
final class CatsDerivationScala2OracleSpec extends MacroSuite {
  import CatsDerivationOracleSpec.{pairs, sign}

  private object KindlingsSide {
    import hearth.kindlings.catsderivation.extensions.*
    val consKNel: ConsK[CtNel] = ConsK.derived[CtNel]
    val consKIList: ConsK[CtIList] = ConsK.derived[CtIList]
    val orderOne: Order[CtOne] = Order.derived[CtOne]
  }

  private object KittensSide {
    import cats.derived.semiauto
    val consKIList: ConsK[CtIList] = semiauto.consK[CtIList]
    val orderOne: Order[CtOne] = semiauto.order[CtOne]
  }

  private def check[R](label: String)(kindlings: R, kittens: R): Unit = {
    val differences = Oracle.diff(kindlings, kittens)
    assertEquals(differences, Nil, s"$label: Kindlings vs kittens differences:\n${differences.mkString("\n")}")
  }

  test("ConsK: list-like sealed traits like kittens, plus case classes prepending to a collection (documented)") {
    // kittens 2 derives ConsK for list-like sealed traits (a cons case `(head: A, tail: F[A])`); Kindlings derives it
    // for those too, and also for case classes (prepending to a nested collection, e.g. `CtNel(head: A, tail: List[A])`),
    // which kittens rejects - documented in docs/user-guide/feature-parity.md (cats-derivation): Improvement.
    val lists: List[CtIList[Int]] = List(CtINil(), CtICons(1, CtICons(2, CtINil())))
    check("consK ilist")(lists.map(KindlingsSide.consKIList.cons(0, _)), lists.map(KittensSide.consKIList.cons(0, _)))

    assertEquals(KindlingsSide.consKNel.cons(0, CtNel(1, List(2))), CtNel(0, List(1, 2)))
    assert(compileErrors("""
      cats.derived.semiauto.consK[hearth.kindlings.oracle.catsderivation.CtNel]
    """).contains("Could not derive ConsK"))
  }

  test("Order on a sealed trait with one subtype") {
    val values: List[CtOne] = List(CtOnly(1, "a"), CtOnly(1, "b"), CtOnly(0, "z"))
    check("order one")(
      pairs(values).map { case (x, y) => sign(KindlingsSide.orderOne.compare(x, y)) },
      pairs(values).map { case (x, y) => sign(KittensSide.orderOne.compare(x, y)) }
    )
  }
}
