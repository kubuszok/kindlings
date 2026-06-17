package hearth.kindlings.optics

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import hearth.MacroSuite

/** `.each` over cats collections works with NO optics-specific cats code — purely because `kindlings-cats-integration`
  * (a TEST dependency here) registers `IsCollection` providers on the classpath, which the `modify` macro discovers via
  * `loadStandardExtensions`. The only import is the optics DSL itself.
  */
final class CatsEachSpec extends MacroSuite {

  import hearth.kindlings.optics.*

  group("`.each` over cats collections via the IsCollection SPI") {

    test("NonEmptyList") {
      val team = CatsEachSpec.Nel(NonEmptyList.of("ann", "bob", "cid"))
      team.modify(_.xs.each).using(_.toUpperCase) ==> CatsEachSpec.Nel(NonEmptyList.of("ANN", "BOB", "CID"))
    }

    test("NonEmptyVector") {
      val team = CatsEachSpec.Nev(NonEmptyVector.of(1, 2, 3))
      team.modify(_.xs.each).using(_ + 10) ==> CatsEachSpec.Nev(NonEmptyVector.of(11, 12, 13))
    }

    test("NonEmptyChain") {
      val team = CatsEachSpec.Nec(NonEmptyChain.of("a", "b"))
      team.modify(_.xs.each).using(_ + "!") ==> CatsEachSpec.Nec(NonEmptyChain.of("a!", "b!"))
    }

    test("Chain") {
      val team = CatsEachSpec.Ch(Chain("x", "y", "z"))
      team.modify(_.xs.each).using(_.toUpperCase) ==> CatsEachSpec.Ch(Chain("X", "Y", "Z"))
    }

    test("field descent after `.each` over a NonEmptyList") {
      val roster = CatsEachSpec.Roster(NonEmptyList.of(CatsEachSpec.Player("ann", 1), CatsEachSpec.Player("bob", 2)))
      roster.modify(_.players.each.name).using(_.toUpperCase) ==>
        CatsEachSpec.Roster(NonEmptyList.of(CatsEachSpec.Player("ANN", 1), CatsEachSpec.Player("BOB", 2)))
    }

    test("`.eachWhere` over a NonEmptyList") {
      val team = CatsEachSpec.Nel(NonEmptyList.of("ann", "bob", "cid"))
      team.modify(_.xs.eachWhere(_.startsWith("b"))).using(_.toUpperCase) ==>
        CatsEachSpec.Nel(NonEmptyList.of("ann", "BOB", "cid"))
    }
  }
}

object CatsEachSpec {
  final case class Nel(xs: NonEmptyList[String])
  final case class Nev(xs: NonEmptyVector[Int])
  final case class Nec(xs: NonEmptyChain[String])
  final case class Ch(xs: Chain[String])

  final case class Player(name: String, number: Int)
  final case class Roster(players: NonEmptyList[Player])
}
