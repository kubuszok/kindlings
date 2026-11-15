package hearth.kindlings.oracle.optics

import hearth.MacroSuite

/** Kindlings' optics vs SoftwareMill quicklens: the same `modify` paths and terminal operations, applied to the same
  * values, compared on the results and on the exceptions thrown (e.g. `.at` on a missing index).
  *
  * The cases of both objects below are textually identical (only the import differs: both libraries use the same DSL
  * names). Each test lists the *exact* differences it expects; each one must be documented (see
  * `docs/user-guide/feature-parity.md`). A difference that appears or disappears fails the test.
  *
  * Generated from a single list of cases: keep both copies identical.
  */
final class OpticsOracleSpec extends MacroSuite {

  private object Kindlings {
    import hearth.kindlings.optics.*
    import OpFixtures.outcome

    def person(p: OpPerson): List[(String, Either[String, Any])] = List(
      "field: using" -> outcome(p.modify(_.name).using(_.toUpperCase)),
      "field: apply" -> outcome(p.modify(_.age)(_ + 1)),
      "nested field: setTo" -> outcome(p.modify(_.address.street.name).setTo("high")),
      "setToIfDefined(Some)" -> outcome(p.modify(_.address.city).setToIfDefined(Some("rome"))),
      "setToIfDefined(None)" -> outcome(p.modify(_.address.city).setToIfDefined(None)),
      "setToIf(true)" -> outcome(p.modify(_.age).setToIf(true)(1)),
      "setToIf(false)" -> outcome(p.modify(_.age).setToIf(false)(1)),
      "usingIf(true)" -> outcome(p.modify(_.age).usingIf(true)(_ * 2)),
      "usingIf(false)" -> outcome(p.modify(_.age).usingIf(false)(_ * 2)),
      "List.each" -> outcome(p.modify(_.nicknames.each).using(_.toUpperCase)),
      "List.eachWhere" -> outcome(p.modify(_.nicknames.eachWhere(_.startsWith("a"))).using(_.toUpperCase)),
      "Seq.each.field" -> outcome(p.modify(_.seq.each.number).using(_ * 10)),
      "Vector.at(1)" -> outcome(p.modify(_.scores.at(1)).using(_ + 1)),
      "Vector.at(5) (missing)" -> outcome(p.modify(_.scores.at(5)).using(_ + 1)),
      "Vector.at(-1) (missing)" -> outcome(p.modify(_.scores.at(-1)).using(_ + 1)),
      "Vector.index(1)" -> outcome(p.modify(_.scores.index(1)).using(_ + 1)),
      "Vector.index(-1) (missing)" -> outcome(p.modify(_.scores.index(-1)).using(_ + 1)),
      "Vector.index(5) (missing)" -> outcome(p.modify(_.scores.index(5)).using(_ + 1)),
      "Vector.atOrElse(1, 0)" -> outcome(p.modify(_.scores.atOrElse(1, 0)).using(_ + 1)),
      "Vector.atOrElse(5, 0) (missing)" -> outcome(p.modify(_.scores.atOrElse(5, 0)).using(_ + 1)),
      "Seq.at(1).field" -> outcome(p.modify(_.seq.at(1).name).setTo("z")),
      "List.at(3) (missing)" -> outcome(p.modify(_.nicknames.at(3)).setTo("z")),
      "Map.each" -> outcome(p.modify(_.tags.each).using(_ * 10)),
      "Map.at(key)" -> outcome(p.modify(_.tags.at("a")).using(_ + 1)),
      "Map.at(missing)" -> outcome(p.modify(_.tags.at("z")).using(_ + 1)),
      "Map.index(key)" -> outcome(p.modify(_.tags.index("a")).using(_ + 1)),
      "Map.index(missing)" -> outcome(p.modify(_.tags.index("z")).using(_ + 1)),
      "Map.atOrElse(key, 0)" -> outcome(p.modify(_.tags.atOrElse("a", 0)).using(_ + 1)),
      "Map.atOrElse(missing, 0)" -> outcome(p.modify(_.tags.atOrElse("z", 0)).using(_ + 1)),
      "Option.each.field" -> outcome(p.modify(_.maybeAddress.each.city).using(_.toUpperCase)),
      "Option.at.field" -> outcome(p.modify(_.maybeAddress.at.city).using(_.toUpperCase)),
      "Option.index.field" -> outcome(p.modify(_.maybeAddress.index.city).using(_.toUpperCase)),
      "Option.atOrElse" -> outcome(
        p.modify(_.maybeAddress.atOrElse(OpAddress(OpStreet("new", 0), "new"))).using(a => a.copy(city = a.city + "!"))
      ),
      "Either.eachRight" -> outcome(p.modify(_.result.eachRight).using(_ + 1)),
      "Either.eachLeft" -> outcome(p.modify(_.result.eachLeft).using(_.toUpperCase)),
      "modifyAll" -> outcome(p.modifyAll(_.name, _.address.city, _.address.street.name).using(_.toUpperCase)),
      "modifyAll with each" -> outcome(p.modifyAll(_.nicknames.each, _.name).using(_.reverse)),
      "chained modify" -> outcome(p.modify(_.age).setTo(1).modify(_.name).setTo("bob")),
      "modifyLens" -> outcome(modifyLens[OpPerson](_.address.city).using(_.toUpperCase)(p)),
      "modifyLens.setToIf(false)" -> outcome(modifyLens[OpPerson](_.address.city).setToIf(false)("x")(p)),
      "modifyAllLens" -> outcome(modifyAllLens[OpPerson](_.name, _.address.city).setTo("x")(p)),
      "andThenModify" -> outcome(
        modifyLens[OpPerson](_.address).andThenModify(modifyLens[OpAddress](_.street.number)).using(_ + 1)(p)
      )
    )

    def zoo(z: OpZoo): List[(String, Either[String, Any])] = List(
      "when[Dog].field" -> outcome(z.modify(_.main.when[OpDog].age).using(_ + 1)),
      "when[Cat].field" -> outcome(z.modify(_.main.when[OpCat].indoor).setTo(true)),
      "each.when[Cat].field" -> outcome(z.modify(_.animals.each.when[OpCat].name).using(_.toUpperCase)),
      "sealed common field" -> outcome(z.modify(_.main.name).using(_.toUpperCase)),
      "each.sealed common field" -> outcome(z.modify(_.animals.each.name).using(_.toUpperCase)),
      "Option.each.nested" -> outcome(z.modify(_.keeper.each.address.street.number).setTo(9))
    )
  }

  private object Quicklens {
    import com.softwaremill.quicklens.*
    import OpFixtures.outcome

    def person(p: OpPerson): List[(String, Either[String, Any])] = List(
      "field: using" -> outcome(p.modify(_.name).using(_.toUpperCase)),
      "field: apply" -> outcome(p.modify(_.age)(_ + 1)),
      "nested field: setTo" -> outcome(p.modify(_.address.street.name).setTo("high")),
      "setToIfDefined(Some)" -> outcome(p.modify(_.address.city).setToIfDefined(Some("rome"))),
      "setToIfDefined(None)" -> outcome(p.modify(_.address.city).setToIfDefined(None)),
      "setToIf(true)" -> outcome(p.modify(_.age).setToIf(true)(1)),
      "setToIf(false)" -> outcome(p.modify(_.age).setToIf(false)(1)),
      "usingIf(true)" -> outcome(p.modify(_.age).usingIf(true)(_ * 2)),
      "usingIf(false)" -> outcome(p.modify(_.age).usingIf(false)(_ * 2)),
      "List.each" -> outcome(p.modify(_.nicknames.each).using(_.toUpperCase)),
      "List.eachWhere" -> outcome(p.modify(_.nicknames.eachWhere(_.startsWith("a"))).using(_.toUpperCase)),
      "Seq.each.field" -> outcome(p.modify(_.seq.each.number).using(_ * 10)),
      "Vector.at(1)" -> outcome(p.modify(_.scores.at(1)).using(_ + 1)),
      "Vector.at(5) (missing)" -> outcome(p.modify(_.scores.at(5)).using(_ + 1)),
      "Vector.at(-1) (missing)" -> outcome(p.modify(_.scores.at(-1)).using(_ + 1)),
      "Vector.index(1)" -> outcome(p.modify(_.scores.index(1)).using(_ + 1)),
      "Vector.index(-1) (missing)" -> outcome(p.modify(_.scores.index(-1)).using(_ + 1)),
      "Vector.index(5) (missing)" -> outcome(p.modify(_.scores.index(5)).using(_ + 1)),
      "Vector.atOrElse(1, 0)" -> outcome(p.modify(_.scores.atOrElse(1, 0)).using(_ + 1)),
      "Vector.atOrElse(5, 0) (missing)" -> outcome(p.modify(_.scores.atOrElse(5, 0)).using(_ + 1)),
      "Seq.at(1).field" -> outcome(p.modify(_.seq.at(1).name).setTo("z")),
      "List.at(3) (missing)" -> outcome(p.modify(_.nicknames.at(3)).setTo("z")),
      "Map.each" -> outcome(p.modify(_.tags.each).using(_ * 10)),
      "Map.at(key)" -> outcome(p.modify(_.tags.at("a")).using(_ + 1)),
      "Map.at(missing)" -> outcome(p.modify(_.tags.at("z")).using(_ + 1)),
      "Map.index(key)" -> outcome(p.modify(_.tags.index("a")).using(_ + 1)),
      "Map.index(missing)" -> outcome(p.modify(_.tags.index("z")).using(_ + 1)),
      "Map.atOrElse(key, 0)" -> outcome(p.modify(_.tags.atOrElse("a", 0)).using(_ + 1)),
      "Map.atOrElse(missing, 0)" -> outcome(p.modify(_.tags.atOrElse("z", 0)).using(_ + 1)),
      "Option.each.field" -> outcome(p.modify(_.maybeAddress.each.city).using(_.toUpperCase)),
      "Option.at.field" -> outcome(p.modify(_.maybeAddress.at.city).using(_.toUpperCase)),
      "Option.index.field" -> outcome(p.modify(_.maybeAddress.index.city).using(_.toUpperCase)),
      "Option.atOrElse" -> outcome(
        p.modify(_.maybeAddress.atOrElse(OpAddress(OpStreet("new", 0), "new"))).using(a => a.copy(city = a.city + "!"))
      ),
      "Either.eachRight" -> outcome(p.modify(_.result.eachRight).using(_ + 1)),
      "Either.eachLeft" -> outcome(p.modify(_.result.eachLeft).using(_.toUpperCase)),
      "modifyAll" -> outcome(p.modifyAll(_.name, _.address.city, _.address.street.name).using(_.toUpperCase)),
      "modifyAll with each" -> outcome(p.modifyAll(_.nicknames.each, _.name).using(_.reverse)),
      "chained modify" -> outcome(p.modify(_.age).setTo(1).modify(_.name).setTo("bob")),
      "modifyLens" -> outcome(modifyLens[OpPerson](_.address.city).using(_.toUpperCase)(p)),
      "modifyLens.setToIf(false)" -> outcome(modifyLens[OpPerson](_.address.city).setToIf(false)("x")(p)),
      "modifyAllLens" -> outcome(modifyAllLens[OpPerson](_.name, _.address.city).setTo("x")(p)),
      "andThenModify" -> outcome(
        modifyLens[OpPerson](_.address).andThenModify(modifyLens[OpAddress](_.street.number)).using(_ + 1)(p)
      )
    )

    def zoo(z: OpZoo): List[(String, Either[String, Any])] = List(
      "when[Dog].field" -> outcome(z.modify(_.main.when[OpDog].age).using(_ + 1)),
      "when[Cat].field" -> outcome(z.modify(_.main.when[OpCat].indoor).setTo(true)),
      "each.when[Cat].field" -> outcome(z.modify(_.animals.each.when[OpCat].name).using(_.toUpperCase)),
      "sealed common field" -> outcome(z.modify(_.main.name).using(_.toUpperCase)),
      "each.sealed common field" -> outcome(z.modify(_.animals.each.name).using(_.toUpperCase)),
      "Option.each.nested" -> outcome(z.modify(_.keeper.each.address.street.number).setTo(9))
    )
  }

  /** Compares both libraries case by case; `expectedDifferences` are the names of the cases expected to differ. */
  private def compare(
      kindlings: List[(String, Either[String, Any])],
      quicklens: List[(String, Either[String, Any])]
  )(expectedDifferences: String*): Unit = {
    val differences = kindlings.zip(quicklens).collect { case ((name, k), (_, q)) if k != q => (name, k, q) }
    assertEquals(
      differences.map(_._1),
      expectedDifferences.toList,
      differences
        .map { case (n, k, q) => s"$n: kindlings=$k quicklens=$q" }
        .mkString("Kindlings vs quicklens differences:\n", "\n", "")
    )
  }

  // Documented difference (feature-parity.md, optics): `.index(i)` with a negative index is a no-op, as in quicklens
  // for Scala 3; quicklens for Scala 2 throws `IndexOutOfBoundsException` (it only checks `i < size`).
  private val negativeIndex: List[String] =
    if (QuicklensVersion.negativeIndexThrows) List("Vector.index(-1) (missing)") else Nil

  group("optics oracle (quicklens)") {

    test("paths over a case class, collections, maps, options and eithers") {
      compare(Kindlings.person(OpFixtures.person), Quicklens.person(OpFixtures.person))(negativeIndex*)
    }

    test("the same paths when options are empty, eithers are Left and maps are empty") {
      compare(Kindlings.person(OpFixtures.personWithoutOptions), Quicklens.person(OpFixtures.personWithoutOptions))(
        negativeIndex*
      )
    }

    test("sealed hierarchies: when[Subtype] and common fields") {
      compare(Kindlings.zoo(OpFixtures.zoo), Quicklens.zoo(OpFixtures.zoo))()
      compare(Kindlings.zoo(OpFixtures.catZoo), Quicklens.zoo(OpFixtures.catZoo))()
    }
  }
}
