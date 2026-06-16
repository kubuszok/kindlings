package hearth.kindlings.optics

import hearth.MacroSuite

final class LensCompositionSpec extends MacroSuite {

  import LensCompositionSpec.*
  // `modifyLens`/`modifyAllLens` resolve via the `optics` package object (which extends `OpticsSyntax`), so no
  // `syntax.*` import is needed here.

  group("modifyLens — reusable, object-independent lens") {

    test("setTo yields a reusable T => T applied to several objects") {
      val rename = modifyLens[Person](_.name).setTo("anon")
      rename(Person("ann", 1)) ==> Person("anon", 1)
      rename(Person("bob", 2)) ==> Person("anon", 2)
    }

    test("using transforms through a nested path") {
      val upperCity = modifyLens[Person2](_.address.city).using(_.toUpperCase)
      upperCity(Person2("ann", Address("oldcity", "street"))) ==>
        Person2("ann", Address("OLDCITY", "street"))
    }

    test("terminal ops mirror PathModify") {
      val lens = modifyLens[Person](_.age)
      lens.setToIf(true)(99)(Person("ann", 1)) ==> Person("ann", 99)
      lens.setToIf(false)(99)(Person("ann", 1)) ==> Person("ann", 1)
      lens.usingIf(true)(_ + 1)(Person("ann", 1)) ==> Person("ann", 2)
      lens.setToIfDefined(Some(7))(Person("ann", 1)) ==> Person("ann", 7)
      lens.setToIfDefined(None)(Person("ann", 1)) ==> Person("ann", 1)
    }
  }

  group("modifyAllLens — reusable multi-path lens") {

    test("setTo over sibling fields") {
      val blank = modifyAllLens[Names](_.first, _.second).setTo("x")
      blank(Names("a", "b", "c")) ==> Names("x", "x", "c")
    }

    test("shared prefix in a reusable lens") {
      val up = modifyAllLens[Outer](_.inner.left, _.inner.right).using(_.toUpperCase)
      up(Outer(Inner("p", "q"), "t")) ==> Outer(Inner("P", "Q"), "t")
    }
  }

  group("andThenModify — lens composition") {

    test("compose two lenses and apply") {
      val toAddress: PathLazyModify[Person2, Address] = modifyLens[Person2](_.address)
      val toCity: PathLazyModify[Address, String] = modifyLens[Address](_.city)
      val toPersonCity: PathLazyModify[Person2, String] = toAddress.andThenModify(toCity)
      toPersonCity.setTo("NewCity")(Person2("ann", Address("old", "street"))) ==>
        Person2("ann", Address("NewCity", "street"))
      toPersonCity.using(_.toUpperCase)(Person2("ann", Address("old", "street"))) ==>
        Person2("ann", Address("OLD", "street"))
    }
  }
}

object LensCompositionSpec {

  final case class Person(name: String, age: Int)

  final case class Address(city: String, street: String)
  final case class Person2(name: String, address: Address)

  final case class Inner(left: String, right: String)
  final case class Outer(inner: Inner, top: String)

  final case class Names(first: String, second: String, third: String)
}
