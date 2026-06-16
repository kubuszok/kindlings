package hearth.kindlings.optics

import hearth.MacroSuite

final class ModifyFieldSpec extends MacroSuite {

  import ModifyFieldSpec._
  import hearth.kindlings.optics.syntax._

  group("modify single field") {

    test("setTo replaces the field") {
      Person("a", 1).modify(_.name).setTo("b") ==> Person("b", 1)
    }

    test("using transforms the field") {
      Person("a", 1).modify(_.name).using(_ + "!") ==> Person("a!", 1)
    }

    test("apply is an alias of using") {
      Person("a", 1).modify(_.age).apply(_ + 10) ==> Person("a", 11)
    }
  }

  group("modify nested field") {

    test("two-level setTo") {
      val p = Person2("a", Address("OldCity", "Street"))
      p.modify(_.address.city).setTo("NewCity") ==> Person2("a", Address("NewCity", "Street"))
    }

    test("two-level using leaves siblings untouched") {
      val p = Person2("a", Address("OldCity", "Street"))
      p.modify(_.address.city).using(_.toUpperCase) ==> Person2("a", Address("OLDCITY", "Street"))
    }

    test("three-level setTo") {
      val abc = A(B(C(1)))
      abc.modify(_.b.c.value).setTo(42) ==> A(B(C(42)))
    }

    test("three-level using") {
      val abc = A(B(C(1)))
      abc.modify(_.b.c.value).using(_ + 100) ==> A(B(C(101)))
    }
  }

  group("terminal operations") {

    test("usingIf true applies, false is a no-op") {
      Person("a", 1).modify(_.name).usingIf(true)(_ + "!") ==> Person("a!", 1)
      Person("a", 1).modify(_.name).usingIf(false)(_ + "!") ==> Person("a", 1)
    }

    test("setToIf true sets, false is a no-op") {
      Person("a", 1).modify(_.age).setToIf(true)(99) ==> Person("a", 99)
      Person("a", 1).modify(_.age).setToIf(false)(99) ==> Person("a", 1)
    }

    test("setToIfDefined Some sets, None is a no-op") {
      Person("a", 1).modify(_.name).setToIfDefined(Some("z")) ==> Person("z", 1)
      Person("a", 1).modify(_.name).setToIfDefined(None) ==> Person("a", 1)
    }
  }

}

object ModifyFieldSpec {

  final case class Person(name: String, age: Int)

  final case class Address(city: String, street: String)
  final case class Person2(name: String, address: Address)

  final case class C(value: Int)
  final case class B(c: C)
  final case class A(b: B)
}
