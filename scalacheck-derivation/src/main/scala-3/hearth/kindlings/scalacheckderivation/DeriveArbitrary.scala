package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary => ScalaCheckArbitrary}

object DeriveArbitrary {

  /** Derives an Arbitrary instance for type A.
    *
    * For complete, compilable examples, see:
    * `scalacheck-derivation/src/test/scala/.../scalacheckderivation/examples.scala`
    *
    * Supports:
    *   - Case classes (generates random values for each field)
    *   - Sealed traits / enums (randomly selects a subtype)
    *   - Collections and Option types
    *   - Recursive types (no lazy wrappers needed)
    *
    * ==Basic Usage==
    * {{{
    * import org.scalacheck.Arbitrary
    * import hearth.kindlings.scalacheckderivation.DeriveArbitrary
    * import org.scalacheck.Prop.forAll
    *
    * case class Person(name: String, age: Int)
    * implicit val arbPerson: Arbitrary[Person] = DeriveArbitrary.derived[Person]
    *
    * // Use in ScalaCheck properties
    * property("age is always non-negative after normalization") {
    *   forAll { (p: Person) =>
    *     val normalized = p.copy(age = p.age.abs)
    *     normalized.age >= 0
    *   }
    * }
    * }}}
    *
    * ==Nested Types==
    * {{{
    * case class Address(city: String, country: String)
    * case class Person(name: String, address: Address)
    *
    * implicit val arbAddress: Arbitrary[Address] =
    *   DeriveArbitrary.derived
    * implicit val arbPerson: Arbitrary[Person] =
    *   DeriveArbitrary.derived
    * }}}
    *
    * ==Sealed Traits==
    * {{{
    * sealed trait Shape
    * case class Circle(radius: Double) extends Shape
    * case class Rectangle(width: Double, height: Double) extends Shape
    *
    * implicit val arbCircle: Arbitrary[Circle] =
    *   DeriveArbitrary.derived
    * implicit val arbRectangle: Arbitrary[Rectangle] =
    *   DeriveArbitrary.derived
    * implicit val arbShape: Arbitrary[Shape] =
    *   DeriveArbitrary.derived
    * }}}
    *
    * ==Recursive Types==
    * {{{
    * case class Tree(value: Int, children: List[Tree])
    *
    * implicit val arbTree: Arbitrary[Tree] =
    *   DeriveArbitrary.derived
    * // No Lazy wrapper needed - recursion is handled automatically
    * }}}
    *
    * @tparam A
    *   the type for which to derive an Arbitrary instance
    * @return
    *   an Arbitrary[A] instance
    */
  inline def derived[A]: ScalaCheckArbitrary[A] = ${ internal.compiletime.ArbitraryMacros.deriveArbitrary[A] }
}
