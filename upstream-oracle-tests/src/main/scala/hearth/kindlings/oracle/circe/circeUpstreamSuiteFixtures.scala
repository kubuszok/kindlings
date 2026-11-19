package hearth.kindlings.oracle.circe

import io.circe.{Decoder, Encoder, Json}

// Fixtures mirroring the ones of upstream's own derivation test suites (prefixed `Cu`), so that the oracle runs the
// cases upstream tests itself with:
//  - circe-generic-extras `ConfiguredSemiautoDerivedSuite` / `ConfiguredAutoDerivedSuite` and circe-core (Scala 3)
//    `ConfiguredDerivesSuite`: `ConfigExampleBase`, `FooWithDefault`, `FooNoDefault`, `GenericFoo`, `pascalExampleFoo`
//  - circe-generic `SemiautoDerivedSuite` / circe-core `DerivesSuite` and their `examples`: `Box`, `Qux`, `Wub`, `Foo`
//    (children with hand-written instances, one of them not a JSON object), `RecursiveAdtExample`,
//    `RecursiveWithOptionExample`, `LongClass`
//  - circe-core `SemiautoDerivationSuite`: `Adt3` (an empty case class next to a case object), `Adt4` (sub-traits)
// They live in the main sources for the reason given in circeFixtures.scala.

sealed trait CuConfigExampleBase
final case class CuConfigExampleFoo(thisIsAField: String, a: Int = 0, b: Double) extends CuConfigExampleBase
case object CuConfigExampleBar extends CuConfigExampleBase

final case class CuFooWithDefault(a: Option[Int] = Some(0), b: String = "b")
final case class CuFooNoDefault(a: Option[Int], b: String = "b")
final case class CuGenericFoo[T](a: List[T] = List.empty[T], b: String = "b")

/** Constructor names starting with a lower-case letter (`withPascalCaseConstructorNames`). */
sealed trait CuPascalExampleBase
final case class cuPascalExampleFoo(thisIsAField: String, a: Int = 0, b: Double) extends CuPascalExampleBase
case object cuPascalExampleBar extends CuPascalExampleBase

final case class CuBox[A](a: A)
final case class CuQux[A](i: Int, a: A, j: Int)
final case class CuWub(x: Long)

sealed trait CuFoo
final case class CuBar(i: Int, s: String) extends CuFoo
final case class CuBaz(xs: List[String]) extends CuFoo
final case class CuBam(w: CuWub, d: Double) extends CuFoo
object CuBar {
  implicit val decodeBar: Decoder[CuBar] = Decoder.forProduct2("i", "s")(CuBar.apply)
  implicit val encodeBar: Encoder[CuBar] = Encoder.forProduct2("i", "s") { case CuBar(i, s) => (i, s) }
}
object CuBaz {
  implicit val decodeBaz: Decoder[CuBaz] = Decoder[List[String]].map(CuBaz(_))
  implicit val encodeBaz: Encoder[CuBaz] = Encoder.instance { case CuBaz(xs) =>
    Json.fromValues(xs.map(Json.fromString))
  }
}

sealed trait CuRecursiveAdtExample
final case class CuBaseAdtExample(a: String) extends CuRecursiveAdtExample
final case class CuNestedAdtExample(r: CuRecursiveAdtExample) extends CuRecursiveAdtExample

final case class CuRecursiveWithOptionExample(o: Option[CuRecursiveWithOptionExample])

sealed trait CuAdt3
final case class CuAdt3Class1() extends CuAdt3
case object CuAdt3Object1 extends CuAdt3

sealed trait CuAdt4
sealed trait CuAdt4SubTrait1 extends CuAdt4
final case class CuAdt4Class1() extends CuAdt4SubTrait1
sealed trait CuAdt4SubTrait2 extends CuAdt4
case object CuAdt4Object1 extends CuAdt4SubTrait2

/** More fields than a tuple / `Function22` can hold. */
final case class CuLongClass(
    v1: String,
    v2: String,
    v3: String,
    v4: String,
    v5: String,
    v6: String,
    v7: String,
    v8: String,
    v9: String,
    v10: String,
    v11: String,
    v12: String,
    v13: String,
    v14: String,
    v15: String,
    v16: String,
    v17: String,
    v18: String,
    v19: String,
    v20: String,
    v21: String,
    v22: String,
    v23: String,
    v24: String,
    v25: String,
    v26: String,
    v27: String,
    v28: String,
    v29: String,
    v30: String,
    v31: String,
    v32: String,
    v33: String
)
