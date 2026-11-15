package hearth.kindlings.oracle.scalacheck

import io.github.martinhh.derived.RecursionFallback
import org.scalacheck.{Arbitrary, Cogen, Shrink}

/** scalacheck-derived (Scala 3): `deriveArbitrary` / `deriveShrink` / `deriveCogen`, with the `RecursionFallback` its
  * docs recommend for recursive types (falling back to a leaf when the size runs out).
  */
object ScUpstreamSide extends ScInstances {
  import io.github.martinhh.derived.scalacheck.{deriveArbitrary, deriveCogen}
  import io.github.martinhh.derived.shrink.deriveShrink

  given RecursionFallback[ScTree] = RecursionFallback[ScTree](ScLeaf(0))

  val arbPerson: Arbitrary[ScPerson] = deriveArbitrary[ScPerson]
  val arbShape: Arbitrary[ScShape] = deriveArbitrary[ScShape]
  val arbTree: Arbitrary[ScTree] = deriveArbitrary[ScTree]
  val arbWrapper: Arbitrary[ScWrapper] = deriveArbitrary[ScWrapper]
  val shrinkPerson: Shrink[ScPerson] = deriveShrink[ScPerson]
  val shrinkShape: Shrink[ScShape] = deriveShrink[ScShape]
  val shrinkTree: Shrink[ScTree] = deriveShrink[ScTree]
  val shrinkWrapper: Shrink[ScWrapper] = deriveShrink[ScWrapper]
  val cogenPerson: Cogen[ScPerson] = deriveCogen[ScPerson]
  val cogenShape: Cogen[ScShape] = deriveCogen[ScShape]
  val cogenTree: Cogen[ScTree] = deriveCogen[ScTree]
}
