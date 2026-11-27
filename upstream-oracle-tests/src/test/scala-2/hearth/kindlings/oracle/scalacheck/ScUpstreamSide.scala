package hearth.kindlings.oracle.scalacheck

import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.derive.Recursive

/** scalacheck-shapeless (Scala 2): `import org.scalacheck.ScalacheckShapeless._` and implicit resolution, with the
  * `Recursive` marker its docs require for recursive types (falling back to a leaf when the size runs out).
  */
object ScUpstreamSide extends ScInstances {
  import org.scalacheck.ScalacheckShapeless.*

  implicit val recursiveTree: Recursive[ScTree] = Recursive[ScTree](Gen.const(ScLeaf(0)))

  val arbPerson: Arbitrary[ScPerson] = implicitly[Arbitrary[ScPerson]]
  val arbShape: Arbitrary[ScShape] = implicitly[Arbitrary[ScShape]]
  val arbTree: Arbitrary[ScTree] = implicitly[Arbitrary[ScTree]]
  val arbWrapper: Arbitrary[ScWrapper] = implicitly[Arbitrary[ScWrapper]]
  val arbContainers: Arbitrary[ScContainers] = implicitly[Arbitrary[ScContainers]]
  val shrinkPerson: Shrink[ScPerson] = implicitly[Shrink[ScPerson]]
  val shrinkShape: Shrink[ScShape] = implicitly[Shrink[ScShape]]
  val shrinkTree: Shrink[ScTree] = implicitly[Shrink[ScTree]]
  val shrinkWrapper: Shrink[ScWrapper] = implicitly[Shrink[ScWrapper]]
  val cogenPerson: Cogen[ScPerson] = implicitly[Cogen[ScPerson]]
  val cogenShape: Cogen[ScShape] = implicitly[Cogen[ScShape]]
  val cogenTree: Cogen[ScTree] = implicitly[Cogen[ScTree]]
}
