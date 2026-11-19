package hearth.kindlings.oracle.circe

import io.circe.{Codec, Decoder, Encoder}

// Scala 3 fixtures mirroring circe-core's own `DerivesSuite`, `ConfiguredDerivesSuite` and
// `ConfiguredEnumDerivesSuites` (prefixed `Cu`, see circeUpstreamSuiteFixtures.scala).

enum CuVegetable {
  case Potato(species: String)
  case Carrot(length: Double)
  case Onion(layers: Int)
  case Turnip
}

enum CuRecursiveEnumAdt {
  case BaseAdtExample(a: String)
  case NestedAdtExample(r: CuRecursiveEnumAdt)
}

/** circe/circe#2135: a member of an intersection ("tagged") type, with its instance in the companion. */
final case class CuProductWithTaggedMember(x: CuProductWithTaggedMember.TaggedString)
object CuProductWithTaggedMember {
  sealed trait Tag
  type TaggedString = String & Tag
  given Codec[TaggedString] =
    Codec.from(summon[Decoder[String]].map(_.asInstanceOf[TaggedString]), summon[Encoder[String]].contramap(x => x))
  def fromUntagged(x: String): CuProductWithTaggedMember = CuProductWithTaggedMember(x.asInstanceOf[TaggedString])
}

enum CuLongEnum {
  case v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24,
    v25, v26, v27, v28, v29, v30, v31, v32, v33
}

enum CuLongSum {
  case v1(str: String)
  case v2(str: String)
  case v3(str: String)
  case v4(str: String)
  case v5(str: String)
  case v6(str: String)
  case v7(str: String)
  case v8(str: String)
  case v9(str: String)
  case v10(str: String)
  case v11(str: String)
  case v12(str: String)
  case v13(str: String)
  case v14(str: String)
  case v15(str: String)
  case v16(str: String)
  case v17(str: String)
  case v18(str: String)
  case v19(str: String)
  case v20(str: String)
  case v21(str: String)
  case v22(str: String)
  case v23(str: String)
  case v24(str: String)
  case v25(str: String)
  case v26(str: String)
  case v27(str: String)
  case v28(str: String)
  case v29(str: String)
  case v30(str: String)
  case v31(str: String)
  case v32(str: String)
  case v33(str: String)
}

/** A hierarchy of three levels where a sibling of the intermediate has a field named like a leaf (`CuChild`). */
sealed trait CuGreatGrandParent
sealed trait CuGrandParent extends CuGreatGrandParent
final case class CuUncle(CuChild: Int) extends CuGrandParent
sealed trait CuParent extends CuGrandParent
final case class CuChild(a: Int, b: String) extends CuParent

/** An enumeration with nested sealed traits and a diamond (`D`). */
sealed trait CuHierarchicalEnum
object CuHierarchicalEnum {
  sealed trait NestedA extends CuHierarchicalEnum
  sealed trait NestedB extends CuHierarchicalEnum
  case object A extends CuHierarchicalEnum
  case object B extends NestedA
  case object C extends NestedB
  case object D extends NestedA, NestedB
}

enum CuIntercardinalDirections {
  case NorthEast, SouthEast, SouthWest, NorthWest
}
