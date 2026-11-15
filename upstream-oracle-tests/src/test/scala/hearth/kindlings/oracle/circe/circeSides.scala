package hearth.kindlings.oracle.circe

import hearth.kindlings.circederivation.{Configuration, KindlingsCodecAsObject}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** The Kindlings side: every fixture derived on its own (no sibling implicits), with `config` (read at runtime). */
final class CiKindlingsSide(implicit config: Configuration) {
  val person: Codec.AsObject[CiPerson] = KindlingsCodecAsObject.derived[CiPerson]
  val defaults: Codec.AsObject[CiDefaults] = KindlingsCodecAsObject.derived[CiDefaults]
  val nested: Codec.AsObject[CiNested] = KindlingsCodecAsObject.derived[CiNested]
  val withId: Codec.AsObject[CiWithId] = KindlingsCodecAsObject.derived[CiWithId]
  val names: Codec.AsObject[CiNames] = KindlingsCodecAsObject.derived[CiNames]
  val emptyClass: Codec.AsObject[CiEmptyClass] = KindlingsCodecAsObject.derived[CiEmptyClass]
  val shape: Codec.AsObject[CiShape] = KindlingsCodecAsObject.derived[CiShape]
  val animal: Codec.AsObject[CiAnimal] = KindlingsCodecAsObject.derived[CiAnimal]
  val color: Codec.AsObject[CiColor] = KindlingsCodecAsObject.derived[CiColor]
  val tree: Codec.AsObject[CiTree] = KindlingsCodecAsObject.derived[CiTree]
  val expr: Codec.AsObject[CiExpr] = KindlingsCodecAsObject.derived[CiExpr]
  val holder: Codec.AsObject[CiHolder] = KindlingsCodecAsObject.derived[CiHolder]
  val media: Codec.AsObject[CiMedia] = KindlingsCodecAsObject.derived[CiMedia]
}

/** The upstream side: circe-generic-extras' `deriveConfiguredCodec` for every fixture (semi-automatic derivation, as
  * its docs say: an instance for each type, recursive ones as `implicit lazy val`s).
  */
final class CiExtrasSide(implicit config: ExtrasConfiguration) {
  val withId: Option[Codec[CiWithId]] = CiVersionSpecific.extras
  import io.circe.generic.extras.semiauto.deriveConfiguredCodec

  implicit lazy val person: Codec.AsObject[CiPerson] = deriveConfiguredCodec[CiPerson]
  implicit lazy val defaults: Codec.AsObject[CiDefaults] = deriveConfiguredCodec[CiDefaults]
  implicit lazy val nested: Codec.AsObject[CiNested] = deriveConfiguredCodec[CiNested]
  implicit lazy val names: Codec.AsObject[CiNames] = deriveConfiguredCodec[CiNames]
  implicit lazy val emptyClass: Codec.AsObject[CiEmptyClass] = deriveConfiguredCodec[CiEmptyClass]
  implicit lazy val circle: Codec.AsObject[CiCircle] = deriveConfiguredCodec[CiCircle]
  implicit lazy val rectangle: Codec.AsObject[CiRectangle] = deriveConfiguredCodec[CiRectangle]
  implicit lazy val noShape: Codec.AsObject[CiNoShape.type] = deriveConfiguredCodec[CiNoShape.type]
  implicit lazy val shape: Codec.AsObject[CiShape] = deriveConfiguredCodec[CiShape]
  implicit lazy val dog: Codec.AsObject[CiDog] = deriveConfiguredCodec[CiDog]
  implicit lazy val cat: Codec.AsObject[CiCat] = deriveConfiguredCodec[CiCat]
  implicit lazy val wolf: Codec.AsObject[CiWolf] = deriveConfiguredCodec[CiWolf]
  implicit lazy val unknownAnimal: Codec.AsObject[CiUnknownAnimal.type] = deriveConfiguredCodec[CiUnknownAnimal.type]
  implicit lazy val animal: Codec.AsObject[CiAnimal] = deriveConfiguredCodec[CiAnimal]
  implicit lazy val color: Codec.AsObject[CiColor] = deriveConfiguredCodec[CiColor]
  implicit lazy val tree: Codec.AsObject[CiTree] = deriveConfiguredCodec[CiTree]
  implicit lazy val lit: Codec.AsObject[CiLit] = deriveConfiguredCodec[CiLit]
  implicit lazy val add: Codec.AsObject[CiAdd] = deriveConfiguredCodec[CiAdd]
  implicit lazy val expr: Codec.AsObject[CiExpr] = deriveConfiguredCodec[CiExpr]
  implicit lazy val holder: Codec.AsObject[CiHolder] = deriveConfiguredCodec[CiHolder]
  implicit lazy val novel: Codec.AsObject[CiNovel] = deriveConfiguredCodec[CiNovel]
  implicit lazy val film: Codec.AsObject[CiFilm] = deriveConfiguredCodec[CiFilm]
  implicit lazy val media: Codec.AsObject[CiMedia] = deriveConfiguredCodec[CiMedia]
}

/** The upstream side without configuration: circe-generic's `deriveCodec` (shapeless on Scala 2, circe-core's
  * `Codec.AsObject.derived` on Scala 3).
  */
object CiGenericSide {
  val withId: Option[Codec[CiWithId]] = CiVersionSpecific.generic
  import io.circe.generic.semiauto.deriveCodec

  implicit lazy val person: Codec.AsObject[CiPerson] = deriveCodec[CiPerson]
  implicit lazy val defaults: Codec.AsObject[CiDefaults] = deriveCodec[CiDefaults]
  implicit lazy val nested: Codec.AsObject[CiNested] = deriveCodec[CiNested]
  implicit lazy val names: Codec.AsObject[CiNames] = deriveCodec[CiNames]
  implicit lazy val emptyClass: Codec.AsObject[CiEmptyClass] = deriveCodec[CiEmptyClass]
  implicit lazy val circle: Codec.AsObject[CiCircle] = deriveCodec[CiCircle]
  implicit lazy val rectangle: Codec.AsObject[CiRectangle] = deriveCodec[CiRectangle]
  implicit lazy val noShape: Codec.AsObject[CiNoShape.type] = deriveCodec[CiNoShape.type]
  implicit lazy val shape: Codec.AsObject[CiShape] = deriveCodec[CiShape]
  implicit lazy val dog: Codec.AsObject[CiDog] = deriveCodec[CiDog]
  implicit lazy val cat: Codec.AsObject[CiCat] = deriveCodec[CiCat]
  implicit lazy val wolf: Codec.AsObject[CiWolf] = deriveCodec[CiWolf]
  implicit lazy val unknownAnimal: Codec.AsObject[CiUnknownAnimal.type] = deriveCodec[CiUnknownAnimal.type]
  implicit lazy val animal: Codec.AsObject[CiAnimal] = deriveCodec[CiAnimal]
  implicit lazy val color: Codec.AsObject[CiColor] = deriveCodec[CiColor]
  implicit lazy val tree: Codec.AsObject[CiTree] = deriveCodec[CiTree]
  implicit lazy val lit: Codec.AsObject[CiLit] = deriveCodec[CiLit]
  implicit lazy val add: Codec.AsObject[CiAdd] = deriveCodec[CiAdd]
  implicit lazy val expr: Codec.AsObject[CiExpr] = deriveCodec[CiExpr]
  implicit lazy val holder: Codec.AsObject[CiHolder] = deriveCodec[CiHolder]
  implicit lazy val novel: Codec.AsObject[CiNovel] = deriveCodec[CiNovel]
  implicit lazy val film: Codec.AsObject[CiFilm] = deriveCodec[CiFilm]
  implicit lazy val media: Codec.AsObject[CiMedia] = deriveCodec[CiMedia]
}

/** Sample values and pairs of the fixtures. */
object CiSamples {
  val persons: List[CiPerson] = List(CiPerson("Ann", 30, Some("An")), CiPerson("Bob", 40, None))
  val defaults: List[CiDefaults] = List(CiDefaults("a"), CiDefaults("b", 1, Nil, None))
  val nested: List[CiNested] = List(
    CiNested(
      persons.head,
      persons,
      Map("x" -> persons.head),
      Map(1 -> 2L, -3 -> 4L),
      Some(Some(1)),
      (1, "one")
    ),
    CiNested(persons(1), Nil, Map.empty, Map.empty, Some(None), (0, "")),
    CiNested(persons(1), Nil, Map.empty, Map.empty, None, (0, ""))
  )
  val withIds: List[CiWithId] = List(CiWithId(CiUserId(1), List(CiUserId(2), CiUserId(3)), Some(CiUserId(4))))
  val names: List[CiNames] = List(CiNames(200, "u", x = true, already_snake = 1))
  val emptyClasses: List[CiEmptyClass] = List(CiEmptyClass())
  val shapes: List[CiShape] = List(CiCircle(1.5), CiRectangle(2.0, 3.0), CiNoShape)
  val animals: List[CiAnimal] = List(CiDog("Rex"), CiCat(9), CiWolf(5), CiUnknownAnimal)
  val colors: List[CiColor] = List(CiRed, CiGreen, CiDarkBlue)
  val trees: List[CiTree] = List(CiTree(1, List(CiTree(2, Nil), CiTree(3, List(CiTree(4, Nil))))))
  val exprs: List[CiExpr] = List(CiAdd(CiLit(1), CiAdd(CiLit(2), CiLit(3))))
  val medias: List[CiMedia] = List(CiNovel("Dune"), CiFilm(120))
  val holders: List[CiHolder] = List(CiHolder(CiCircle(1.0), animals, Some(CiDarkBlue)), CiHolder(CiNoShape, Nil, None))

  def pair[A](k: Codec[A], u: Codec[A]): CiPair[A] = CiPair(k, k, u, u)

  def pair[A](k: Encoder[A], kd: Decoder[A], u: Encoder[A], ud: Decoder[A]): CiPair[A] = CiPair(k, kd, u, ud)

  /** Every fixture compared between `k` and an upstream side. */
  def all(label: String, k: CiKindlingsSide, u: UpstreamCodecs, wrapperInputs: List[String]): List[CiDiff] = {
    import CirceOracle.compare
    compare(s"$label CiPerson", pair(k.person, u.person), persons) ++
      compare(s"$label CiDefaults", pair(k.defaults, u.defaults), defaults) ++
      compare(s"$label CiNested", pair(k.nested, u.nested), nested) ++
      u.withId.toList.flatMap(w => compare(s"$label CiWithId", pair(k.withId, w), withIds)) ++
      compare(s"$label CiNames", pair(k.names, u.names), names) ++
      compare(s"$label CiEmptyClass", pair(k.emptyClass, u.emptyClass), emptyClasses) ++
      compare(s"$label CiShape", pair(k.shape, u.shape), shapes, wrapperInputs) ++
      compare(s"$label CiAnimal", pair(k.animal, u.animal), animals) ++
      compare(s"$label CiColor", pair(k.color, u.color), colors) ++
      compare(s"$label CiTree", pair(k.tree, u.tree), trees) ++
      compare(s"$label CiExpr", pair(k.expr, u.expr), exprs) ++
      compare(s"$label CiHolder", pair(k.holder, u.holder), holders) ++
      compare(s"$label CiMedia", pair(k.media, u.media), medias)
  }

  /** The upstream codecs `all` compares with (structural, so both upstream sides fit). */
  final case class UpstreamCodecs(
      person: Codec[CiPerson],
      defaults: Codec[CiDefaults],
      nested: Codec[CiNested],
      withId: Option[Codec[CiWithId]], // None on Scala 3: upstream does not derive value classes
      names: Codec[CiNames],
      emptyClass: Codec[CiEmptyClass],
      shape: Codec[CiShape],
      animal: Codec[CiAnimal],
      color: Codec[CiColor],
      tree: Codec[CiTree],
      expr: Codec[CiExpr],
      holder: Codec[CiHolder],
      media: Codec[CiMedia]
  )

  def extras(u: CiExtrasSide): UpstreamCodecs =
    UpstreamCodecs(
      u.person,
      u.defaults,
      u.nested,
      u.withId,
      u.names,
      u.emptyClass,
      u.shape,
      u.animal,
      u.color,
      u.tree,
      u.expr,
      u.holder,
      u.media
    )

  val generic: UpstreamCodecs = {
    val u = CiGenericSide
    UpstreamCodecs(
      u.person,
      u.defaults,
      u.nested,
      u.withId,
      u.names,
      u.emptyClass,
      u.shape,
      u.animal,
      u.color,
      u.tree,
      u.expr,
      u.holder,
      u.media
    )
  }

  /** Wrapper-object (no discriminator) edge cases of `CiShape`. */
  val wrapperInputs: List[String] = List(
    """{"CiCircle":{"radius":1.0},"CiRectangle":{"widthCm":1.0,"heightCm":2.0}}""",
    """{"CiRectangle":{"widthCm":1.0,"heightCm":2.0},"CiCircle":{"radius":1.0}}""",
    """{"extra":true,"CiCircle":{"radius":1.0}}""",
    """{"CiCircle":{"radius":1.0},"extra":true}""",
    """{"CiTriangle":{}}""",
    """{"CiNoShape":{"extra":1}}""",
    """{"CiNoShape":null}""",
    """{"CiNoShape":[]}"""
  )

  /** Discriminator (`type`) edge cases of `CiShape`. */
  def discriminatorInputs(circle: String, noShape: String): List[String] = List(
    s"""{"type":"$circle","radius":1.0}""",
    s"""{"radius":1.0}""",
    s"""{"type":null,"radius":1.0}""",
    s"""{"type":1,"radius":1.0}""",
    s"""{"type":"CiTriangle"}""",
    s"""{"type":"$noShape","extra":1}""",
    s"""{"radius":1.0,"type":"$circle","extra":true}"""
  )
}
