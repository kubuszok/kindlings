package hearth.kindlings.oracle.pureconfig

// Fixtures mirroring upstream's own suites (pureconfig `modules/generic` and `modules/generic-scala3`: ProductHintSuite,
// ProductConvertersSuite / ProductConvertDerivationSuite, CoproductHintSuite, CoproductConvertersSuite,
// TupleConvertersSuite, ValueClassSuite, DerivationModesSuite), derived both by Kindlings and by pureconfig.

final case class PcEmptyConf()
case object PcObjConf

final case class PcInner(e: Int, g: Int)
final case class PcManyDefaults(
    a: Int,
    b: String = "default",
    c: Int = 42,
    d: PcInner = PcInner(43, 44),
    e: Option[Int] = Some(45)
)

final case class PcTwoInts(a: Int, b: Int)
final case class PcOptInt(a: Int, b: Option[Int])
final case class PcFlat(b: Boolean, d: Double, f: Float, i: Int, l: Long, s: String, o: Option[String])

final case class PcTupled(values: (Boolean, Int))
final case class PcTupled3(values: (Int, (Long, String), Boolean))

final case class PcRec(ls: List[PcRec])

final case class PcGenericDefaults[T](i: Int = 1, s: String = "a", l: List[T] = Nil)

object PcLazy {
  def boom: String = throw new RuntimeException("Should not be evaluated")
}
final case class PcLazyA(foo: String = PcLazy.boom)
final case class PcLazyB(bar: Option[PcLazyA])
final case class PcLazyC(baz: PcLazyA = PcLazyA(), foo: String = PcLazy.boom)

sealed trait PcAnimalConfig
final case class PcDogConfig(age: Int) extends PcAnimalConfig
final case class PcCatConfig(age: Int) extends PcAnimalConfig
final case class PcBirdConfig(canFly: Boolean) extends PcAnimalConfig
final case class PcAnimalCage(animal: PcAnimalConfig)

sealed trait PcAmbiguous
final case class PcAmbiguousConf(typ: String) extends PcAmbiguous

sealed trait PcA
final case class PcAA1(a: Int) extends PcA
final case class PcAA2(a: String) extends PcA
final case class PcEnclosingA(values: Map[String, PcA])

final case class PcCamelInner(thisIsAnInt: Int, thisIsAnotherInt: Int)
final case class PcCamelOuter(camelCaseInt: Int, camelCaseString: String, camelCaseConf: PcCamelInner)

final class PcIntWrapper(val inner: Int) extends AnyVal {
  override def toString: String = s"PcIntWrapper($inner)"
}
final case class PcShapeWrapper(value: PcShape) extends AnyVal
final case class PcShapeDoubleWrapper(value: PcShapeWrapper)

/** A type with hand-written instances in its companion (upstream's DerivationModesSuite). */
final case class PcCustomObject(value: Int)
object PcCustomObject {
  implicit val pureconfigReader: pureconfig.ConfigReader[PcCustomObject] = pureconfig.ConfigReader.fromStringOpt {
    case "eaaxacaca" => Some(PcCustomObject(453))
    case "a"         => Some(PcCustomObject(45))
    case _           => Some(PcCustomObject(1))
  }
  implicit val pureconfigWriter: pureconfig.ConfigWriter[PcCustomObject] = pureconfig.ConfigWriter.toString {
    case PcCustomObject(453) => "eaaxacaca"
    case PcCustomObject(45)  => "a"
    case _                   => "cvbc"
  }
}
final case class PcCustomCaseClass(
    customObject: PcCustomObject,
    mapCustomObject: Map[String, PcCustomObject],
    mapListCustomObject: Map[String, List[PcCustomObject]]
)

/** Hints in companion objects (upstream docs, "Overriding behavior for case classes" / "... for sealed families"): each
  * library finds its own hint in the implicit scope of the type.
  */
final case class PcCompanionHinted(someField: Int, otherField: String = "x")
object PcCompanionHinted {
  import pureconfig.{CamelCase, ConfigFieldMapping, SnakeCase}
  implicit val kindlingsHint: hearth.kindlings.pureconfigderivation.KindlingsProductHint[PcCompanionHinted] =
    hearth.kindlings.pureconfigderivation
      .KindlingsProductHint[PcCompanionHinted](ConfigFieldMapping(CamelCase, SnakeCase), allowUnknownKeys = false)
  implicit val upstreamHint: pureconfig.generic.ProductHint[PcCompanionHinted] =
    pureconfig.generic
      .ProductHint[PcCompanionHinted](ConfigFieldMapping(CamelCase, SnakeCase), allowUnknownKeys = false)
}

sealed trait PcCompanionFamily
object PcCompanionFamily {
  implicit val kindlingsHint: hearth.kindlings.pureconfigderivation.KindlingsCoproductHint.Field[PcCompanionFamily] =
    hearth.kindlings.pureconfigderivation.KindlingsCoproductHint.Field[PcCompanionFamily]("kind")
  implicit val upstreamHint: pureconfig.generic.FieldCoproductHint[PcCompanionFamily] =
    new pureconfig.generic.FieldCoproductHint[PcCompanionFamily]("kind")
}
final case class PcCompanionLeaf(someValue: Int) extends PcCompanionFamily
case object PcCompanionObject extends PcCompanionFamily
