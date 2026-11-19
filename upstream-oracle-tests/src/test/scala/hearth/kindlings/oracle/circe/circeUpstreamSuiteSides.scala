package hearth.kindlings.oracle.circe

import hearth.kindlings.circederivation.{Configuration, KindlingsCodecAsObject, KindlingsDecoder, KindlingsEncoder}
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.extras.Configuration as ExtrasConfiguration

/** The codecs of the `Cu` fixtures (the ones of upstream's own test suites), derived by one side. */
final case class CuCodecs(
    configBase: Codec[CuConfigExampleBase],
    configFoo: Codec[CuConfigExampleFoo],
    fooWithDefault: Codec[CuFooWithDefault],
    fooNoDefault: Codec[CuFooNoDefault],
    genericFoo: Codec[CuGenericFoo[Int]],
    pascalBase: Codec[CuPascalExampleBase],
    boxWub: Codec[CuBox[CuWub]],
    boxLong: Codec[CuBox[Long]],
    quxString: Codec[CuQux[String]],
    foo: Codec[CuFoo],
    tuple1: Codec[Tuple1[Int]],
    tuple3: Codec[(Int, Int, CuFoo)],
    seqFoo: Codec[Seq[CuFoo]],
    recursiveAdt: Codec[CuRecursiveAdtExample],
    recursiveWithOption: Codec[CuRecursiveWithOptionExample],
    adt3: Codec[CuAdt3],
    adt4: Codec[CuAdt4],
    longClass: Codec[CuLongClass]
)

object CuCodecs {

  /** Kindlings: every fixture derived on its own with `config` (read at runtime). */
  def kindlings(implicit config: Configuration): CuCodecs = CuCodecs(
    configBase = KindlingsCodecAsObject.derived[CuConfigExampleBase],
    configFoo = KindlingsCodecAsObject.derived[CuConfigExampleFoo],
    fooWithDefault = KindlingsCodecAsObject.derived[CuFooWithDefault],
    fooNoDefault = KindlingsCodecAsObject.derived[CuFooNoDefault],
    genericFoo = KindlingsCodecAsObject.derived[CuGenericFoo[Int]],
    pascalBase = KindlingsCodecAsObject.derived[CuPascalExampleBase],
    boxWub = KindlingsCodecAsObject.derived[CuBox[CuWub]],
    boxLong = KindlingsCodecAsObject.derived[CuBox[Long]],
    quxString = KindlingsCodecAsObject.derived[CuQux[String]],
    foo = KindlingsCodecAsObject.derived[CuFoo],
    tuple1 = Codec.from(KindlingsDecoder.derived[Tuple1[Int]], KindlingsEncoder.derived[Tuple1[Int]]),
    tuple3 = Codec.from(KindlingsDecoder.derived[(Int, Int, CuFoo)], KindlingsEncoder.derived[(Int, Int, CuFoo)]),
    seqFoo = Codec.from(KindlingsDecoder.derived[Seq[CuFoo]], KindlingsEncoder.derived[Seq[CuFoo]]),
    recursiveAdt = KindlingsCodecAsObject.derived[CuRecursiveAdtExample],
    recursiveWithOption = KindlingsCodecAsObject.derived[CuRecursiveWithOptionExample],
    adt3 = KindlingsCodecAsObject.derived[CuAdt3],
    adt4 = KindlingsCodecAsObject.derived[CuAdt4],
    longClass = KindlingsCodecAsObject.derived[CuLongClass]
  )

  /** circe-generic-extras' `deriveConfiguredCodec` (semi-automatic: an instance for each type). */
  def extras(implicit config: ExtrasConfiguration): CuCodecs = {
    import io.circe.generic.extras.semiauto.deriveConfiguredCodec
    object u {
      implicit lazy val configFoo: Codec.AsObject[CuConfigExampleFoo] = deriveConfiguredCodec[CuConfigExampleFoo]
      implicit lazy val configBar: Codec.AsObject[CuConfigExampleBar.type] =
        deriveConfiguredCodec[CuConfigExampleBar.type]
      implicit lazy val configBase: Codec.AsObject[CuConfigExampleBase] = deriveConfiguredCodec[CuConfigExampleBase]
      implicit lazy val fooWithDefault: Codec.AsObject[CuFooWithDefault] = deriveConfiguredCodec[CuFooWithDefault]
      implicit lazy val fooNoDefault: Codec.AsObject[CuFooNoDefault] = deriveConfiguredCodec[CuFooNoDefault]
      implicit lazy val genericFoo: Codec.AsObject[CuGenericFoo[Int]] = deriveConfiguredCodec[CuGenericFoo[Int]]
      implicit lazy val pascalFoo: Codec.AsObject[cuPascalExampleFoo] = deriveConfiguredCodec[cuPascalExampleFoo]
      implicit lazy val pascalBar: Codec.AsObject[cuPascalExampleBar.type] =
        deriveConfiguredCodec[cuPascalExampleBar.type]
      implicit lazy val pascalBase: Codec.AsObject[CuPascalExampleBase] = deriveConfiguredCodec[CuPascalExampleBase]
      implicit lazy val wub: Codec.AsObject[CuWub] = deriveConfiguredCodec[CuWub]
      implicit lazy val boxWub: Codec.AsObject[CuBox[CuWub]] = deriveConfiguredCodec[CuBox[CuWub]]
      implicit lazy val boxLong: Codec.AsObject[CuBox[Long]] = deriveConfiguredCodec[CuBox[Long]]
      implicit lazy val quxString: Codec.AsObject[CuQux[String]] = deriveConfiguredCodec[CuQux[String]]
      implicit lazy val bam: Codec.AsObject[CuBam] = deriveConfiguredCodec[CuBam]
      implicit lazy val foo: Codec.AsObject[CuFoo] = deriveConfiguredCodec[CuFoo]
      implicit lazy val baseAdt: Codec.AsObject[CuBaseAdtExample] = deriveConfiguredCodec[CuBaseAdtExample]
      implicit lazy val nestedAdt: Codec.AsObject[CuNestedAdtExample] = deriveConfiguredCodec[CuNestedAdtExample]
      implicit lazy val recursiveAdt: Codec.AsObject[CuRecursiveAdtExample] =
        deriveConfiguredCodec[CuRecursiveAdtExample]
      implicit lazy val recursiveWithOption: Codec.AsObject[CuRecursiveWithOptionExample] =
        deriveConfiguredCodec[CuRecursiveWithOptionExample]
      implicit lazy val adt3Class1: Codec.AsObject[CuAdt3Class1] = deriveConfiguredCodec[CuAdt3Class1]
      implicit lazy val adt3Object1: Codec.AsObject[CuAdt3Object1.type] = deriveConfiguredCodec[CuAdt3Object1.type]
      implicit lazy val adt3: Codec.AsObject[CuAdt3] = deriveConfiguredCodec[CuAdt3]
      implicit lazy val adt4Class1: Codec.AsObject[CuAdt4Class1] = deriveConfiguredCodec[CuAdt4Class1]
      implicit lazy val adt4Object1: Codec.AsObject[CuAdt4Object1.type] = deriveConfiguredCodec[CuAdt4Object1.type]
      implicit lazy val adt4: Codec.AsObject[CuAdt4] = deriveConfiguredCodec[CuAdt4]
      implicit lazy val longClass: Codec.AsObject[CuLongClass] = deriveConfiguredCodec[CuLongClass]
    }
    import u.*
    CuCodecs(
      configBase,
      configFoo,
      fooWithDefault,
      fooNoDefault,
      genericFoo,
      pascalBase,
      boxWub,
      boxLong,
      quxString,
      foo,
      Codec.from(Decoder[Tuple1[Int]], Encoder[Tuple1[Int]]),
      Codec.from(Decoder[(Int, Int, CuFoo)], Encoder[(Int, Int, CuFoo)]),
      Codec.from(Decoder[Seq[CuFoo]], Encoder[Seq[CuFoo]]),
      recursiveAdt,
      recursiveWithOption,
      adt3,
      adt4,
      longClass
    )
  }

  /** circe-generic's `deriveCodec` (shapeless on Scala 2, circe-core's `Codec.AsObject.derived` on Scala 3). */
  lazy val generic: CuCodecs = {
    import io.circe.generic.semiauto.deriveCodec
    object u {
      implicit lazy val configFoo: Codec.AsObject[CuConfigExampleFoo] = deriveCodec[CuConfigExampleFoo]
      implicit lazy val configBar: Codec.AsObject[CuConfigExampleBar.type] = deriveCodec[CuConfigExampleBar.type]
      implicit lazy val configBase: Codec.AsObject[CuConfigExampleBase] = deriveCodec[CuConfigExampleBase]
      implicit lazy val fooWithDefault: Codec.AsObject[CuFooWithDefault] = deriveCodec[CuFooWithDefault]
      implicit lazy val fooNoDefault: Codec.AsObject[CuFooNoDefault] = deriveCodec[CuFooNoDefault]
      implicit lazy val genericFoo: Codec.AsObject[CuGenericFoo[Int]] = deriveCodec[CuGenericFoo[Int]]
      implicit lazy val pascalFoo: Codec.AsObject[cuPascalExampleFoo] = deriveCodec[cuPascalExampleFoo]
      implicit lazy val pascalBar: Codec.AsObject[cuPascalExampleBar.type] = deriveCodec[cuPascalExampleBar.type]
      implicit lazy val pascalBase: Codec.AsObject[CuPascalExampleBase] = deriveCodec[CuPascalExampleBase]
      implicit lazy val wub: Codec.AsObject[CuWub] = deriveCodec[CuWub]
      implicit lazy val boxWub: Codec.AsObject[CuBox[CuWub]] = deriveCodec[CuBox[CuWub]]
      implicit lazy val boxLong: Codec.AsObject[CuBox[Long]] = deriveCodec[CuBox[Long]]
      implicit lazy val quxString: Codec.AsObject[CuQux[String]] = deriveCodec[CuQux[String]]
      implicit lazy val bam: Codec.AsObject[CuBam] = deriveCodec[CuBam]
      implicit lazy val foo: Codec.AsObject[CuFoo] = deriveCodec[CuFoo]
      implicit lazy val baseAdt: Codec.AsObject[CuBaseAdtExample] = deriveCodec[CuBaseAdtExample]
      implicit lazy val nestedAdt: Codec.AsObject[CuNestedAdtExample] = deriveCodec[CuNestedAdtExample]
      implicit lazy val recursiveAdt: Codec.AsObject[CuRecursiveAdtExample] = deriveCodec[CuRecursiveAdtExample]
      implicit lazy val recursiveWithOption: Codec.AsObject[CuRecursiveWithOptionExample] =
        deriveCodec[CuRecursiveWithOptionExample]
      implicit lazy val adt3Class1: Codec.AsObject[CuAdt3Class1] = deriveCodec[CuAdt3Class1]
      implicit lazy val adt3Object1: Codec.AsObject[CuAdt3Object1.type] = deriveCodec[CuAdt3Object1.type]
      implicit lazy val adt3: Codec.AsObject[CuAdt3] = deriveCodec[CuAdt3]
      implicit lazy val adt4Class1: Codec.AsObject[CuAdt4Class1] = deriveCodec[CuAdt4Class1]
      implicit lazy val adt4Object1: Codec.AsObject[CuAdt4Object1.type] = deriveCodec[CuAdt4Object1.type]
      implicit lazy val adt4: Codec.AsObject[CuAdt4] = deriveCodec[CuAdt4]
      implicit lazy val longClass: Codec.AsObject[CuLongClass] = deriveCodec[CuLongClass]
    }
    import u.*
    CuCodecs(
      configBase,
      configFoo,
      fooWithDefault,
      fooNoDefault,
      genericFoo,
      pascalBase,
      boxWub,
      boxLong,
      quxString,
      foo,
      Codec.from(Decoder[Tuple1[Int]], Encoder[Tuple1[Int]]),
      Codec.from(Decoder[(Int, Int, CuFoo)], Encoder[(Int, Int, CuFoo)]),
      Codec.from(Decoder[Seq[CuFoo]], Encoder[Seq[CuFoo]]),
      recursiveAdt,
      recursiveWithOption,
      adt3,
      adt4,
      longClass
    )
  }
}

/** Sample values of the `Cu` fixtures and the comparison of two sides over all of them. */
object CuSamples {
  import CiSamples.pair

  val configFoos: List[CuConfigExampleFoo] =
    List(CuConfigExampleFoo("field_value", 0, 100.0), CuConfigExampleFoo("", -7, 2.5))
  val configBases: List[CuConfigExampleBase] = configFoos :+ CuConfigExampleBar
  val foosWithDefault: List[CuFooWithDefault] =
    List(CuFooWithDefault(), CuFooWithDefault(None, "x"), CuFooWithDefault(Some(3), "y"))
  val foosNoDefault: List[CuFooNoDefault] = List(CuFooNoDefault(None), CuFooNoDefault(Some(1), "x"))
  val genericFoos: List[CuGenericFoo[Int]] = List(CuGenericFoo[Int](), CuGenericFoo(List(1, 2), "x"))
  val pascalBases: List[CuPascalExampleBase] = List(cuPascalExampleFoo("f", 1, 2.5), cuPascalExampleBar)
  val foos: List[CuFoo] = List(CuBar(1, "s"), CuBaz(List("a", "b")), CuBaz(Nil), CuBam(CuWub(5L), 1.5))
  val recursiveAdts: List[CuRecursiveAdtExample] =
    List(CuBaseAdtExample("a"), CuNestedAdtExample(CuNestedAdtExample(CuBaseAdtExample("b"))))
  val recursiveWithOptions: List[CuRecursiveWithOptionExample] = List(
    CuRecursiveWithOptionExample(None),
    CuRecursiveWithOptionExample(Some(CuRecursiveWithOptionExample(Some(CuRecursiveWithOptionExample(None)))))
  )
  val longClasses: List[CuLongClass] = List(
    CuLongClass(
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12",
      "13",
      "14",
      "15",
      "16",
      "17",
      "18",
      "19",
      "20",
      "21",
      "22",
      "23",
      "24",
      "25",
      "26",
      "27",
      "28",
      "29",
      "30",
      "31",
      "32",
      "33"
    )
  )

  /** `{}` and `{"a":null}` / `{"b":null}`: the inputs of upstream's "Option[T] with/without default" tests. */
  private val defaultInputs = List("""{}""", """{"a":null}""", """{"b":null}""", """{"a":"NotAnInt"}""", """{"b":1}""")

  def all(label: String, k: CuCodecs, u: CuCodecs, fooWrapperInputs: List[String] = Nil): List[CiDiff] = {
    import CirceOracle.compare
    compare(s"$label CuConfigExampleBase", pair(k.configBase, u.configBase), configBases) ++
      compare(s"$label CuConfigExampleFoo", pair(k.configFoo, u.configFoo), configFoos) ++
      compare(s"$label CuFooWithDefault", pair(k.fooWithDefault, u.fooWithDefault), foosWithDefault, defaultInputs) ++
      compare(s"$label CuFooNoDefault", pair(k.fooNoDefault, u.fooNoDefault), foosNoDefault, defaultInputs) ++
      compare(s"$label CuGenericFoo", pair(k.genericFoo, u.genericFoo), genericFoos, List("{}")) ++
      compare(s"$label CuPascalExampleBase", pair(k.pascalBase, u.pascalBase), pascalBases) ++
      compare(s"$label CuBox[CuWub]", pair(k.boxWub, u.boxWub), List(CuBox(CuWub(1L)))) ++
      compare(s"$label CuBox[Long]", pair(k.boxLong, u.boxLong), List(CuBox(1L))) ++
      compare(s"$label CuQux[String]", pair(k.quxString, u.quxString), List(CuQux(1, "a", 2))) ++
      compare(s"$label CuFoo", pair(k.foo, u.foo), foos, fooWrapperInputs) ++
      compare(s"$label Tuple1[Int]", pair(k.tuple1, u.tuple1), List(Tuple1(1)), List("[]", "[1,2]", "{}")) ++
      compare(s"$label (Int, Int, CuFoo)", pair(k.tuple3, u.tuple3), foos.map(f => (1, 2, f)), List("[1,2]")) ++
      compare(s"$label Seq[CuFoo]", pair(k.seqFoo, u.seqFoo), List(foos, Nil)) ++
      compare(s"$label CuRecursiveAdtExample", pair(k.recursiveAdt, u.recursiveAdt), recursiveAdts) ++
      compare(
        s"$label CuRecursiveWithOptionExample",
        pair(k.recursiveWithOption, u.recursiveWithOption),
        recursiveWithOptions
      ) ++
      compare(s"$label CuAdt3", pair(k.adt3, u.adt3), List(CuAdt3Class1(), CuAdt3Object1)) ++
      compare(s"$label CuAdt4", pair(k.adt4, u.adt4), List(CuAdt4Class1(), CuAdt4Object1)) ++
      compare(s"$label CuLongClass", pair(k.longClass, u.longClass), longClasses)
  }
}
