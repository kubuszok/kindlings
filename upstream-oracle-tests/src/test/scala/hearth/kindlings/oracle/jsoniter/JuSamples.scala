package hearth.kindlings.oracle.jsoniter

import java.time.*
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.collection.{immutable as ci, mutable as cm}

/** Values and JSON inputs of jsoniter-scala's `JsonCodecMakerSpec` (the values its tests serialize, the inputs they
  * parse and the ones they expect to be rejected), plus the usual invalid inputs, for `JsoniterUpstreamSpecOracleSpec`.
  */
@nowarn("cat=deprecation")
object JuSamples {

  private val commonInvalid = Seq("null", "[]", "{}", "1", "\"x\"", "{", "{\"a\":1,}", "")

  private def s[A](values: A*)(inputs: String*): JoSamples[A] = JoSamples(values, inputs ++ commonInvalid)

  /** Inputs for top-level scalars (numbers, booleans, chars, strings), plain and stringified. */
  private val scalars = Seq(
    "1", "-1", "0", "-0", "01", "127", "128", "-129", "32767", "32768", "2147483647", "2147483648",
    "9223372036854775807", "9223372036854775808", "1.5", "1.1", "2.2", "1e2", "1E-2", "1e400", "-1e400", "NaN",
    "true", "false", "tru", "\"1\"", "\"-1\"", "\"1.1\"", "\"1e2\"", "\"128\"", "\"true\"", "\"false\"", "\"V\"",
    "\"VV\"", "\"\"", "\" 1\"", "\"1 \"", "\"NaN\"", "\"\\u0056\"", "\"\\n\"", "1 ", " 1", "1,", "[1]"
  )

  private def scalar[A](values: A*): JoSamples[A] = s(values*)(scalars*)

  // primitives and standard types

  val primitives: JoSamples[JUPrimitives] = s(JUPrimitives(1.toByte, 2.toShort, 3, 4L, bl = true, 'V', 1.1, 2.2f))(
    """{"b":-128,"s":-32768,"i":-2147483648,"l":-9223372036854775808,'bl':true,"ch":"V","dbl":-123456789.0,"f":-12345.0}""",
    """{"b":-128,"s":-32768,"i":-2147483648,"l":-9223372036854775808,"bl":true,"ch":"V","dbl":-123456789.0,"f":""",
    """{"b":-128,"s":-32768,"i":-2147483648,"l":-9223372036854775808,"bl":true,"ch":"V","dbl":-123456789.0,"f":-12345.0}""",
    """{"b":128,"s":2,"i":3,"l":4,"bl":true,"ch":"V","dbl":1.1,"f":2.2}""",
    """{"b":1,"s":2,"i":3,"l":4,"bl":true,"ch":"VV","dbl":1.1,"f":2.2}""",
    """{"b":1,"s":2,"i":3,"l":4,"bl":null,"ch":"V","dbl":1.1,"f":2.2}""",
    """{"b":1,"s":2,"i":3,"l":4,"bl":true,"ch":"V","dbl":1.1}"""
  )

  val boxedPrimitives: JoSamples[JUBoxedPrimitives] =
    s(JUBoxedPrimitives(1.toByte, 2.toShort, 3, 4L, bl = true, 'V', 1.1, 2.2f))(
      """{"b":null,"s":2,"i":3,"l":4,"bl":true,"ch":"V","dbl":1.1,"f":2.2}""",
      """{"b":1,"s":2,"i":3,"l":4,"bl":null,"ch":"V","dbl":1.1,"f":2.2}""",
      """{"b":1,"s":2,"i":3,"l":4,"bl":true,"ch":null,"dbl":1.1,"f":2.2}""",
      """{"b":1,"s":2,"i":3,"l":4,"bl":true,"ch":"V","dbl":1.1}""",
      """{"b":1,"s":2,"i":null,"l":null,"bl":true,"ch":"V","dbl":null,"f":null}"""
    )

  val byteTop: JoSamples[Byte] = scalar(1.toByte)
  val shortTop: JoSamples[Short] = scalar(2.toShort)
  val intTop: JoSamples[Int] = scalar(3)
  val longTop: JoSamples[Long] = scalar(4L)
  val booleanTop: JoSamples[Boolean] = scalar(true, false)
  val charTop: JoSamples[Char] = scalar('V')
  val doubleTop: JoSamples[Double] = scalar(1.1)
  val floatTop: JoSamples[Float] = scalar(2.2f)
  val jByteTop: JoSamples[java.lang.Byte] = scalar(java.lang.Byte.valueOf(1.toByte))
  val jShortTop: JoSamples[java.lang.Short] = scalar(java.lang.Short.valueOf(2.toShort))
  val jIntegerTop: JoSamples[java.lang.Integer] = scalar(java.lang.Integer.valueOf(3))
  val jLongTop: JoSamples[java.lang.Long] = scalar(java.lang.Long.valueOf(4L))
  val jBooleanTop: JoSamples[java.lang.Boolean] = scalar(java.lang.Boolean.valueOf(true))
  val jCharacterTop: JoSamples[java.lang.Character] = scalar(java.lang.Character.valueOf('V'))
  val jDoubleTop: JoSamples[java.lang.Double] = scalar(java.lang.Double.valueOf(1.1))
  val jFloatTop: JoSamples[java.lang.Float] = scalar(java.lang.Float.valueOf(2.2f))

  private val boxedKeys = Seq(
    """{"1":11,"2":12}""", """{"false":false,"true":true}""", """{"1.0":11.0,"2.0":12.0}""", """{"a":"1","2":"b"}""",
    """{"x":1}""", """{"1":null}""", """{"":1}""", """{"ab":"c"}""", """{"128":1}""", """{"1":1,"1":2}""",
    """{"1e2":1}""", """{"true":1}""", """{"1":true}"""
  )
  private def boxedMap[A](k1: A, v1: A, k2: A, v2: A): JoSamples[Map[A, A]] = s(Map(k1 -> v1, k2 -> v2))(boxedKeys*)

  val jByteMap: JoSamples[Map[java.lang.Byte, java.lang.Byte]] =
    boxedMap[java.lang.Byte](1.toByte, 11.toByte, 2.toByte, 12.toByte)
  val jShortMap: JoSamples[Map[java.lang.Short, java.lang.Short]] =
    boxedMap[java.lang.Short](1.toShort, 11.toShort, 2.toShort, 12.toShort)
  val jIntegerMap: JoSamples[Map[java.lang.Integer, java.lang.Integer]] = boxedMap[java.lang.Integer](1, 11, 2, 12)
  val jLongMap: JoSamples[Map[java.lang.Long, java.lang.Long]] = boxedMap[java.lang.Long](1L, 11L, 2L, 12L)
  val jBooleanMap: JoSamples[Map[java.lang.Boolean, java.lang.Boolean]] =
    boxedMap[java.lang.Boolean](false, false, true, true)
  val jCharacterMap: JoSamples[Map[java.lang.Character, java.lang.Character]] =
    boxedMap[java.lang.Character]('a', '1', '2', 'b')
  val jDoubleMap: JoSamples[Map[java.lang.Double, java.lang.Double]] = boxedMap[java.lang.Double](1.0, 11.0, 2.0, 12.0)
  val jFloatMap: JoSamples[Map[java.lang.Float, java.lang.Float]] = boxedMap[java.lang.Float](1.0f, 11.0f, 2.0f, 12.0f)

  val standardTypes: JoSamples[JUStandardTypes] =
    s(
      JUStandardTypes("text" * 1000, BigInt("1234567890"), BigDecimal("1234567890.1234567890")),
      JUStandardTypes("VVV", BigInt(1), BigDecimal(2.0))
    )(
      """{"s":"XXX","s":"VVV","bi":10,"bi":1,"bd":20.0,"bd":2.0}""",
      "{\"\\udd1e\":\"VVV\",\"bi\":1,\"bd\":1.1}",
      """"s":"VVV","bi":1,"bd":1.1}""",
      """{"s""VVV","bi":1,"bd":1.1}""",
      """{"s":"VVV""bi":1"bd":1.1}""",
      """["s":"VVV","bi":1,"bd":2}""",
      """{,"s":"VVV","bi":1,"bd":2}""",
      """{"s":"VVV","bi":1,"bd":2]""",
      """{"s":"VVV","bi":1,"bd":2,}""",
      """{"s":null,"bi":0,"bd":1}""",
      """{"s":"VVV","bi":null,"bd":1}""",
      """{"s":"VVV","bi":1,"bd":null}""",
      """{"s":"VVV","bi":1}""",
      """{"s":"VVV","bi":1.5,"bd":1}""",
      """{"\u0073":"VVV","bi":1,"bd":1}"""
    )

  val stringTop: JoSamples[String] = scalar(
    "JavaScript Object Notation (JSON) is a lightweight, text-based, language-independent data interchange format."
  )
  val bigIntTop: JoSamples[BigInt] = scalar(BigInt("123456789012345678901234567890"))
  val bigDecimalTop: JoSamples[BigDecimal] = scalar(BigDecimal("1234567890.12345678901234567890"))

  val nonCaseClass: JoSamples[JUNonCaseClass] =
    s(new JUNonCaseClass(1, "VVV"))("""{"id":1}""", """{"name":"VVV"}""", """{"id":1,"name":null}""")

  val javaTypes: JoSamples[JUJavaTypes] = s(JUJavaTypes(new UUID(0, 0)))(
    """{"uuid":"00000000-XXXX-0000-0000-000000000000"}""",
    """{"uuid":null}""",
    """{"uuid":"00000000-0000-0000-0000-00000000000"}"""
  )
  val uuidTop: JoSamples[UUID] = s(new UUID(0, 0))(""""00000000-XXXX-0000-0000-000000000000"""", """"""""")
  val uuidMap: JoSamples[Map[UUID, Int]] =
    s(Map(new UUID(0, 0) -> 0))("""{"00000000-XXXX-0000-0000-000000000000":0}""", """{"":0}""")

  val javaEnums: JoSamples[JUJavaEnums] = s(JUJavaEnums(TimeUnit.SECONDS, java.math.RoundingMode.HALF_UP))(
    """{"l":null,"il":"HALF_UP"}""",
    """{"l":"SECOND","il":"HALF_UP"}""",
    """{"l":"SECONDS","il":"HALF_UP"}""",
    """{"l":"seconds","il":"half_up"}""",
    """{"l":"Seconds","il":"HalfUp"}""",
    """{"l":"s","il":"r"}""",
    """{"l":"SECONDS","il":"HALF_EVEN"}""",
    """{"l":"seconds","il":"half_even"}""",
    """{"l":"Seconds","il":"HalfEven"}""",
    """{"l":"SECONDS","il":1}""",
    """{"l":"SECONDS"}"""
  )
  val javaEnumTop: JoSamples[TimeUnit] = s(TimeUnit.HOURS)(""""HOUR"""", """"hours"""", "null", "0")
  val javaEnumMap: JoSamples[Map[TimeUnit, Int]] = s(Map(TimeUnit.HOURS -> 0))("""{"HOUR":0}""", """{"":0}""")

  // enumerations

  private val galileo1 = JULocationType.extra("Galileo1")

  val enums: JoSamples[JUEnums] = s(JUEnums(JULocationType.GPS), JUEnums(galileo1))(
    """{"lt":null}""", """{"lt":"GLONASS"}""", """{"lt":5}""", """{"lt":1}""", """{"lt":"1"}""", """{"lt":"GPS"}""",
    """{"lt":-1}""", """{"lt":1.0}""", """{"lt":2147483648}"""
  )
  val enums2: JoSamples[JUEnums2] = s(JUEnums2(JULocationType.GPS), JUEnums2(galileo1))(
    """{"lt":null}""", """{"lt":"GLONASS"}""", """{"lt":"5"}""", """{"lt":1}""", """{"lt":"1"}""", """{"lt":"GPS"}"""
  )
  val locationTop: JoSamples[JULocationType.LocationType] =
    s(JULocationType.GPS)(""""GPS"""", "1", """"1"""", "5", """"5"""", """"GLONASS"""")
  val locationMap: JoSamples[Map[JULocationType.LocationType, Int]] =
    s(Map(JULocationType.GPS -> 0))("""{"GPS":0}""", """{"1":0}""", """{"5":0}""", """{"GLONASS":0}""")

  val celestialList: JoSamples[List[JUCelestialBody.CelestialBody]] = s(JUCelestialBody.values.toList)(
    """["Earth","Eris"]""", "[2,100]", """["Vesta","Pallas","Juno","Halley","Encke"]""", "[10,11,12,13,14,15]"
  )
  val celestialMap: JoSamples[Map[JUCelestialBody.CelestialBody, Int]] =
    s(Map(JUCelestialBody.Earth -> 3))("""{"Eris":1}""", """{"100":1}""", """{"Earth":3}""", """{"4":3}""")

  val spectralList: JoSamples[List[JUSpectralClass.SpectralClass]] =
    s(List(JUSpectralClass.O, JUSpectralClass.G))("""["A"]""", "[3]", "[0,5]", """["O","G"]""")
  val spectralMap: JoSamples[Map[JUSpectralClass.SpectralClass, Int]] =
    s(Map(JUSpectralClass.G -> 1))("""{"G":1}""", """{"5":1}""", """{"A":1}""", """{"3":1}""")

  val temperatureList: JoSamples[List[JUTemperature.Temperature]] =
    s(List(JUTemperature.Cold, JUTemperature.Hot))("[-1,1]", "[0]", "[-2]", """["Cold"]""")
  val temperatureMap: JoSamples[Map[JUTemperature.Temperature, Int]] =
    s(Map(JUTemperature.Cold -> 1))("""{"-1":1}""", """{"0":1}""", """{"Cold":1}""")

  val cardRankList: JoSamples[List[JUCardRank.CardRank]] =
    s(
      List(JUCardRank.Two, JUCardRank.Ace),
      List(JUCardRank.King, JUCardRank.Queen, JUCardRank.Jack, JUCardRank.Ace)
    )("""["Ace"]""", """["King","K"]""", """["2","A"]""")
  val cardRankMap: JoSamples[Map[JUCardRank.CardRank, Int]] =
    s(Map(JUCardRank.Ace -> 1), Map(JUCardRank.K -> 1, JUCardRank.Jack -> 2))("""{"Ace":1}""", """{"K":1}""")

  val birdList: JoSamples[List[JUBird.Bird]] =
    s(List(JUBird.`Ñandú`, JUBird.Emu))("[\"\\u00d1and\\u00fa\"]", """["Nandu"]""")
  val birdMap: JoSamples[Map[JUBird.Bird, Int]] =
    s(Map(JUBird.`Ñandú` -> 1, JUBird.Emu -> 2))("{\"\\u00d1and\\u00fa\":1}", """{"Nandu":1}""")

  // options, tuples, arrays

  val options: JoSamples[JUOptions] = s(
    JUOptions(
      Option("VVV"),
      Option(BigInt(4)),
      Option(Set()),
      Option(1L),
      Option(java.lang.Long.valueOf(2L)),
      Option(JUStandardTypes("WWW", BigInt(2), BigDecimal(3.3)))
    ),
    JUOptions(None, None, None, None, None, None)
  )(
    """{"os":null,"obi":null,"osi":null,"ol":null,"ojl":null,"ost":null}""",
    """{"os":"VVV","obi":4,"osi":[1],"ol":1,"ojl":2,"ost":{"s":"WWW","bi":2,"bd":3.3}}""",
    """{"os":1}""", """{"obi":"4"}""", """{"osi":{}}""", """{"ost":{}}""", """{"ost":[]}""", """{"ojl":2.5}"""
  )
  val optionInt: JoSamples[Option[Int]] = s(Option.empty[Int], Option(1))("no!!!", "nul", "1.5", "\"1\"")
  val optionInner: JoSamples[Option[JUOptionInner]] =
    s(Option.empty[JUOptionInner], Option(JUOptionInner(1, b = true, "VVV")))("""{"a":1}""", "nul")
  val optionString: JoSamples[Option[String]] = s(Option("VVV"), Option.empty[String])("no!!!", "nul", "1")
  val optionBigInt: JoSamples[Option[BigInt]] = s(Option(BigInt(123)), Option.empty[BigInt])("123", "\"12x\"", "nul")
  val arrayOption: JoSamples[Array[Option[Int]]] = s(Array(Option.empty[Int], Option(1)))("[null,1]", "[1,]", "[,1]")
  val listOption: JoSamples[List[Option[Int]]] = s(List(Option(1), Option.empty[Int]))("[1,null]", "[1,nul]")
  val setOption: JoSamples[Set[Option[Int]]] = s(Set(Option(1), Option.empty[Int]))("[1,null]", "[null,null]")
  val mapOption: JoSamples[Map[String, Option[Int]]] =
    s(Map("VVV" -> Option.empty[Int], "WWW" -> Option(1)))("""{"VVV":null,"WWW":1}""", """{"VVV":"1"}""")

  val tuples: JoSamples[JUTuples] =
    s(JUTuples((1, 2.2, List('V')), ("VVV", 3, Some(JULocationType.GPS))))(
      """{"t1":[1,2.2],"t2":["VVV",3,"GPS"]}""",
      """{"t1":[1,2.2,["V"]],"t2":["VVV",3,"GPS","XXX"]}""",
      """{"t1":[1,2.2,["V"]],"t2":["VVV",3,null]}""",
      """{"t1":null,"t2":["VVV",3,"GPS"]}""",
      """{"t1":[],"t2":["VVV",3,"GPS"]}""",
      """{"t1":{},"t2":["VVV",3,"GPS"]}""",
      """{"t2":["VVV",3,"GPS"]}"""
    )
  val tupleTop: JoSamples[(String, Int)] = s(("VVV", 1))("""["VVV"]""", """["VVV",1,2]""", """["VVV",1,]""", "[,]")
  val tupleAliases: JoSamples[JUTupleAliases] = s(JUTupleAliases(("VVV", 1), (2, "WWW")))("""{"t1":["VVV",1]}""")
  val tuple3: JoSamples[(Long, Float, BigDecimal)] =
    s((1L, 2.2f, BigDecimal(3.3)))("""[1,2.2,3.3]""", """["1","2.2","3.3"]""", """["1","2.2"]""")

  val arrays: JoSamples[JUArrays] = s(
    JUArrays(Array(Array(1, 2), Array(3, 4)), Array[BigInt](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)),
    JUArrays(Array(Array[Int](), Array[Int]()), Array[BigInt]())
  )(
    """{"aa":[[],[]]}""",
    """{"aa":[[],[]],"a":[]}""",
    """{"aa":[{1,2,3]],"a":[]}""",
    """{"aa":[[,1,2,3]],"a":[]}""",
    """{"aa":[[1,2,3}],"a":[]}""",
    """{"aa":[[1,2,3,]],"a":[]}""",
    """{"aa":null,"a":null}""",
    """{"aa":[null],"a":[null]}""",
    """{"a":[1]}"""
  )
  val arrayArrayTop: JoSamples[Array[Array[Int]]] =
    s(Array(Array(1, 2, 3), Array(4, 5, 6)))(
      """[[1,2,3],[4,5,6]]""", """[["1","2","3"],["4","5","6"]]""", "[[]]", "[null]", "[[1,],[2]]", "[[1],]"
    )

  // collections

  val iterables: JoSamples[JUIterables] = s(
    JUIterables(collection.Set(collection.SortedSet("1", "2", "3")), collection.IndexedSeq(collection.Seq(1.1f, 2.2f, 3.3f)), collection.Iterable(4, 5, 6)),
    JUIterables(ci.Set(ci.SortedSet("1", "2", "3")), ci.IndexedSeq(ci.Seq(1.1f, 2.2f, 3.3f)), ci.Iterable(4, 5, 6)),
    JUIterables(cm.Set(cm.SortedSet("1", "2", "3")), cm.IndexedSeq(cm.Seq(1.1f, 2.2f, 3.3f)), cm.Iterable(4, 5, 6))
  )("""{"s":[["3","1","2","1"]],"is":[[]],"i":null}""", """{"s":null}""", """{"s":[null]}""")

  val mutableIterables: JoSamples[JUMutableIterables] = s(
    JUMutableIterables(
      cm.Seq(cm.SortedSet("1", "2", "3")),
      cm.ArrayBuffer(cm.Set[BigInt](4), cm.Set.empty[BigInt]),
      cm.ArraySeq(cm.LinkedHashSet(5, 6), cm.LinkedHashSet.empty[Int]),
      cm.Buffer(cm.HashSet(7.7)),
      cm.ListBuffer(cm.TreeSet(9L, 10L)),
      cm.IndexedSeq(cm.ArrayStack(11.11f, 12.12f)),
      cm.UnrolledBuffer(cm.Iterable(13.toShort, 14.toShort))
    )
  )(
    """{"ml":[["3","2","1"]],"ab":[[4,4]],"as":[[6,5,6]],"b":[[7.7,7.7]],"lb":[[10,9]],"is":[[]],"ub":[[]]}""",
    """{"ml":null,"ab":null,"as":null,"b":null,"lb":null,"is":null,"ub":null}"""
  )

  val immutableIterables: JoSamples[JUImmutableIterables] = s(
    JUImmutableIterables(
      List(ci.ListSet("1")),
      ::(::(2, ::(3, Nil)), Nil),
      ci.Queue(Set[BigInt](4)),
      IndexedSeq(ci.SortedSet(5, 6, 7), ci.SortedSet()),
      Stream(ci.TreeSet(8.9)),
      Vector(Iterable(10L, 11L))
    )
  )(
    """{"l":[["1"]],"nl":[[]],"q":[[4]],"is":[[5,6,7],[]],"s":[[8.9]],"v":[[10,11]]}""",
    """{"l":[["1"]],"nl":[],"q":[[4]],"is":[[5,6,7],[]],"s":[[8.9]],"v":[[10,11]]}""",
    """{"l":[["1"]],"nl":null,"q":[[4]],"is":[[5,6,7],[]],"s":[[8.9]],"v":[[10,11]]}""",
    """{"l":[["1"]],"q":[[4]],"is":[[5,6,7],[]],"s":[[8.9]],"v":[[10,11]]}""",
    """{"nl":[[1]]}""",
    """{"l":[["1","1","2"]],"nl":[[1]],"is":[[7,6,5,5]],"s":[[9.9,8.9]]}"""
  )

  val nonEmptyTop: JoSamples[::[Int]] = s(::(1, ::(2, ::(3, Nil))))("[1,2,3]", "[1]", "[1,]", "[null]")

  val emptyIterables: JoSamples[JUEmptyIterables] =
    s(JUEmptyIterables(List(), cm.ArrayBuffer()), JUEmptyIterables(List("VVV"), cm.ArrayBuffer(1)))(
      """{"l":null,"a":null}""", """{"l":[],"a":[]}""", """{"l":[]}""", """{"a":[]}""", """{"l":["VVV"]}"""
    )
  val emptyIterables2: JoSamples[JUEmptyIterables2] = s(JUEmptyIterables2(List(), Set(), List()))("""{"ls":[[]]}""")
  val nestedIterables: JoSamples[JUNestedIterables] =
    s(JUNestedIterables(List(Left("x"), Right(Set(1)))))(
      """{"lessi":[{"type":"Left"}]}""", """{"lessi":[{"type":"Right"}]}""",
      """{"lessi":[{"type":"Right","value":null}]}""", """{"lessi":[{"type":"Right","value":[]}]}""",
      """{"lessi":[{"type":"Left","value":null}]}""", """{"lessi":[{"type":"Left","a":"x"}]}"""
    )
  val eitherTop: JoSamples[Either[Int, String]] =
    s[Either[Int, String]](Right("VVV"), Left(1))(
      """{"type":"Right","value":"VVV"}""", """{"type":"Right","b":"VVV"}""", """{"type":"Left","a":1}""",
      """{"type":"Left"}""", """{"type":"Middle","value":1}"""
    )
  val mutableSetTop: JoSamples[cm.Set[List[BigDecimal]]] =
    s(cm.Set(List[BigDecimal](1.1, 2.2)))("""[[1.1,2.2]]""", """[["1.1","2.2"]]""", """[[1.1],[1.1]]""", "[null]")

  private val elevenInts = """[1,2,3,4,5,6,7,8,9,10,11]"""
  val set: JoSamples[Set[Int]] = s(Set(1, 2))(elevenInts, """[1,1,1,1,1,1,1,1,1,1,1,1]""", """[1,2,3,4,5,6,7,8,9,10]""")
  val mutableSet: JoSamples[cm.Set[Int]] =
    s(cm.Set(1, 2))(elevenInts, """[1,1,1,1,1,1,1,1,1,1,1,1]""", """[1,2,3,4,5,6,7,8,9,10]""")

  // maps

  val genericMaps: JoSamples[JUGenericMaps] = s(
    JUGenericMaps(collection.Map(1 -> true)),
    JUGenericMaps(cm.Map(1 -> true)),
    JUGenericMaps(ci.Map(1 -> true))
  )("""{"m":null}""", """{"m":{"1":true,"1":false}}""", """{"m":{"x":true}}""")

  val mutableMaps: JoSamples[JUMutableMaps] = s(
    JUMutableMaps(
      cm.HashMap(true -> cm.AnyRefMap(BigDecimal(1.1) -> 1)),
      cm.Map(1.1f -> cm.ListMap(BigInt(2) -> "2")),
      cm.OpenHashMap(1.1 -> cm.LinkedHashMap(3.toShort -> 3.3), 2.2 -> cm.LinkedHashMap())
    )
  )(
    """{"m":{"1.1":{"null":"2"}}""",
    """{"hm":null,"m":null,"ohm":null}""",
    """{"hm":{"true":{"1.1":1,"1.10":2}},"m":{},"ohm":{}}""",
    """{"hm":{"yes":{}}}"""
  )

  val immutableMaps: JoSamples[JUImmutableMaps] = s(
    JUImmutableMaps(
      Map(1 -> 1.1),
      ci.HashMap("2" -> ci.ListMap('V' -> 2), "3" -> ci.ListMap('X' -> 3)),
      ci.SortedMap(4L -> ci.TreeMap(4.toByte -> 4.4f), 5L -> ci.TreeMap.empty[Byte, Float])
    )
  )(
    """{"m":["1":1.1},"hm":{},"sm":{}}""",
    """{"m":{,"1":1.1},"hm":{},"sm":{}}""",
    """{"m":{"1":1.1],"hm":{},"sm":{}""",
    """{"m":{"1":1.1,},"hm":{},"sm":{}""",
    """{"m":null,"hm":null,"sm":null}""",
    """{"sm":{"5":{"2":1,"1":2},"4":{}}}""",
    """{"hm":{"2":{"VV":2}}}"""
  )

  val emptyMaps: JoSamples[JUEmptyMaps] =
    s(JUEmptyMaps(Map(), cm.Map()), JUEmptyMaps(Map("VVV" -> 1), cm.Map(2L -> 3)))(
      """{"im":null,"mm":null}""", """{"im":{},"mm":{}}""", """{"im":{}}""", """{"mm":{}}"""
    )

  val linkedMapTop: JoSamples[cm.LinkedHashMap[Int, Map[Char, Boolean]]] =
    s(cm.LinkedHashMap(1 -> Map('V' -> true), 2 -> Map.empty[Char, Boolean]))(
      """{"1":{"V":true},"2":{}}""", """{"1":{"V":"true"},"2":{}}""", """{"2":{},"1":{}}""", """{"1":null}"""
    )

  private val pairArrays = Seq(
    """[[1,true]]""", """[["1","true"]]""", """{[1,true]]""", """[,[1,true]]""", """[[1,true,]]""", """[[1,true]}""",
    """[[1,true],]""", """[[1]]""", """[[1,true],[1,false]]""", """{"1":true}""", """[[2,true],[-1,false]]"""
  )
  val genericMap: JoSamples[collection.Map[Int, Boolean]] = s(collection.Map(1 -> true))(pairArrays*)
  val linkedMap: JoSamples[cm.LinkedHashMap[Int, Boolean]] = s(cm.LinkedHashMap(1 -> true))(pairArrays*)
  val mutableLongMap: JoSamples[cm.LongMap[Boolean]] = s(cm.LongMap(1L -> true))(pairArrays*)
  val immutableLongMap: JoSamples[ci.LongMap[Boolean]] = s(ci.LongMap(1L -> true))(pairArrays*)
  val intMap: JoSamples[ci.IntMap[Boolean]] = s(ci.IntMap(1 -> true))(pairArrays*)

  private val elevenEntries = Seq(
    """{"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,"9":9,"10":10,"11":11}""",
    """[[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8],[9,9],[10,10],[11,11]]""",
    """{"1":1,"2":2,"3":3,"4":4,"5":5,"6":6,"7":7,"8":8,"9":9,"10":10}""",
    """{"1":1,"1":2,"1":3,"1":4,"1":5,"1":6,"1":7,"1":8,"1":9,"1":10,"1":11}""",
    """{"3":1,"-2":2,"2147483647":3,"-2147483648":4,"3":5}""",
    """{"9223372036854775807":1,"-9223372036854775808":2,"4294967296":3}""",
    """[[3,1],[-2,2],[2147483647,3],[-2147483648,4],[3,5]]"""
  )
  val limitedMap: JoSamples[Map[Int, Int]] = s(Map(1 -> 1))(elevenEntries*)
  val limitedLongMap: JoSamples[ci.LongMap[Int]] = s(ci.LongMap(1L -> 1))(elevenEntries*)
  val limitedIntMap: JoSamples[ci.IntMap[Int]] = s(ci.IntMap(1 -> 1))(elevenEntries*)

  val mutableLongMaps: JoSamples[JUMutableLongMaps] =
    s(JUMutableLongMaps(cm.LongMap(1L -> 1.1), cm.LongMap(3L -> "33")))(
      """{"lm1":{"1":1.1},"lm2":{"3":"33"}}""", """{"lm1":null,"lm2":null}""", """{"lm1":{"x":1.1}}"""
    )
  val mutableLongMapsList: JoSamples[List[JUMutableLongMaps]] =
    s(List(JUMutableLongMaps(cm.LongMap(1L -> 1.1), cm.LongMap(3L -> "33"))))(
      """[{"lm1":[[1,1.1]],"lm2":[[3,"33"]]}]""", """[{"lm1":[["1","1.1"]],"lm2":[["3","33"]]}]"""
    )
  val stringifiedMutableLongMaps: JoSamples[JUStringifiedMutableLongMaps] =
    s(JUStringifiedMutableLongMaps(cm.LongMap(1L -> 1.1)))("""{"lm":[["1","1.1"]]}""", """{"lm":[[1,1.1]]}""")
  val immutableIntLongMaps: JoSamples[JUImmutableIntLongMaps] =
    s(JUImmutableIntLongMaps(ci.IntMap(1 -> 1.1, 2 -> 2.2), ci.LongMap(3L -> "33")))(
      """{"im":{"2":2.2,"1":1.1,"-1":0.0},"lm":{"3":"33","-3":"x"}}""", """{"im":null,"lm":null}"""
    )
  val immutableIntLongMapsList: JoSamples[List[JUImmutableIntLongMaps]] =
    s(List(JUImmutableIntLongMaps(ci.IntMap(1 -> 1.1, 2 -> 2.2), ci.LongMap(3L -> "33"))))(
      """[{"im":[[1,1.1],[2,2.2]],"lm":[[3,"33"]]}]""", """[{"im":[["1","1.1"]],"lm":[["3","33"]]}]"""
    )
  val stringifiedImmutableIntLongMaps: JoSamples[JUStringifiedImmutableIntLongMaps] =
    s(JUStringifiedImmutableIntLongMaps(ci.IntMap(1 -> 1.1, 2 -> 2.2), ci.LongMap(3L -> 3.3)))(
      """{"im":[["1","1.1"],["2","2.2"]],"lm":[["3","3.3"]]}""", """{"im":[[1,1.1]],"lm":[[3,3.3]]}"""
    )

  private val bitSetInputs =
    Seq("[1,2,1000]", "[1,2,10000]", "[1,2,-1]", "[1,2,999]", "[1,2,1023]", "[1,2,1024]", "[3,1,1]", """["1"]""")
  val bitSets: JoSamples[JUBitSets] =
    s(JUBitSets(collection.BitSet(0), ci.BitSet(1, 2, 3), cm.BitSet(4, 5, 6)))(
      """{"bs":null,"ibs":null,"mbs":null}""", """{"bs":[1024]}""", """{"ibs":[-1]}""", """{"mbs":[1024]}"""
    )
  val longIntBitTop: JoSamples[cm.LongMap[ci.IntMap[cm.BitSet]]] =
    s(cm.LongMap(1L -> ci.IntMap(2 -> cm.BitSet(4, 5, 6), 3 -> cm.BitSet.empty)))(
      """{"1":{"2":[4,5,6],"3":[]}}""", """{"1":{"2":["4","5","6"],"3":[]}}""", """{"1":{"2":[-1]}}"""
    )
  val immutableBitSetTop: JoSamples[ci.BitSet] = s(ci.BitSet(1, 2, 3))(bitSetInputs*)
  val mutableBitSetTop: JoSamples[cm.BitSet] = s(cm.BitSet(1, 2, 3))(bitSetInputs*)

  // names

  val cases: JoSamples[JUCamelPascalSnakeKebabCases] = s(JUCamelPascalSnakeKebabCases(1, 2, 3, 4, 5, 6, 7, 8))(
    """{"camelCase":1,"PascalCase":2,"snake_case":3,"kebab-case":4,"camel1":5,"Pascal1":6,"snake_1":7,"kebab-1":8}""",
    """{"camel_case":1,"pascal_case":2,"snake_case":3,"kebab_case":4,"camel_1":5,"pascal_1":6,"snake_1":7,"kebab_1":8}""",
    """{"camel-case":1,"pascal-case":2,"snake-case":3,"kebab-case":4,"camel-1":5,"pascal-1":6,"snake-1":7,"kebab-1":8}""",
    """{"CamelCase":1,"PascalCase":2,"SnakeCase":3,"KebabCase":4,"Camel1":5,"Pascal1":6,"Snake1":7,"Kebab1":8}""",
    """{"camelCase":1,"pascalCase":2,"snakeCase":3,"kebabCase":4,"camel1":5,"pascal1":6,"snake1":7,"kebab1":8}""",
    """{"camel_case":1,"pascal_case":2,"snake_case":3,"kebab_case":4,"camel1":5,"pascal1":6,"snake1":7,"kebab1":8}""",
    """{"camel-case":1,"pascal-case":2,"snake-case":3,"kebab-case":4,"camel1":5,"pascal1":6,"snake1":7,"kebab1":8}""",
    """{"CMLCS":1,"PascalCase":2,"snake_case":3,"kebab-case":4,"camel1":5,"Pascal1":6,"snake_1":7,"KBB1":8}""",
    """{"CAMELCASE":1,"PascalCase":2,"snake_case":3,"kebab-case":4,"camel1_":5,"Pascal1":6,"snake_1_":7,"KEBAB-1":8}""",
    """{"camelCase":1,"PascalCase":2,"snake_case10":3,"kebab-case":4,"camel1":5,"Pascal1":6,"snake_17":7,"kebab-1":8}"""
  )

  val nameOverridden: JoSamples[JUNameOverridden] =
    s(JUNameOverridden(oldName = "VVV"))("""{"oldName":"VVV"}""", """{"new_name":"VVV","oldName":"WWW"}""")

  val stringified: JoSamples[JUStringified] = s(JUStringified(1, 2, Option(1), List(2)))(
    """{"i":1,"bi":"2","o":"1","l":["2"]}""",
    """{"i":"1","bi":2,"o":"1","l":["2"]}""",
    """{"i":"1","bi":"2","o":1,"l":["2"]}""",
    """{"i":"1","bi":"2","o":"1","l":[2]}""",
    """{"i":"1","bi":"2","o":null,"l":null}""",
    """{"i":"1","bi":"2"}""",
    """{"i_":"1","bi_":"2","o_":"1","l_":["2"]}""",
    """{"i":"1.0","bi":"2"}""",
    """{"i":" 1","bi":"2"}"""
  )

  val utf8: JoSamples[JUUTF8KeysAndValues] = s(JUUTF8KeysAndValues("ვვვ"), JUUTF8KeysAndValues("ვვვ\b\f\n\r\t/"))(
    "{\"\\u10d2\\u10d0\\u10e1\\u10d0\\u10e6\\u10d4\\u10d1\\u10d8\":\"\\u10d5\\u10d5\\u10d5\\b\\f\\n\\r\\t\\/\"}",
    """{"გასაღებ":"ვვვ"}"""
  )
  val operators: JoSamples[JUOperators] =
    s(JUOperators(7))("""{" =<>!#%^&|*/\\~+-:$ ":7}""", """{"=<>!#%^&|*/\\~+-:$":7}""")

  // defaults, transient and required fields

  val recursive: JoSamples[JURecursive] =
    s(JURecursive("VVV", 1.1, List(1, 2, 3), Map('S' -> JURecursive("WWW", 2.2, List(4, 5, 6), Map()))))(
      "{\r \"s\":\t\"VVV\",\n\t\"bd\":\t1.1,\r  \"l\":\t[\r\t\t1,\r\t\t2,\r\t\t3\r\t],\r\t\"m\":\t{\n\t\t\"S\":\t{\r  \t\t\"s\":\t\t \t\"WWW\",\n\t\"bd\":\t2.2,\"l\":\t[\r\t\t4,\n\n\n5,\r\t\t6\r\t]\n}\n}\r}",
      """{"s":"VVV","bd":1.1,"m":{"S":{"s":"WWW","bd":2.2,"m":{"T":{"s":"X","bd":1}}}}}""",
      """{"s":"VVV","bd":1.1,"m":{"S":null}}""",
      """{"s":"VVV","bd":1.1,"m":{"SS":{"s":"WWW","bd":2.2}}}"""
    )

  val defaults: JoSamples[JUDefaults] = s(
    JUDefaults(),
    JUDefaults(oc = None, l = Nil),
    JUDefaults(
      oc = None,
      l = List(),
      a = Array(),
      ab = cm.ArrayBuffer(),
      m = Map(),
      mm = cm.Map(),
      im = ci.IntMap(),
      lm = cm.LongMap(),
      s = Set(),
      ms = cm.Set(),
      bs = ci.BitSet(),
      mbs = cm.BitSet()
    ),
    JUDefaults(st = "X", i = 2, bi = 3, oc = Some('Y'), a = Array(Array(1L, 2L), Array(3L, 5L)), ab = cm.ArrayBuffer(1))
  )(
    """{"st":"VVV"}""",
    """{"st":"VVV","i":1,"bi":-1,"oc":"X","l":[0],"a":[[1,2],[3,4]],"ab":[1,2],"m":{"1":true},"mm":{"VVV":1},"im":{"1":"VVV"},"lm":{"1":2},"s":["VVV"],"ms":[1],"bs":[1],"mbs":[1]}""",
    """{"oc":null,"l":[],"a":[],"ab":[],"m":{},"mm":{},"im":{},"lm":{},"s":[],"ms":[],"bs":[],"mbs":[]}""",
    """{"st":null,"bi":null,"l":null,"oc":null,"a":null,"ab":null,"m":null,"mm":null,"im":null,"lm":null,"s":null,"ms":null,"bs":null,"mbs":null}""",
    """{"i":null}"""
  )

  val defaults2: JoSamples[JUDefaults2] = s(JUDefaults2())(
    """{"st":null,"bi":null,"l":null,"oc":null,"e":null,"ab":null,"m":null,"mm":null,"im":null,"lm":null,"s":null,"ms":null,"bs":null,"mbs":null}""",
    """{"l":[],"ab":[],"m":{},"mm":{},"im":{},"lm":{},"s":[],"ms":[],"bs":[],"mbs":[]}"""
  )

  val sharedEmptyMutableDefaults: JoSamples[JUSharedEmptyMutableDefaults] =
    s(JUSharedEmptyMutableDefaults(cm.ArrayBuffer(1), cm.ListBuffer(2), cm.Map("VVV" -> 3), cm.LongMap(4L -> 5), cm.Set(6)))(
      """{"ab":[1],"lb":[2],"mm":{"VVV":3},"lm":{"4":5},"ms":[6]}""",
      """{"ab":[],"lb":[],"mm":{},"lm":{},"ms":[]}""",
      """{"ab":null,"lb":null,"mm":null,"lm":null,"ms":null}"""
    )

  val polymorphicDefaults: JoSamples[JUPolymorphicDefaults[Int, String]] =
    s(JUPolymorphicDefaults[Int, String](), JUPolymorphicDefaults[Int, String](2, List("a")))(
      """{"i":1,"s":[]}""", """{"i":1,"cs":[]}""", """{"cs":null}"""
    )
  val nestedUser: JoSamples[JUNestedDefaults.User] =
    s(JUNestedDefaults.User("John"), JUNestedDefaults.User("John", 1))("""{"name":"John"}""", """{"age":1}""")
  val nestedBox: JoSamples[JUNestedDefaults.Box[Int]] =
    s(JUNestedDefaults.Box(1), JUNestedDefaults.Box(1, Nil))("""{"a":1}""", """{"a":1,"tags":null}""", """{"tags":[]}""")

  val iterablesWithDefaults: JoSamples[JUIterablesWithDefaults] =
    s(JUIterablesWithDefaults(), JUIterablesWithDefaults(List(1), Set(None, Some("x"))))(
      """{"l":[1,2,3]}""", """{"s":[]}""", """{"l":[],"s":[]}""", """{"l":null,"s":null}""", """{"s":[null,"x"]}"""
    )

  val transient: JoSamples[JUTransient] =
    s(JUTransient(required = "VVV"), JUTransient(required = "VVV", transient = "non-default"))(
      """{"transient":"XXX","required":"VVV"}""", """{"required":"VVV"}""", """{"transient":1,"required":"VVV"}""",
      """{"transient":"XXX"}""", """{"ignored":"x","required":"VVV"}"""
    )
  val scalaTransient: JoSamples[JUScalaTransient] =
    s(JUScalaTransient(required = "VVV"), JUScalaTransient(required = "VVV", transient = "non-default"))(
      """{"transient":"XXX","required":"VVV"}""", """{"required":"VVV"}""", """{"transient":1,"required":"VVV"}""",
      """{"transient":"XXX"}"""
    )

  val noneValues: JoSamples[JUNoneValues] = s(JUNoneValues(None), JUNoneValues(Some("x")))("""{"opt":null}""")
  val noneValuesList: JoSamples[List[JUNoneValues]] = s(List(JUNoneValues(None)))("[{}]", "[{},]", "[null]")
  val skipUnknown: JoSamples[JUSkipUnknown] = s(JUSkipUnknown())("""{"x":1,"y":[1,2],"z":{"a",3}}""", """{"x":1}""", """{"x":1,}""")

  private def required(range: Range): String =
    range.map(i => f""""r$i%02d":$i""").mkString("{\n", ",", "\n}")
  val required32: JoSamples[JURequired32] = s(
    JURequired32(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
      28, 29, 30, 31)
  )(required(0 to 30), required(1 to 31), required(0 to 31), required(2 to 31), required(1 to 30), required(31 to 31))

  val requiredAfterOptional: JoSamples[JURequiredAfterOptionalFields] =
    s(JURequiredAfterOptionalFields(None, 2, Some(3), 4))("""{"f2":2}""", """{"f1":1,"f2":2}""", """{"f4":4}""", """{"f2":2,"f4":4}""")

  // value classes

  val valueClassTypes: JoSamples[JUValueClassTypes] =
    s(JUValueClassTypes(JUUserId("123abc"), JUOrderId(123123)))("""{"uid":null,"oid":1}""", """{"uid":"x"}""", """{"uid":{"id":"x"},"oid":{"value":1}}""")
  val userIds: JoSamples[JUUserIds] = s(JUUserIds(List("VVV", "WWW")))("""["VVV","WWW"]""", """{"l":["VVV"]}""", "[1]")
  val userIdTop: JoSamples[JUUserId] = s(JUUserId("123abc"))(""""123abc"""", """{"id":"123abc"}""")
  val orderIdTop: JoSamples[JUOrderId] = scalar(JUOrderId(123123))
  val userIdArray: JoSamples[Array[JUUserId]] = s(Array(JUUserId("123abc"), JUUserId("123def")))("""["x",null]""")
  val orderIdArray: JoSamples[Array[JUOrderId]] = s(Array(JUOrderId(123123), JUOrderId(123456)))("""[1,null]""", """["1"]""")
  val orderUserMap: JoSamples[Map[JUOrderId, JUUserId]] =
    s(Map(JUOrderId(123123) -> JUUserId("123abc")))("""{"x":"y"}""", """{"1":1}""")
  val userOrderMap: JoSamples[Map[JUUserId, JUOrderId]] =
    s(Map(JUUserId("123abc") -> JUOrderId(123123)))("""{"x":"y"}""", """{"1":1}""")
  val aliasUserId: JoSamples[JUAlias.UserId] = s(JUAlias.UserId("123abc"))(""""x"""")
  val aliasOrderId: JoSamples[JUAlias.OrderId] = s(JUAlias.OrderId(123123))("2", """"2"""")
  val genericUserId: JoSamples[JUGeneric.UserId[String]] = s(JUGeneric.UserId[String]("123abc"))(""""x"""")
  val genericOrderId: JoSamples[JUGeneric.OrderId[Int]] = s(JUGeneric.OrderId[Int](123123))("2", """"2"""")

  val oneValueClassTypes: JoSamples[JUOneValueClassTypes] =
    s(JUOneValueClassTypes(JUOneUserId("123abc"), JUOneOrderId(123123)))(
      """{"uid":"123abc","oid":123123}""", """{"uid":{"value":"123abc"},"oid":{"value":123123}}""", """{"uid":null,"oid":1}"""
    )
  val transientOneValueClassTypes: JoSamples[JUTransientOneValueClassTypes] =
    s(JUTransientOneValueClassTypes(JUTransientUserId(value = "123abc"), JUTransientOrderId(value = 123123)))(
      """{"uid":"123abc","oid":123123}""", """{"uid":{"value":"123abc"},"oid":{"value":123123}}"""
    )
  val transientMap: JoSamples[Map[JUTransientUserId, JUTransientOrderId]] =
    s(Map(JUTransientUserId(value = "123abc") -> JUTransientOrderId(value = 123123)))("""{"123abc":123123}""", """{"x":"y"}""")
  val dependentMap: JoSamples[Map[JUDependentTransientUserId, List[JUDependentTransientUserId]]] =
    s(Map(JUDependentTransientUserId("123abc")() -> List(JUDependentTransientUserId("1234")())))("""{"123abc":["1234"]}""")
  val oneUserIdTop: JoSamples[JUOneUserId] = s(JUOneUserId("123abc"))(""""123abc"""", """{"value":"123abc"}""")
  val oneOrderIdTop: JoSamples[JUOneOrderId] = s(JUOneOrderId(123123))("123123", """{"value":123123}""", """"1"""")
  val oneUserIdArray: JoSamples[Array[JUOneUserId]] = s(Array(JUOneUserId("123abc"), JUOneUserId("123def")))("""["x",null]""")
  val oneOrderIdArray: JoSamples[Array[JUOneOrderId]] = s(Array(JUOneOrderId(123123), JUOneOrderId(123456)))("[1,null]")
  val oneUserIds: JoSamples[JUOneUserIds] = s(JUOneUserIds(List("VVV", "WWW")))("""["VVV","WWW"]""", """{"l":["VVV"]}""")
  val oneUserIdsT: JoSamples[JUOneUserIdsT[List[String]]] = s(JUOneUserIdsT(List("VVV", "WWW")))("""["VVV","WWW"]""", """{"l":["VVV"]}""")
  val oneUserIdsF: JoSamples[JUOneUserIdsF[List, String]] =
    s(JUOneUserIdsF[List, String](List("VVV", "WWW")))("""["VVV","WWW"]""", """{"l":["VVV"]}""")

  // nested options, either

  val nestedOptions: JoSamples[JUNestedOptions] = s(
    JUNestedOptions(None, None),
    JUNestedOptions(Some("VVV"), None),
    JUNestedOptions(None, Some(Some(Some("VVV")))),
    JUNestedOptions(None, Some(None)),
    JUNestedOptions(None, Some(Some(None)))
  )(
    """{"y":null,"x":null}""",
    """{"x":{"type":"Some","value":{"type":"Some","value":"VVV"}}}""",
    """{"x":{"type":"None"}}""",
    """{"x":{"type":"Some","value":{"type":"None"}}}""",
    """{"x":{"type":"Some","value":null}}""",
    """{"x":{"type":"Some"}}""",
    """{"x":"VVV"}"""
  )

  val modelList: JoSamples[List[JUModel]] =
    s(List(JUModel(Some("VVV"), Some(Some("WWW"))), JUModel(None), JUModel(None, Some(None))))(
      """[{"field1":"VVV","field2":"WWW"},{},{"field2":null}]""", """[{"field1":null,"field2":null}]""", """[{"field2":{"type":"None"}}]"""
    )

  val eitherFoo: JoSamples[JUEitherFoo] = s(JUEitherFoo(Left(1)), JUEitherFoo(Right(1)))(
    """{"value":{"type":"Left","value":1}}""", """{"value":{"Right":{"value":1}}}""", """{"value":{"type":"Left"}}""",
    """{"value":{"Left":{}}}""", """{"value":null}""", """{"value":1}""", """{"value":{"value":1,"type":"Left"}}"""
  )

  // java.time

  private val dow = DayOfWeek.FRIDAY
  private val d = Duration.parse("PT10H30M")
  private val i = Instant.parse("2007-12-03T10:15:30.001Z")
  private val ld = LocalDate.parse("2007-12-03")
  private val ldt = LocalDateTime.parse("2007-12-03T10:15:30")
  private val lt = LocalTime.parse("10:15:30")
  private val m = Month.APRIL
  private val md = MonthDay.parse("--12-03")
  private val odt = OffsetDateTime.parse("2007-12-03T10:15:30+01:00")
  private val ot = OffsetTime.parse("10:15:30+01:00")
  private val p = Period.parse("P1Y2M25D")
  private val y = Year.parse("2007")
  private val ym = YearMonth.parse("2007-12")
  private val zdt = ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]")
  private val zi = ZoneId.of("Europe/Paris")
  private val zo = ZoneOffset.of("+01:00")

  private val timeStrings = Seq(
    "FRIDAY", "friday", "Friday", "PT10H30M", "PT0S", "P1D", "2007-12-03T10:15:30.001Z", "2007-12-03T10:15:30Z",
    "2007-12-03", "2007-13-03", "2007-02-30", "2007-12-03T10:15:30", "2007-12-03T10:15", "10:15:30", "10:15",
    "25:15:30", "APRIL", "april", "--12-03", "--02-30", "2007-12-03T10:15:30+01:00", "10:15:30+01:00", "10:15:30Z",
    "P1Y2M25D", "P0D", "P1W", "2007", "-2007", "+12007", "99", "2007-12", "2007-13",
    "2007-12-03T10:15:30+01:00[Europe/Paris]", "2007-12-03T10:15:30+01:00", "2007-12-03T10:15:30Z[UTC]",
    "Europe/Paris", "Europe/Nowhere", "UTC", "Z", "+01:00", "+01", "+19:00", "-18:00", "", "x"
  )
  private def time[A](value: A): JoSamples[A] = s(value)(timeStrings.map(t => "\"" + t + "\"")*)

  val dayOfWeekTop: JoSamples[DayOfWeek] = time(dow)
  val durationTop: JoSamples[Duration] = time(d)
  val instantTop: JoSamples[Instant] = time(i)
  val localDateTop: JoSamples[LocalDate] = time(ld)
  val localDateTimeTop: JoSamples[LocalDateTime] = time(ldt)
  val localTimeTop: JoSamples[LocalTime] = time(lt)
  val monthTop: JoSamples[Month] = time(m)
  val monthDayTop: JoSamples[MonthDay] = time(md)
  val offsetDateTimeTop: JoSamples[OffsetDateTime] = time(odt)
  val offsetTimeTop: JoSamples[OffsetTime] = time(ot)
  val periodTop: JoSamples[Period] = time(p)
  val yearTop: JoSamples[Year] = time(y)
  val yearMonthTop: JoSamples[YearMonth] = time(ym)
  val zonedDateTimeTop: JoSamples[ZonedDateTime] = time(zdt)
  val zoneIdTop: JoSamples[ZoneId] = time(zi)
  val zoneOffsetTop: JoSamples[ZoneOffset] = time(zo)

  val javaTime: JoSamples[JUJavaTimeTypes] =
    s(JUJavaTimeTypes(dow, d, i, ld, ldt, lt, m, md, odt, ot, p, y, ym, zdt, zi, zo))("""{"dow":null}""", """{"dow":"FRIDAY"}""")
  val javaTimeMaps: JoSamples[JUJavaTimeMaps] = s(
    JUJavaTimeMaps(
      Map(dow -> 0), Map(d -> 0), Map(i -> 0), Map(ld -> 0), Map(ldt -> 0), Map(lt -> 0), Map(m -> 0), Map(md -> 0),
      Map(odt -> 0), Map(ot -> 0), Map(p -> 0), Map(y -> 0), Map(ym -> 0), Map(zdt -> 0), Map(zi -> 0), Map(zo -> 0)
    )
  )(
    (Seq("dow", "d", "i", "ld", "ldt", "lt", "m", "md", "odt", "ot", "p", "y", "ym", "zdt", "zi", "zo").flatMap { field =>
      Seq("x", "", "2007-13-03", "friday").map(key => s"""{"$field":{"$key":0}}""")
    })*
  )

  // ADTs

  val adtList: JoSamples[List[JUAdtBase]] = s[List[JUAdtBase]](
    List(JUAAA(1), JUBBB(BigInt(1)), JUCCC(1, "VVV"), JUDDD),
    List(JUCCC(2, "WWW"), JUCCC(1, "VVV"))
  )(
    """[{"type":"AAA","a":1},{"type":"BBB","a":1},{"type":"CCC","a":1,"b":"VVV"},{"type":"DDD"}]""",
    """[{"AAA":{"a":1}},{"BBB":{"a":1}},{"CCC":{"a":1,"b":"VVV"}},"DDD"]""",
    """[{"t":"CCC","a":2,"b":"WWW"},{"t":"CCC","a":1,"b":"VVV"}]""",
    """[{"type":"AAA","a":1},{"a":1,"type":"BBB"},{"a":1,"type":"CCC","b":"VVV"},{"type":"DDD"}]""",
    """[{"a":2,"b":"WWW","t":"CCC"},{"a":1,"t":"CCC","b":"VVV"}]""",
    s"""[{"a":2,"b":"${"W" * 1000}","type":"CCC"},{"a":1,"type":"CCC","b":"VVV"}]""",
    s"""[{"a":${"9" * 2000},"type":"BBB"},{"a":1,"type":"BBB"}]""",
    """[{"type":"AAA","a":1,"type":"XXX"}]""",
    """[{"type":"AAA","a":1,"type":"AAA"}]""",
    """[{"a":1}]""",
    """[{"a":1,"type":"aaa"}]""",
    """[{"a":1,"type":123}]""",
    """[null]""",
    """[true]""",
    """[{{"a":1}}]""",
    """[{"aaa":{"a":1}}]""",
    """[{"type":"AAA"}]""",
    """[{"type":"DDD","a":1}]""",
    """["AAA"]""",
    """[{"DDD":{}}]""",
    """[{"type":"Inner","a":1}]"""
  )

  private val leafInputs = Seq(
    """{"a":1}""", """{"type":"AAA","a":1}""", """{"type":"BBB","a":1}""", """{"type":"CCC","a":1,"b":"VVV"}""",
    """{"type":"DDD"}""", """{"type":"AAA","type":"AAA","a":1}""", """{"a":1,"type":"AAA"}""", """{"a":1,"b":"VVV"}""",
    """{"type":"XXX","a":1}""", """{"type":1,"a":1}""", """{"type":null,"a":1}""", """{"x":1}""", """{"a":1,"x":1}""",
    """{"a":1,"b":"VVV","type":"CCC"}""", """{"type":"DDD","type":"DDD"}"""
  )
  val aaa: JoSamples[JUAAA] = s(JUAAA(1))(leafInputs*)
  val bbb: JoSamples[JUBBB] = s(JUBBB(BigInt(1)))(leafInputs*)
  val ccc: JoSamples[JUCCC] = s(JUCCC(1, "VVV"))(leafInputs*)
  val ddd: JoSamples[JUDDD.type] = s[JUDDD.type](JUDDD)(leafInputs*)

  val weaponList: JoSamples[List[JUWeapon]] =
    s[List[JUWeapon]](List(JUWeapon.Axe, JUWeapon.Sword))("""["Axe","Sword"]""", """["Bow"]""", """[{"Axe":{}}]""", """[{"type":"Axe"}]""", "[null]")

  val extra: JoSamples[JUExtra.Base] = s[JUExtra.Base](JUExtra.A(1), JUExtra.B(), JUExtra.C)(
    """{"type":"A","x":1,"extra":"should be ignored"}""",
    """{"type":"B","extra":"should be ignored"}""",
    """{"type":"C","extra":"should be ignored"}""",
    """{"A":{"x":1,"extra":"should be ignored"}}""",
    """{"B":{"extra":"should be ignored"}}""",
    """"C"""",
    """{"C":{"extra":"should be ignored"}}""",
    """{"A":{"x":1},"extra":1}""",
    """"B""""
  )

  val enumeration: JoSamples[JUOpenapi.Enum] = s[JUOpenapi.Enum](JUOpenapi.EnumValue1, JUOpenapi.EnumValue2)(
    """{"type":"EnumValue1"}""", """{"hint":"EnumValue2"}""", """{"hint":"enum_value_1"}""", """"EnumValue2"""",
    """{"EnumValue1":{}}""", """{"hint":"EnumValue3"}""", """{"type":"enum_value_1"}"""
  )
  val enumerationList: JoSamples[List[JUOpenapi.Enum]] =
    s[List[JUOpenapi.Enum]](List(JUOpenapi.EnumValue1, JUOpenapi.EnumValue2))(
      """[{"EnumValue1":{}},{"EnumValue2":{}}]""", """["EnumValue1"]""", """[{"EnumValue1":{"x":1}}]""", """[{"EnumValue3":{}}]""", """[{"EnumValue1":null}]"""
    )

  val circeBase: JoSamples[JUCirce.Base] = s[JUCirce.Base](
    JUCirce.AbbA(2, List(1, 2, 3), Some(JUCirce.EnumValue)),
    JUCirce.AbbA(1, List(), None)
  )(
    """{"AbbA":{"x":2,"xs":[1,2,3],"oX":{"EnumValue":{}}}}""",
    """{"AbbA":{"x":1,"xs":[],"oX":null}}""",
    """{"abb_a":{"x":2,"xs":[1,2,3],"o_x":{"enum_value":{}}}}""",
    """{"abb_a":{"x":1,"xs":[],"o_x":null}}""",
    """{"AbbA":{"xs":[]}}""", """{"AbbA":{}}""", """{"abb_a":{"xs":[]}}""", """{"abb_a":{}}""",
    """{"AbbA":{"x":2,"xs":[1,2,3],"oX":"EnumValue"}}""", """{"AbbA":{"x":null,"xs":null,"oX":null}}"""
  )

  private val customNameInputs = Seq(
    """[{"t":"A","b":{"c":"x"}},{"t":"B","c":"x"}]""",
    """[{"name":"A","b":{"c":"x"}},{"name":"B","c":"x"}]""",
    """[{"type":"X","b":{"c":"x"}},{"type":"Y","c":"x"}]""",
    """[{"type":"A","b":{"c":"x"}},{"type":"B","c":"x"}]""",
    """[{"t":"A","b":{"c":"x","t":"B"}}]""",
    """[{"t":"A","b":{"t":"B","c":"x"}}]""",
    """[{"type":"X","b":{"c":"x","d":1}}]""",
    """[{"t":"B","c":"x","d":1}]"""
  )
  val customNameList: JoSamples[List[JUCustomName.Base]] =
    s[List[JUCustomName.Base]](List(JUCustomName.A(JUCustomName.B("x")), JUCustomName.B("x")))(customNameInputs*)
  val namedLeaves: JoSamples[List[JUNamedLeaves.Base]] =
    s[List[JUNamedLeaves.Base]](List(JUNamedLeaves.A(JUNamedLeaves.B("x")), JUNamedLeaves.B("x")))(customNameInputs*)
  val nestedAdtList: JoSamples[List[JUNestedAdts.Base]] =
    s[List[JUNestedAdts.Base]](List(JUNestedAdts.Leafs.Leaf(1), JUNestedAdts.Leafs.LeafObj))(
      """[{"type":"Leaf","a":1},{"type":"LeafObj"}]""",
      """[{"type":"hearth.kindlings.oracle.jsoniter.JUNestedAdts.Leafs.Leaf","a":1},{"type":"hearth.kindlings.oracle.jsoniter.JUNestedAdts.Leafs.LeafObj"}]"""
    )
  val versionList: JoSamples[List[JUVersion]] =
    s[List[JUVersion]](List(JUVersion.`8.09`, JUVersion.Current))("""["8.09","Current"]""", """["8.10"]""", """["`8.09`"]""")
  val nonAsciiList: JoSamples[List[JUNonAscii.База]] =
    s[List[JUNonAscii.База]](List(JUNonAscii.А(JUNonAscii.Б("x")), JUNonAscii.Б("x")))(
      """[{"тип":"А","б":{"с":"x"}},{"тип":"Б","с":"x"}]""", """[{"тип":"A","б":{"с":"x"}}]""", """[{"type":"А","б":{"с":"x"}}]""",
      "[{\"\\u0442\\u0438\\u043f\":\"\\u0411\",\"\\u0441\":\"x\"}]"
    )
  val timeZoneList: JoSamples[List[JUOperatorNames.TimeZone]] =
    s[List[JUOperatorNames.TimeZone]](List(JUOperatorNames.`US/Alaska`, JUOperatorNames.`Europe/Paris`))(
      """[{"zoneId":"US/Alaska"},{"zoneId":"Europe/Paris"}]""", """[{"zoneId":"US\/Alaska"}]""", """[{"zoneId":"US$divAlaska"}]"""
    )
  val deep: JoSamples[List[JUDeep.Base]] =
    s[List[JUDeep.Base]](List(JUDeep.A(1), JUDeep.B("VVV")))("""[{"type":"A","a":1},{"type":"B","b":"VVV"}]""", """[{"type":"Base5"}]""")
  val mixed: JoSamples[List[JUMixed.Base]] =
    s[List[JUMixed.Base]](List(JUMixed.A(1), JUMixed.B("VVV")))("""[{"type":"A","a":1},{"type":"B","b":"VVV"}]""")
  val diamond: JoSamples[List[JUDiamond.Base]] =
    s[List[JUDiamond.Base]](List(JUDiamond.A(1), JUDiamond.B("VVV")))("""[{"type":"A","a":1},{"type":"B","b":"VVV"}]""")
  val nonCaseLeaves: JoSamples[List[JUNonCaseLeaves.X]] =
    s[List[JUNonCaseLeaves.X]](List(new JUNonCaseLeaves.A(), JUNonCaseLeaves.B))("""[{"type":"A"},{"type":"B"}]""", """[{"type":"A","x":1}]""")
  val sealedClass: JoSamples[List[JUSealedClass.A]] =
    s[List[JUSealedClass.A]](List(new JUSealedClass.A(), JUSealedClass.B(1), JUSealedClass.C("VVV")))(
      """[{"type":"A"},{"type":"B","n":1},{"type":"C","s":"VVV"}]""", """[{"type":"A","n":1}]"""
    )
  val hintFoo: JoSamples[JUHintFoo] = s(JUHintFoo("a"))("""{"hint":"a"}""", """{"hint":"a","hint":"b"}""", """{"hint":1}""")
  val human: JoSamples[JUKingDom.Human] = s[JUKingDom.Human](
    JUKingDom.Human.King(name = "John", reignOver = Seq(JUKingDom.Human.Subject("Joanna")))
  )(
    """{"type":"King","name":"John","reignOver":[{"type":"Subject","name":"Joanna"}]}""",
    """{"type":"King","name":"John","reignOver":[{"type":"King","name":"Joanna"}]}""",
    """{"type":"King","name":"John"}""",
    """{"type":"King","name":"John","reignOver":null}"""
  )
  val basketApple: JoSamples[JUBasket[JUApple]] =
    s(JUBasket(List(JUApple("golden"), JUApple("red"))))("""{"fruits":[{"family":"golden"},{"family":"red"}]}""", """{"fruits":[{"color":1}]}""")
  val basketOrange: JoSamples[JUBasket[JUOrange]] =
    s(JUBasket(List(JUOrange(1), JUOrange(2))))("""{"fruits":[{"color":1},{"color":2}]}""", """{"fruits":[{"family":"x"}]}""")

  val typeAliases: JoSamples[JUTypeAliases.TypeAliases] =
    s(JUTypeAliases.TypeAliases(1, "VVV", List(1, 2, 3), Map(1 -> "VVV")))("""{"i":1,"s":"VVV","l":[1,2,3],"m":{"1":"VVV"}}""", """{"i":1,"s":"VVV"}""")
  val aliasMap: JoSamples[Map[Int, String]] = s(Map(1 -> "VVV"))("""{"1":"VVV"}""", """{"x":"VVV"}""")
  val aliasList: JoSamples[List[Int]] = s(List(1, 2, 3))("[1,2,3]", "[1,]")
  val idArray: JoSamples[Array[JUId[String]]] = s(Array[JUId[String]](JUId("1"), JUId("2")))("""["1","2"]""", "[1]")
  val firstOrder: JoSamples[JUFirstOrderType[Int, String]] =
    s(JUFirstOrderType[Int, String](1, "VVV", Some(1), List("WWW")))("""{"a":1,"b":"VVV","oa":1,"bs":["WWW"]}""", """{"a":1,"b":"VVV"}""")
  val firstOrderId: JoSamples[JUFirstOrderType[JUId[Int], JUId[String]]] =
    s(JUFirstOrderType[JUId[Int], JUId[String]](JUId[Int](1), JUId[String]("VVV"), Some(JUId[Int](2)), List(JUId[String]("WWW"))))(
      """{"a":1,"b":"VVV","oa":2,"bs":["WWW"]}""", """{"a":1,"b":"VVV"}"""
    )
  val gadtArray: JoSamples[Array[JUGadt.GADT[?]]] =
    s(Array[JUGadt.GADT[?]](JUGadt.Foo, JUGadt.Bar, JUGadt.Baz, JUGadt.Qux))("""["Foo","Bar","Baz","Qux"]""", """["Quux"]""")
  val gadt2Array: JoSamples[Array[JUGADT2[?]]] =
    s(Array[JUGADT2[?]](JUGADT2.Exists("WWW"), JUGADT2.ReadBytes("QQQ"), JUGADT2.CopyOver("AAA".getBytes.toSeq, "OOO")))(
      """[{"type":"Exists","path":"WWW"},{"type":"ReadBytes","path":"QQQ"},{"type":"CopyOver","src":[65,65,65],"path":"OOO"}]""",
      """[{"type":"IsDir","path":"x"}]""", """[{"type":"CopyOver","path":"x"}]"""
    )
  val hk: JoSamples[JUHk.Foo[Option]] =
    s[JUHk.Foo[Option]](JUHk.Bar[Option](Some(1)), JUHk.Baz[Option](Some("VVV")), JUHk.Bar[Option](None))(
      """{"type":"Bar","a":1}""", """{"type":"Baz","a":"VVV"}""", """{"type":"Bar","a":"VVV"}""", """{"type":"Baz"}"""
    )
  val freeGadt: JoSamples[List[JUFreeGadt.GADT[Int]]] =
    s[List[JUFreeGadt.GADT[Int]]](List(JUFreeGadt.FixedType(100), JUFreeGadt.VarType(200)))(
      """[{"type":"FixedType","value":100},{"type":"VarType","value":200}]""", """[{"type":"VarType","value":"x"}]"""
    )
  val hkOption: JoSamples[JUHigherKindedType[Option]] =
    s(JUHigherKindedType[Option](Some(1), Some(JUHigherKindedType[Option](Some(2), None))))("""{"f":1,"fs":{"f":2}}""", """{"fs":{"fs":{"fs":{}}}}""")
  val hkSeq: JoSamples[JUHigherKindedType[Seq]] =
    s(JUHigherKindedType[Seq](Seq(1, 2, 3), Seq(JUHigherKindedType[Seq](Seq(4, 5, 6), Seq()))))(
      """{"f":[1,2,3],"fs":[{"f":[4,5,6]}]}""", """{"fs":[{"fs":[{}]}]}"""
    )
  val auxiliary: JoSamples[JUAuxiliaryConstructor] =
    s(new JUAuxiliaryConstructor("VVV"), JUAuxiliaryConstructor(1))("""{"i":0,"s":"VVV"}""", """{"i":1}""", """{"s":"VVV"}""")
  val multiList: JoSamples[JUMultiListOfArgs] =
    s(new JUMultiListOfArgs(1)(2)("VVV"))("""{"i":1,"l":2,"s":"VVV"}""", """{"i":1,"l":2}""", """{"i":1,"s":"VVV"}""")
  val multiListDefault: JoSamples[JUMultiListOfArgsDefault] =
    s(new JUMultiListOfArgsDefault()(2)("VVV"), new JUMultiListOfArgsDefault(3)(2)("VVV"))(
      """{"l":2,"s":"VVV"}""", """{"i":1,"l":2,"s":"VVV"}""", """{"i":null,"l":2,"s":"VVV"}"""
    )
}
