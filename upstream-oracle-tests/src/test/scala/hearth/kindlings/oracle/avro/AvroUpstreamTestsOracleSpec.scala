package hearth.kindlings.oracle.avro

import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder}

import java.nio.ByteBuffer
import java.util.UUID
import scala.util.{Success, Try}

/** avro4s' own test suites (avro4s-core/src/test/scala/com/sksamuel/avro4s: `schema`, `record/encoder`,
  * `record/decoder`, `github`), replayed against Kindlings: the same types (see `avroUpstreamTestFixtures.scala`, each
  * group names its upstream test file) are derived with both libraries and compared as in [[AvroOracleSpec]] - schema,
  * bytes, cross-decoding.
  *
  * Kindlings runs with `AvroConfig.avro4sDefaults`, so the two documented default value differences (BigDecimal,
  * OffsetDateTime) do not show up here; they are asserted in [[AvroOracleSpec]].
  */
final class AvroUpstreamTestsOracleSpec extends AvroOracleSuite {

  private def k[A](implicit e: AvroEncoder[A], d: AvroDecoder[A]): AvroSide[A] = AvroSide.kindlings[A]

  private def cmp[A](name: String, k: => AvroSide[A], u: => AvroSide[A], values: A*)(expected: String*): Unit =
    cmpWith[A](name, k, u, values.toList, _ == _)(expected*)

  private def cmpWith[A](name: String, k: => AvroSide[A], u: => AvroSide[A], values: List[A], eq: (A, A) => Boolean)(
      expected: String*
  ): Unit = test(name)(compareWith(k, u, values, eq)(expected*))

  /** The differences expected with avro4s 4 (Scala 2.13) and with avro4s 5 (Scala 3). */
  private def byVersion(avro4s4: Seq[String], avro4s5: Seq[String]): Seq[String] = if (isAvro4s5) avro4s5 else avro4s4

  /** A case avro4s cannot handle (deriving, or encoding the values, fails): Kindlings round-trips the values. */
  private def kindlingsOnly[A](name: String, k: => AvroSide[A], u: => AvroSide[A], values: A*): Unit =
    test(name) {
      assert(
        Try(values.map(v => AvroOracle.decodeBinary(u, u.schema, AvroOracle.encodeBinary(u, v).get).get)) != Success(
          values
        ),
        "avro4s handles this case now: compare it with Kindlings"
      )
      values.foreach { v =>
        assertEquals(AvroOracle.decodeBinary(k, k.schema, AvroOracle.encodeBinary(k, v).get), Success(v))
      }
    }

  private def bytesEq(a: Array[Byte], b: Array[Byte]): Boolean = java.util.Arrays.equals(a, b)

  private object K {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults
    lazy val aliases = k[AuAliases]
    lazy val docs = k[AuDocs]
    lazy val docMessage = k[AuDocMessage]
    lazy val docValueType = k[AuDocUsesValueType]
    lazy val fixedString = k[AuFixedString]
    lazy val fixedMessage = k[AuFixedMessage]
    lazy val optionOfFixed = k[AuOptionOfFixed]
    lazy val optionalFixedRecord = k[AuOptionalFixedRecord]
    lazy val fieldName = k[AuFieldName]
    lazy val className = k[AuClassName]
    lazy val weather = k[AuWeather]
    lazy val benelux = k[AuBenelux]
    lazy val ship = k[AuShip]
    lazy val waterproofBox = k[AuWaterproofBox]
    lazy val nsOuter = k[AuNsOuter]
    lazy val nsFieldWins = k[AuNsFieldWins]
    lazy val africa = k[AuAfrica]
    lazy val emptyNs = k[AuEmptyNs]
    lazy val myStark = k[AuMyStark]
    lazy val spaceship = k[AuSpaceship]
    lazy val cupcat = k[AuCupcat]
    lazy val nsParentBla = k[AuNsParent.Bla]
    lazy val usesNsParent = k[AuUsesNsParent]
    lazy val inObject = k[AuInObjectUser]
    lazy val props = k[AuProps]
    lazy val numeric = k[AuNumeric]
    lazy val fightingStyle = k[AuFightingStyleWrapper]
    lazy val posNumeric = k[AuPosNumeric]
    lazy val posStyle = k[AuPosStyleWrapper]
    lazy val byteArray = k[AuByteArray]
    lazy val byteSeq = k[AuByteSeq]
    lazy val byteList = k[AuByteList]
    lazy val byteVector = k[AuByteVector]
    lazy val byteBuffer = k[AuByteBuffer]
    lazy val topByteArray = k[Array[Byte]]
    lazy val topByteBuffer = k[ByteBuffer]
    lazy val arrays = k[AuArrays]
    lazy val seqTuple2 = k[AuSeqTuple2]
    lazy val collectionsOfMaps = k[AuCollectionsOfMaps]
    lazy val topArrayDouble = k[Array[Double]]
    lazy val topListInt = k[List[Int]]
    lazy val topVectorString = k[Vector[String]]
    lazy val topSetBoolean = k[Set[Boolean]]
    lazy val topMapDouble = k[Map[String, Double]]
    lazy val topMapString = k[Map[String, String]]
    lazy val topMapSeq = k[Map[String, Seq[String]]]
    lazy val topMapRecord = k[Map[String, AuSong]]
    lazy val topBoolean = k[Boolean]
    lazy val topLong = k[Long]
    lazy val topInt = k[Int]
    lazy val topString = k[String]
    lazy val topFloat = k[Float]
    lazy val topDouble = k[Double]
    lazy val topUuid = k[UUID]
    lazy val topBigDecimal = k[BigDecimal]
    lazy val topLocalTime = k[java.time.LocalTime]
    lazy val defaultPrimitives = k[AuDefaultPrimitives]
    lazy val defaultInstants = k[AuDefaultInstants]
    lazy val defaultValues = k[AuDefaultValues]
    lazy val optionalDefaultValues = k[AuOptionalDefaultValues]
    lazy val dogProspect = k[AuDogProspect]
    lazy val cuppers = k[AuCuppers]
    lazy val noVarg = k[AuNoVarg]
    lazy val uuidDefault = k[AuUuidDefault]
    lazy val uuids = k[AuUuids]
    lazy val bigDecimalDefault = k[AuBigDecimalDefault]
    lazy val bigDecimals = k[AuBigDecimals]
    lazy val defaultsInDefaults = k[AuDefaultsInDefaults]
    lazy val nestedDefaultNone = k[AuNestedDefaultNone]
    lazy val optionWithDefault = k[AuOptionWithDefault]
    lazy val optionDefaults = k[AuOptionDefaults]
    lazy val optionOfSeq = k[AuOptionOfSeqOfCaseClass]
    lazy val enumDefaults = k[AuEnumDefaults]
    lazy val eitherRecords = k[AuEitherRecords]
    lazy val eitherCollections = k[AuEitherCollections]
    lazy val topEitherOption = k[Either[String, Option[Int]]]
    lazy val genOuter = k[AuGenOuter[String, Int]]
    lazy val sameGeneric = k[AuSameGeneric]
    lazy val topGeneric = k[AuGeneric[String]]
    lazy val topGenericDisabled = k[AuGenericDisabled[String]]
    lazy val dibDabDob = k[AuDibDabDob]
    lazy val genericOfCollections = k[AuGenericOfCollections]
    lazy val maps = k[AuMaps]
    lazy val options = k[AuOptions]
    lazy val none = k[AuNone]
    lazy val singleSubtype = k[Option[AuSingleSubtype]]
    lazy val dibble = k[AuDibble]
    lazy val trapper = k[AuTrapper]
    lazy val napper = k[AuNapper]
    lazy val topNibble = k[AuNibble]
    lazy val a254 = k[AuA254]
    lazy val listA254 = k[List[AuA254]]
    lazy val b254 = k[AuB254]
    lazy val outer281 = k[AuOuter281]
    lazy val transient = k[AuTransient]
    lazy val tuple4 = k[AuTuple4]
    lazy val tuple5 = k[AuTuple5]
    lazy val tupleSeqs = k[AuTupleSeqs]
    lazy val utf8 = k[AuUtf8]
    lazy val charSeq = k[AuCharSeq]
    lazy val noDefault = k[AuNoDefaultTest]
    lazy val salesRecord = k[AuSalesRecord]
    lazy val p1 = k[AuP1]
    lazy val p2 = k[AuP2]
    lazy val optionalString = k[AuOptionalStringTest]
    lazy val defaultString = k[AuDefaultStringTest]
    lazy val localTime = k[AuLocalTime]
    lazy val localDateTime = k[AuLocalDateTime]
    lazy val scalaEnums = k[AuScalaEnums]
    lazy val topScalaEnum = k[AuColours.Value]
  }

  private object U {
    lazy val aliases = u[AuAliases]
    lazy val docs = u[AuDocs]
    lazy val docMessage = u[AuDocMessage]
    lazy val fixedString = u[AuFixedString]
    lazy val optionalFixedRecord = u[AuOptionalFixedRecord]
    lazy val fieldName = u[AuFieldName]
    lazy val className = u[AuClassName]
    lazy val weather = u[AuWeather]
    lazy val benelux = u[AuBenelux]
    lazy val ship = u[AuShip]
    lazy val waterproofBox = u[AuWaterproofBox]
    lazy val nsOuter = u[AuNsOuter]
    lazy val nsFieldWins = u[AuNsFieldWins]
    lazy val africa = u[AuAfrica]
    lazy val emptyNs = u[AuEmptyNs]
    lazy val myStark = u[AuMyStark]
    lazy val spaceship = u[AuSpaceship]
    lazy val cupcat = u[AuCupcat]
    lazy val nsParentBla = u[AuNsParent.Bla]
    lazy val usesNsParent = u[AuUsesNsParent]
    lazy val inObject = u[AuInObjectUser]
    lazy val props = u[AuProps]
    lazy val numeric = u[AuNumeric]
    lazy val fightingStyle = u[AuFightingStyleWrapper]
    lazy val posNumeric = u[AuPosNumeric]
    lazy val posStyle = u[AuPosStyleWrapper]
    lazy val byteArray = u[AuByteArray]
    lazy val byteSeq = u[AuByteSeq]
    lazy val byteList = u[AuByteList]
    lazy val byteVector = u[AuByteVector]
    lazy val byteBuffer = u[AuByteBuffer]
    lazy val topByteArray = u[Array[Byte]]
    lazy val topByteBuffer = u[ByteBuffer]
    lazy val arrays = u[AuArrays]
    lazy val seqTuple2 = u[AuSeqTuple2]
    lazy val collectionsOfMaps = u[AuCollectionsOfMaps]
    lazy val topArrayDouble = u[Array[Double]]
    lazy val topListInt = u[List[Int]]
    lazy val topVectorString = u[Vector[String]]
    lazy val topSetBoolean = u[Set[Boolean]]
    lazy val topMapDouble = u[Map[String, Double]]
    lazy val topMapString = u[Map[String, String]]
    lazy val topMapSeq = u[Map[String, Seq[String]]]
    lazy val topMapRecord = u[Map[String, AuSong]]
    lazy val topBoolean = u[Boolean]
    lazy val topLong = u[Long]
    lazy val topInt = u[Int]
    lazy val topString = u[String]
    lazy val topFloat = u[Float]
    lazy val topDouble = u[Double]
    lazy val topUuid = u[UUID]
    lazy val topBigDecimal = u[BigDecimal]
    lazy val topLocalTime = u[java.time.LocalTime]
    lazy val defaultPrimitives = u[AuDefaultPrimitives]
    lazy val defaultInstants = u[AuDefaultInstants]
    lazy val defaultValues = u[AuDefaultValues]
    lazy val optionalDefaultValues = u[AuOptionalDefaultValues]
    lazy val dogProspect = u[AuDogProspect]
    lazy val cuppers = u[AuCuppers]
    lazy val noVarg = u[AuNoVarg]
    lazy val uuidDefault = u[AuUuidDefault]
    lazy val uuids = u[AuUuids]
    lazy val bigDecimalDefault = u[AuBigDecimalDefault]
    lazy val bigDecimals = u[AuBigDecimals]
    lazy val defaultsInDefaults = u[AuDefaultsInDefaults]
    lazy val nestedDefaultNone = u[AuNestedDefaultNone]
    lazy val optionWithDefault = u[AuOptionWithDefault]
    lazy val optionDefaults = u[AuOptionDefaults]
    lazy val optionOfSeq = u[AuOptionOfSeqOfCaseClass]
    lazy val enumDefaults = u[AuEnumDefaults]
    lazy val eitherRecords = u[AuEitherRecords]
    lazy val eitherCollections = u[AuEitherCollections]
    lazy val topEitherOption = u[Either[String, Option[Int]]]
    lazy val sameGeneric = u[AuSameGeneric]
    lazy val topGeneric = u[AuGeneric[String]]
    lazy val topGenericDisabled = u[AuGenericDisabled[String]]
    lazy val dibDabDob = u[AuDibDabDob]
    lazy val genericOfCollections = u[AuGenericOfCollections]
    lazy val maps = u[AuMaps]
    lazy val options = u[AuOptions]
    lazy val none = u[AuNone]
    lazy val singleSubtype = u[Option[AuSingleSubtype]]
    lazy val dibble = u[AuDibble]
    lazy val trapper = u[AuTrapper]
    lazy val napper = u[AuNapper]
    lazy val topNibble = u[AuNibble]
    lazy val a254 = u[AuA254]
    lazy val listA254 = u[List[AuA254]]
    lazy val b254 = u[AuB254]
    lazy val outer281 = u[AuOuter281]
    lazy val transient = u[AuTransient]
    lazy val tuple4 = u[AuTuple4]
    lazy val tuple5 = u[AuTuple5]
    lazy val tupleSeqs = u[AuTupleSeqs]
    lazy val utf8 = u[AuUtf8]
    lazy val charSeq = u[AuCharSeq]
    lazy val noDefault = u[AuNoDefaultTest]
    lazy val salesRecord = u[AuSalesRecord]
    lazy val p1 = u[AuP1]
    lazy val p2 = u[AuP2]
    lazy val optionalString = u[AuOptionalStringTest]
    lazy val defaultString = u[AuDefaultStringTest]
    lazy val localTime = u[AuLocalTime]
    lazy val localDateTime = u[AuLocalDateTime]
  }

  private val uuid1 = UUID.fromString("86da265c-95bd-443c-8860-9381efca059d")

  group("avro4s tests: annotations") {
    cmp("AvroAliasSchemaTest", K.aliases, U.aliases, AuAliases("a", 1L, 2))()
    cmp("AvroDocSchemaTest: class, fields, nested", K.docs, U.docs, AuDocs(AuDocNested("f"), 1L))()
    cmp("AvroDocSchemaTest: doc on a nested record", K.docMessage, U.docMessage, AuDocMessage(AuDocRecord1("f")))()
    cmpWith[AuFixedString](
      "AvroFixedSchemaTest: @AvroFixed on a String field (decoded with the padding, as avro4s)",
      K.fixedString,
      U.fixedString,
      List(AuFixedString("sam")),
      (decoded, value) => decoded.mystring == value.mystring + "\u0000" * 4
    )()
    cmpWith[AuOptionalFixedRecord](
      "GithubIssue193: optional record with a fixed field",
      K.optionalFixedRecord,
      U.optionalFixedRecord,
      List(AuOptionalFixedRecord(Some(AuFixedUuid(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)))), AuOptionalFixedRecord(None)),
      (a, b) => a.uuid.map(_.bytes.toList) == b.uuid.map(_.bytes.toList)
    )()
    cmp("AvroNameSchemaTest: field", K.fieldName, U.fieldName, AuFieldName("a", "b"))()
    cmp("AvroNameSchemaTest: class", K.className, U.className, AuClassName("a", "b"))()
    cmp[AuWeather]("AvroNameSchemaTest: sealed trait enum", K.weather, U.weather, AuRainy, AuSunny)()
    cmp[AuBenelux](
      "AvroNameSchemaTest: sealed trait enum symbol",
      K.benelux,
      U.benelux,
      AuBelgium,
      AuVlaanderen,
      AuLuxembourg,
      AuNetherlands
    )()
    cmp("AvroNameEncoderTest: enum symbols", K.ship, U.ship, AuShip(AuAtlantic), AuShip(AuPacific))()
    cmp(
      "AvroNameEncoderTest: named union members",
      K.waterproofBox,
      U.waterproofBox,
      AuWaterproofBox(AuAirtightBox(AuCucumber(1.0))),
      AuWaterproofBox(AuAirtightBox(AuBlackberry("b")))
    )()
    cmp("AvroNamespaceSchemaTest: nested records", K.nsOuter, U.nsOuter, AuNsOuter("s", AuNsInner(1)))()
    cmp(
      "AvroNamespaceSchemaTest: field over record",
      K.nsFieldWins,
      U.nsFieldWins,
      AuNsFieldWins("s", AuNsIgnored(1))
    )()
    cmp[AuAfrica]("AvroNamespaceSchemaTest: sealed trait enum", K.africa, U.africa, AuCameroon, AuChad, AuComoros)()
    cmp("AvroNamespaceSchemaTest: empty namespace", K.emptyNs, U.emptyNs, AuEmptyNs("s"))()
    // avro4s 5 fails to build its encoder ("Cannot find subschema for type"): compared with avro4s 4 only
    if (isAvro4s5)
      kindlingsOnly(
        "AvroNameEncoderTest: field namespace on a union",
        K.myStark,
        U.myStark,
        AuMyStark(AuSansa(1), "a", 2),
        AuMyStark(AuBran("b"), "a", 2)
      )
    else
      cmp(
        "AvroNameEncoderTest: field namespace on a union",
        K.myStark,
        U.myStark,
        AuMyStark(AuSansa(1), "a", 2),
        AuMyStark(AuBran("b"), "a", 2)
      )()
    cmp(
      "AvroNameEncoderTest: empty namespaces",
      K.spaceship,
      U.spaceship,
      AuSpaceship(AuFunCosmos(1f)),
      AuSpaceship(AuMiserableCosmos(true))
    )()
    cmp(
      "AvroNameEncoderTest: field namespace on an Option",
      K.cupcat,
      U.cupcat,
      AuCupcat(Some(AuSnoutley("s"))),
      AuCupcat(None)
    )()
    cmp(
      "NamespaceSchemaTest: subtype inherits the parent's namespace",
      K.nsParentBla,
      U.nsParentBla,
      AuNsParent.Bla()
    )(
      byVersion(
        // avro4s 4 applies the sealed parent's namespace only to the members of the parent's union, so the subtype
        // derived on its own has another name than as a union member. Kindlings (as avro4s 5) always inherits it.
        avro4s4 = Seq("schema $[namespace]: kindlings=foobar upstream=hearth.kindlings.oracle.avro.AuNsParent"),
        avro4s5 = Nil
      )*
    )
    cmp(
      "NamespaceSchemaTest: subtype inherits (in a record)",
      K.usesNsParent,
      U.usesNsParent,
      AuUsesNsParent(AuNsParent.Bla())
    )(
      byVersion(
        avro4s4 = Nil,
        // avro4s 5 turns a sealed trait with a single subtype into that subtype's record; Kindlings (as avro4s 4)
        // keeps a union of one, on both Scala versions (feature-parity.md, "Sealed trait with one subtype").
        avro4s5 = Seq(
          "schema $[fields][0][type]: kindlings=List(ListMap(type -> record, name -> Bla, namespace -> foobar, fields -> List())) upstream=ListMap(type -> record, name -> Bla, namespace -> foobar, fields -> List())",
          "bytes: kindlings=00 upstream="
        )
      )*
    )
    cmp("NamespaceSchemaTest: class inside an object", K.inObject, U.inObject, AuInObjectUser(AuInObject.Inner("s")))()
    cmp("AvroPropSchemaTest", K.props, U.props, AuProps("a", 1L, 2))()
    cmp[AuNumeric](
      "AvroSortPrioritySchemaTest: enum",
      K.numeric,
      U.numeric,
      AuRationalNumber,
      AuRealNumber,
      AuNaturalNumber
    )()
    cmp(
      "AvroSortPrioritySchemaTest: union",
      K.fightingStyle,
      U.fightingStyle,
      AuFightingStyleWrapper(AuAggressive(1f)),
      AuFightingStyleWrapper(AuDefensive(true))
    )()
    cmp[AuPosNumeric](
      "AvroUnionPositionSchemaTest: enum",
      K.posNumeric,
      U.posNumeric,
      AuPosRational,
      AuPosReal,
      AuPosNatural
    )()
    cmp(
      "AvroUnionPositionSchemaTest: union",
      K.posStyle,
      U.posStyle,
      AuPosStyleWrapper(AuPosAggressive(1f)),
      AuPosStyleWrapper(AuPosDefensive(true))
    )()
    cmp("AvroNoDefaultTest", K.noDefault, U.noDefault, AuNoDefaultTest("a"))()
    cmp("TransientSchemaTest", K.transient, U.transient, AuTransient("a", None, "c"))()
    cmp("GithubIssue12: private vals", K.salesRecord, U.salesRecord, new AuSalesRecord(1, "d", Some("t")))()
  }

  group("avro4s tests: bytes") {
    cmpWith[AuByteArray](
      "ByteArraySchemaTest: Array[Byte]",
      K.byteArray,
      U.byteArray,
      List(AuByteArray(Array[Byte](1, 4, 9))),
      (a, b) => bytesEq(a.z, b.z)
    )()
    cmp("ByteArraySchemaTest: Seq[Byte]", K.byteSeq, U.byteSeq, AuByteSeq(Seq[Byte](1, 4, 9)))()
    cmp("ByteArraySchemaTest: List[Byte]", K.byteList, U.byteList, AuByteList(List[Byte](1, 4, 9)))()
    cmp("ByteArraySchemaTest: Vector[Byte]", K.byteVector, U.byteVector, AuByteVector(Vector[Byte](1, 4, 9)))()
    cmpWith[AuByteBuffer](
      "ByteArraySchemaTest: ByteBuffer",
      K.byteBuffer,
      U.byteBuffer,
      List(AuByteBuffer(ByteBuffer.wrap(Array[Byte](1, 4, 9)))),
      (a, b) => a.z.duplicate() == b.z.duplicate()
    )()
    cmpWith[Array[Byte]](
      "ByteArraySchemaTest: top level Array[Byte]",
      K.topByteArray,
      U.topByteArray,
      List(Array[Byte](1, 4, 9)),
      bytesEq
    )()
    cmpWith[ByteBuffer](
      "ByteArraySchemaTest: top level ByteBuffer",
      K.topByteBuffer,
      U.topByteBuffer,
      List(ByteBuffer.wrap(Array[Byte](1, 4, 9))),
      (a, b) => a.duplicate() == b.duplicate()
    )()
  }

  // Tuples: the records are named after the type arguments (feature-parity.md, "Tuple record names"), the only
  // difference in this group.
  group("avro4s tests: collections") {
    cmpWith[AuArrays](
      "ArraySchemaTest: fields",
      K.arrays,
      U.arrays,
      List(
        AuArrays(
          Vector(true, false),
          Vector(AuArrRecord("a", 1.5)),
          Seq("a", "34"),
          Array(true, false),
          Array(AuArrRecord("b", 2.5)),
          Set(AuArrRecord("c", 3.5)),
          Set("x"),
          Set(1.5)
        )
      ),
      (a, b) =>
        a.copy(array = null, arrayOfRecords = null) == b.copy(array = null, arrayOfRecords = null) &&
          a.array.toList == b.array.toList && a.arrayOfRecords.toList == b.arrayOfRecords.toList
    )()
    cmp(
      "ArraySchemaTest: Seq[Tuple2] (#156)",
      K.seqTuple2,
      U.seqTuple2,
      AuSeqTuple2("a", Seq((AuTupleA(1), AuTupleB(2))))
    )("schema $[fields][1][type][items][name]: kindlings=Tuple2__AuTupleA_AuTupleB upstream=Tuple2")
    cmpWith[AuCollectionsOfMaps](
      "ArraySchemaTest: collections of maps",
      K.collectionsOfMaps,
      U.collectionsOfMaps,
      List(AuCollectionsOfMaps(Array(Map("a" -> "b")), List(Map("c" -> "d")), Seq(Map.empty), Vector(Map("e" -> "f")))),
      (a, b) => a.array.toList == b.array.toList && a.copy(array = null) == b.copy(array = null)
    )()
    cmpWith[Array[Double]](
      "ArraySchemaTest: top level Array[Double]",
      K.topArrayDouble,
      U.topArrayDouble,
      List(Array(1.5, 2.5)),
      (a, b) => a.toList == b.toList
    )()
    cmp("ArraySchemaTest: top level List[Int]", K.topListInt, U.topListInt, List(1, 2), Nil)()
    cmp("ArraySchemaTest: top level Vector[String]", K.topVectorString, U.topVectorString, Vector("a"))()
    cmp("ArraySchemaTest: top level Set[Boolean]", K.topSetBoolean, U.topSetBoolean, Set(true))()
    cmp(
      "MapSchemaTest: fields",
      K.maps,
      U.maps,
      AuMaps(Map("a" -> "b"), Map("c" -> None, "d" -> Some(true)), Map("e" -> Seq("f")), Map("g" -> Seq(AuSong("s"))))
    )()
    cmp(
      "MapEncoderTest: top level Map[String, Double]",
      K.topMapDouble,
      U.topMapDouble,
      Map("a" -> 1.2, "ç" -> 34.5, "阿夫罗" -> 54.3)
    )()
    cmp("MapEncoderTest: top level Map[String, String]", K.topMapString, U.topMapString, Map("a" -> "b", "ç" -> "đ"))()
    cmp("MapDecoderTest: top level Map[String, Seq[String]]", K.topMapSeq, U.topMapSeq, Map("a" -> Seq("b")))()
    cmp("MapEncoderTest: top level Map[String, CaseClass]", K.topMapRecord, U.topMapRecord, Map("a" -> AuSong("s")))()
    cmp("TupleSchemaTest: tuple 4", K.tuple4, U.tuple4, AuTuple4(("a", 1, true, 2.5)))(
      "schema $[fields][0][type][name]: kindlings=Tuple4__String_Int_Boolean_Double upstream=Tuple4"
    )
    cmp("TupleSchemaTest: tuple 5", K.tuple5, U.tuple5, AuTuple5(("a", 1, true, 2.5, 3L)))(
      "schema $[fields][0][type][name]: kindlings=Tuple5__String_Int_Boolean_Double_Long upstream=Tuple5"
    )
    cmp(
      "TupleDecoderTest: tuple with seq",
      K.tupleSeqs,
      U.tupleSeqs,
      AuTupleSeqs((Seq("a"), Some(1))),
      AuTupleSeqs((Nil, None))
    )("schema $[fields][0][type][name]: kindlings=Tuple2__Seq__String_Option__Int upstream=Tuple2")
  }

  group("avro4s tests: primitives and strings") {
    cmp("PrimitiveSchemaTest: Boolean", K.topBoolean, U.topBoolean, true)()
    cmp("PrimitiveSchemaTest: Long", K.topLong, U.topLong, 1L)()
    cmp("PrimitiveSchemaTest: Int", K.topInt, U.topInt, 1)()
    cmp("PrimitiveSchemaTest: String", K.topString, U.topString, "s")()
    cmp("PrimitiveSchemaTest: Float", K.topFloat, U.topFloat, 1.5f)()
    cmp("PrimitiveSchemaTest: Double", K.topDouble, U.topDouble, 1.5)()
    cmp("UUID: top level", K.topUuid, U.topUuid, uuid1)()
    cmp("BigDecimal: top level", K.topBigDecimal, U.topBigDecimal, BigDecimal("12.34"))()
    cmp(
      "GithubIssue387: LocalTime",
      K.topLocalTime,
      U.topLocalTime,
      java.time.LocalTime.of(12, 50, 45),
      java.time.LocalTime.MAX.truncatedTo(java.time.temporal.ChronoUnit.MICROS)
    )()
    cmp("UtfSchemaTest: Utf8 fields", K.utf8, U.utf8, AuUtf8(new org.apache.avro.util.Utf8("n"), 1))()
    cmpWith[AuCharSeq](
      "StringDecoderTest: CharSequence",
      K.charSeq,
      U.charSeq,
      List(AuCharSeq("e")),
      (a, b) => a.a.toString == b.a.toString
    )()
    cmp("DateEncoderTest: LocalTime", K.localTime, U.localTime, AuLocalTime(java.time.LocalTime.of(12, 50, 45)))()
    cmp(
      "DateEncoderTest: LocalDateTime",
      K.localDateTime,
      U.localDateTime,
      AuLocalDateTime(java.time.LocalDateTime.of(2018, 9, 10, 11, 58, 59, 123)),
      AuLocalDateTime(java.time.LocalDateTime.of(2018, 9, 10, 11, 58, 59, 328187943))
    )()
  }

  group("avro4s tests: default values") {
    cmp("DefaultValueSchemaTest: primitives", K.defaultPrimitives, U.defaultPrimitives, AuDefaultPrimitives())()
    cmp(
      "DefaultValueSchemaTest: instants",
      K.defaultInstants,
      U.defaultInstants,
      AuDefaultInstants(
        java.time.Instant.ofEpochMilli(1),
        java.time.Instant.ofEpochMilli(2),
        java.time.Instant.ofEpochMilli(5)
      )
    )()
    test("DefaultValueSchemaTest: an Instant out of the range of a long cannot be encoded (ArithmeticException)") {
      def failure(side: AvroSide[AuDefaultInstants]): Option[String] =
        AvroOracle.encodeBinary(side, AuDefaultInstants()).failed.toOption.map(_.getClass.getName)
      assertEquals(failure(K.defaultInstants), Some("java.lang.ArithmeticException"))
      assertEquals(failure(K.defaultInstants), failure(U.defaultInstants))
    }
    cmp("DefaultValueSchemaTest: maps, sets and seqs", K.defaultValues, U.defaultValues, AuDefaultValues())()
    cmp(
      "DefaultValueSchemaTest: optional Seq, Set and Map",
      K.optionalDefaultValues,
      U.optionalDefaultValues,
      AuOptionalDefaultValues()
    )()
    cmp(
      "DefaultValueSchemaTest: None for an optional sealed trait",
      K.dogProspect,
      U.dogProspect,
      AuDogProspect(),
      AuDogProspect(Some(AuUpperDog(1.0)))
    )()
    cmp("DefaultValueSchemaTest: case class default", K.cuppers, U.cuppers, AuCuppers(), AuCuppers(AuRendal))(
      byVersion(
        // avro4s 4 leaves the `None` fields out of a record default (which Avro then rejects when the default is
        // needed, as `age` has no default of its own); Kindlings writes the complete record, as avro4s 5 does.
        avro4s4 = Seq("schema $[fields][0][default][age]: only in kindlings: null"),
        avro4s5 = Nil
      )*
    )
    cmp(
      "DefaultValueSchemaTest: case object default",
      K.noVarg,
      U.noVarg,
      AuNoVarg(),
      AuNoVarg(AuSnoutleyAdt("a", Some(1)))
    )()
    cmp("UUIDSchemaTest: default", K.uuidDefault, U.uuidDefault, AuUuidDefault())()
    cmp("UUIDSchemaTest: Seq and Option", K.uuids, U.uuids, AuUuids(Seq(uuid1), Some(uuid1)), AuUuids(Nil, None))()
    cmp("BigDecimalSchemaTest: default", K.bigDecimalDefault, U.bigDecimalDefault, AuBigDecimalDefault())(
      // avro4s writes the default of a decimal as its decimal string ("964.55"), which Avro reads as the bytes of
      // those characters, i.e. another number; Kindlings writes the bytes of the unscaled value, as the Avro
      // specification requires for the default of a `bytes` field (feature-parity.md, "Default of a decimal").
      "schema $[fields][0][default]: kindlings=\u0001x\u00c7 upstream=964.55"
    )
    cmp(
      "BigDecimalSchemaTest: Seq, Seq[Option], Option",
      K.bigDecimals,
      U.bigDecimals,
      AuBigDecimals(Seq(BigDecimal("1.25")), Seq(None, Some(BigDecimal("2.50"))), Some(BigDecimal("123.40")))
    )()
    cmp("Github411: defaults in defaults", K.defaultsInDefaults, U.defaultsInDefaults, AuDefaultsInDefaults())()
    cmp("GithubIssue890: nested default None", K.nestedDefaultNone, U.nestedDefaultNone, AuNestedDefaultNone())(
      // as "case class default" above
      byVersion(avro4s4 = Seq("schema $[fields][0][default][a]: only in kindlings: null"), avro4s5 = Nil)*
    )
    cmp(
      "OptionSchemaTest: default Some first",
      K.optionWithDefault,
      U.optionWithDefault,
      AuOptionWithDefault(),
      AuOptionWithDefault(None)
    )()
    cmp(
      "OptionDecoderTest: option defaults",
      K.optionDefaults,
      U.optionDefaults,
      AuOptionDefaults(t = "t"),
      AuOptionDefaults(None, None, "t"),
      AuOptionDefaults(Some("a"), Some(AuSnoutleyOptionEnum), "t")
    )()
    cmp(
      "OptionDecoderTest: option of seq of case class",
      K.optionOfSeq,
      U.optionOfSeq,
      AuOptionOfSeqOfCaseClass(Some(Seq(AuSong("a")))),
      AuOptionOfSeqOfCaseClass(None)
    )()
    cmp(
      "EnumSchemaTest: sealed trait enum field defaults",
      K.enumDefaults,
      U.enumDefaults,
      AuEnumDefaults(),
      AuEnumDefaults(AuSnoutleyEnum, Some(AuCuppersEnum), None)
    )()
  }

  group("avro4s tests: sum types") {
    cmp(
      "EitherSchemaTest: records with namespaces, primitives",
      K.eitherRecords,
      U.eitherRecords,
      AuEitherRecords(Left(AuGoo("s")), Left("a")),
      AuEitherRecords(Right(AuFoo(true)), Right(1.5))
    )()
    cmpWith[AuEitherCollections](
      "TypeGuardedDecoderTest: Either of collections, bytes, numbers, UUID",
      K.eitherCollections,
      U.eitherCollections,
      List(
        AuEitherCollections(Left(Seq("a")), Right("s"), Left(1L), Left(uuid1)),
        AuEitherCollections(Right(Map("a" -> 1)), Right("s"), Right(2), Right(true))
      ),
      (a, b) =>
        a.copy(bytesOrString = Right("")) == b.copy(bytesOrString =
          Right("")
        ) && a.bytesOrString.isRight == b.bytesOrString.isRight
    )(
      byVersion(
        avro4s4 = Nil,
        // avro4s 5 decodes the `Right(2)` of an `Either[Long, Int]` as `Left(2)`; Kindlings keeps the side
        avro4s5 = Seq(
          "upstream decodes kindlings bytes: Success(AuEitherCollections(Right(Map(a -> 1)),Right(s),Left(2),Right(true)))",
          "upstream round trip: Success(AuEitherCollections(Right(Map(a -> 1)),Right(s),Left(2),Right(true)))"
        )
      )*
    )
    // avro4s 4 decodes only the Left of such an Either, avro4s 5 rejects the flattened union: the schema is compared
    // (avro4s 4), and Kindlings round-trips every case
    test("EitherSchemaTest: nested unions flattened, null first (schema)") {
      assertEquals(K.topEitherOption.schema.toString, """["null","string","int"]""")
      if (!isAvro4s5) compareSchemas(K.topEitherOption, U.topEitherOption)()
    }
    kindlingsOnly[Either[String, Option[Int]]](
      "EitherSchemaTest: nested unions flattened, null first",
      K.topEitherOption,
      U.topEitherOption,
      Left("a"),
      Right(Some(1)),
      Right(None)
    )
    cmp(
      "OptionSchemaTest: Option of Either / enum / sealed trait",
      K.options,
      U.options,
      AuOptions(Some(Left("a")), Some(AuCuppersOptionEnum), Some(AuOptA("a"))),
      AuOptions(Some(Right(true)), None, Some(AuOptC("c"))),
      AuOptions(None, Some(AuSnoutleyOptionEnum), None)
    )()
    // avro4s 5 cannot encode a `None.type` field ("Not a record"): compared with avro4s 4 only
    if (isAvro4s5)
      kindlingsOnly("OptionSchemaTest: None.type", K.none, U.none, AuNone(None))
    else
      cmp("OptionSchemaTest: None.type", K.none, U.none, AuNone(None))()
    cmp[Option[AuSingleSubtype]](
      "GithubIssue885: Option of a sealed trait with 1 subtype",
      K.singleSubtype,
      U.singleSubtype,
      Some(AuOnlySubtype("a")),
      None
    )()
    cmp[AuDibble](
      "SealedTraitSchemaTest: top level enum with a renamed symbol",
      K.dibble,
      U.dibble,
      AuDabble,
      AuDobbles
    )()
    cmp(
      "SealedTraitSchemaTest: subtypes with the same field names",
      K.trapper,
      U.trapper,
      AuTrapper(AuTabble(1.5, 1)),
      AuTrapper(AuTobble("a", "b"))
    )()
    cmp(
      "SealedTraitSchemaTest: same field names and types",
      K.napper,
      U.napper,
      AuNapper(AuNabble("a", 1)),
      AuNapper(AuNobble("a", "b"))
    )()
    cmp[AuNibble](
      "SealedTraitSchemaTest: top level sealed trait",
      K.topNibble,
      U.topNibble,
      AuNabble("a", 1),
      AuNobble("a", "b")
    )()
    cmp("Github254: record wrapping an enum", K.a254, U.a254, AuA254(c1))()
    cmp("Github254: top level list", K.listA254, U.listA254, List(AuA254(c2)))()
    cmp("Github254: list field", K.b254, U.b254, AuB254(List(AuA254(c1), AuA254(c2))))()
    cmp(
      "GithubIssue281: sealed trait in a subobject",
      K.outer281,
      U.outer281,
      AuOuter281(Seq(AuInner281(AuInnerTraitConcrete2(1))))
    )()
  }

  group("avro4s tests: generics") {
    cmp(
      "GenericSchemaTest: same generic, different arguments",
      K.sameGeneric,
      U.sameGeneric,
      AuSameGeneric(AuGeneric(1), AuGeneric("a"))
    )()
    cmp("GenericSchemaTest: top level generic", K.topGeneric, U.topGeneric, AuGeneric("a"))()
    cmp("GenericSchemaTest: @AvroErasedName", K.topGenericDisabled, U.topGenericDisabled, AuGenericDisabled("a"))()
    cmp(
      "GenericSchemaTest: type arguments with the same short name (#260)",
      K.dibDabDob,
      U.dibDabDob,
      AuDibDabDob(AuGeneric(AuFirst.TypeArg("a")), AuGeneric(AuSecond.TypeArg("b")))
    )(
      byVersion(
        avro4s4 = Nil,
        // avro4s 5 ignores @AvroName on a type argument, so both fields get the record `AuGeneric__TypeArg` (and the
        // second one is written as a reference to the first: a wrong schema). Kindlings uses the annotated name, as
        // avro4s 4 does (feature-parity.md, "Generic record names").
        avro4s5 = Seq(
          "schema $[fields][1][type]: kindlings=ListMap(type -> record, name -> AuGeneric__supertypearg, fields -> List(ListMap(name -> t, type -> ListMap(type -> record, name -> supertypearg, namespace -> hearth.kindlings.oracle.avro.AuSecond, fields -> List(ListMap(name -> a, type -> string)))))) upstream=AuGeneric__TypeArg"
        )
      )*
    )
    cmp(
      "GenericSchemaTest: collection type arguments",
      K.genericOfCollections,
      U.genericOfCollections,
      AuGenericOfCollections(AuGeneric(List(1)), AuGeneric(Some("a")), AuGeneric(Map("a" -> 1)))
    )()
  }

  group("avro4s tests: schema evolution") {
    cmp("SchemaEvolutionTest: P1", K.p1, U.p1, AuP1("a"))()
    cmp("SchemaEvolutionTest: P2", K.p2, U.p2, AuP2("a"))()
    cmp("SchemaEvolutionTest: optional", K.optionalString, U.optionalString, AuOptionalStringTest("a", None))()
    cmp("SchemaEvolutionTest: default", K.defaultString, U.defaultString, AuDefaultStringTest("a"))()
  }
}
