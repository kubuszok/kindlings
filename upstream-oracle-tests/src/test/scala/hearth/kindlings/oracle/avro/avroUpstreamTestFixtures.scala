package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{
  AvroAlias,
  AvroDoc,
  AvroErasedName,
  AvroFixed,
  AvroName,
  AvroNamespace,
  AvroNoDefault,
  AvroProp,
  AvroSortPriority,
  AvroTransient,
  AvroUnionPosition
}
import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDoc,
  avroErasedName,
  avroFixed,
  avroName,
  avroNamespace,
  avroNoDefault,
  avroProp,
  avroSortPriority,
  avroUnionPosition,
  fieldName,
  transientField
}

// Fixtures mirroring avro4s' own test suites (avro4s-core/src/test/scala/com/sksamuel/avro4s/{schema,record,github}),
// used by AvroUpstreamTestsOracleSpec. Each group names the upstream test file it comes from. Annotated fixtures carry
// both avro4s' annotation and Kindlings' equivalent (each library reads only its own).

// --- schema/AvroAliasSchemaTest ---
@AvroAlias("queen")
@avroAlias("queen")
@AvroAlias("ledzep")
@avroAlias("ledzep")
case class AuAliases(
    @AvroAlias("cold") @avroAlias("cold") str: String,
    @AvroAlias("kate") @avroAlias("kate") @AvroAlias("bush") @avroAlias("bush") long: Long,
    int: Int
)

// --- schema/AvroDocSchemaTest ---
case class AuDocNested(@AvroDoc("b") @avroDoc("b") foo: String)
@AvroDoc("hello; is it me youre looking for")
@avroDoc("hello; is it me youre looking for")
case class AuDocs(
    @AvroDoc("c") @avroDoc("c") nested: AuDocNested,
    @AvroDoc("I am a long") @avroDoc("I am a long") long: Long
)
@AvroDoc("This is a record")
@avroDoc("This is a record")
case class AuDocRecord1(field: String)
case class AuDocMessage(record1: AuDocRecord1)
@AvroDoc("wibble")
@avroDoc("wibble")
case class AuDocValueType(s: String) extends AnyVal
case class AuDocUsesValueType(a: AuDocValueType)

// --- schema/AvroFixedSchemaTest, record/encoder/FixedEncoderTest, record/decoder/FixedDecoderTest ---
case class AuFixedString(@AvroFixed(7) @avroFixed(7) mystring: String)
@AvroFixed(8)
@avroFixed(8)
case class AuQuarterSha(bytes: Array[Byte]) extends AnyVal
case class AuFixedMessage(q: AuQuarterSha, payload: Array[Byte])
@AvroFixed(8)
@avroFixed(8)
case class AuFixedValueType(z: String) extends AnyVal
case class AuOptionOfFixed(opt: Option[AuFixedValueType])
// github/GithubIssue193
case class AuFixedUuid(@AvroFixed(8) @avroFixed(8) bytes: Array[Byte])
case class AuOptionalFixedRecord(uuid: Option[AuFixedUuid])

// --- schema/AvroNameSchemaTest, record/encoder/AvroNameEncoderTest ---
case class AuFieldName(@AvroName("wibble") @fieldName("wibble") wobble: String, wubble: String)
@AvroName("wibble")
@avroName("wibble")
case class AuClassName(a: String, b: String)
@AvroName("foofoo")
@avroName("foofoo")
sealed trait AuWeather
case object AuRainy extends AuWeather
case object AuSunny extends AuWeather
sealed trait AuBenelux
@AvroName("foofoo")
@avroName("foofoo")
@AvroSortPriority(-1)
@avroSortPriority(-1)
case object AuBelgium extends AuBenelux
case object AuVlaanderen extends AuBenelux
case object AuLuxembourg extends AuBenelux
@AvroSortPriority(1)
@avroSortPriority(1)
case object AuNetherlands extends AuBenelux
sealed trait AuOcean
@AvroName("atlan")
@avroName("atlan")
case object AuAtlantic extends AuOcean
@AvroName("pacif")
@avroName("pacif")
case object AuPacific extends AuOcean
case class AuShip(location: AuOcean)
sealed trait AuFood
@AvroNamespace("storage.boxes")
@avroNamespace("storage.boxes")
@AvroName("cucumber")
@avroName("cucumber")
case class AuCucumber(length: Double) extends AuFood
@AvroNamespace("storage.boxes")
@avroNamespace("storage.boxes")
@AvroName("blackberry")
@avroName("blackberry")
case class AuBlackberry(colour: String) extends AuFood
@AvroNamespace("storage.boxes")
@avroNamespace("storage.boxes")
case class AuAirtightBox(contents: AuFood)
@AvroNamespace("storage.boxes")
@avroNamespace("storage.boxes")
case class AuWaterproofBox(airtight_box: AuAirtightBox)

// --- schema/AvroNamespaceSchemaTest, record/encoder/AvroNameEncoderTest ---
@AvroNamespace("com.yuval.internal")
@avroNamespace("com.yuval.internal")
case class AuNsInner(i: Int)
@AvroNamespace("com.yuval")
@avroNamespace("com.yuval")
case class AuNsOuter(s: String, internal: AuNsInner)
@AvroNamespace("ignore")
@avroNamespace("ignore")
case class AuNsIgnored(i: Int)
@AvroNamespace("com.yuval")
@avroNamespace("com.yuval")
case class AuNsFieldWins(
    s: String,
    @AvroNamespace("com.yuval.internal") @avroNamespace("com.yuval.internal") internal: AuNsIgnored
)
@AvroNamespace("wobble")
@avroNamespace("wobble")
sealed trait AuAfrica
case object AuCameroon extends AuAfrica
case object AuChad extends AuAfrica
case object AuComoros extends AuAfrica
@AvroNamespace("")
@avroNamespace("")
case class AuEmptyNs(s: String)
sealed trait AuStark
case class AuSansa(i: Int) extends AuStark
case class AuBran(s: String) extends AuStark
case class AuMyStark(@AvroNamespace("the.north") @avroNamespace("the.north") stark: AuStark, id: String, x: Int)
@AvroNamespace("")
@avroNamespace("")
sealed trait AuCosmos
@AvroNamespace("")
@avroNamespace("")
case class AuFunCosmos(amountOfFun: Float) extends AuCosmos
@AvroNamespace("")
@avroNamespace("")
case class AuMiserableCosmos(isTrulyAwful: Boolean) extends AuCosmos
@AvroNamespace("")
@avroNamespace("")
case class AuSpaceship(cosmos: AuCosmos)
case class AuSnoutley(name: String)
case class AuCupcat(@AvroNamespace("cup.cat") @avroNamespace("cup.cat") snoutley: Option[AuSnoutley])
// schema/NamespaceSchemaTest: "case classes should inherit namespace from parent sealed trait"
@AvroNamespace("foobar")
@avroNamespace("foobar")
@AvroName("Qux")
@avroName("Qux")
sealed trait AuNsParent
object AuNsParent {
  case class Bla() extends AuNsParent
}
case class AuUsesNsParent(p: AuNsParent)
// schema/NamespaceSchemaTest: "use namespace of object for classes defined inside an object"
case class AuInObjectUser(inner: AuInObject.Inner)
object AuInObject {
  final case class Inner(s: String)
}

// --- schema/AvroPropSchemaTest ---
@AvroProp("cold", "play")
@avroProp("cold", "play")
case class AuProps(
    @AvroProp("cold", "play") @avroProp("cold", "play") str: String,
    @AvroProp("kate", "bush") @avroProp("kate", "bush") long: Long,
    int: Int
)

// --- schema/AvroSortPrioritySchemaTest, schema/AvroUnionPositionSchemaTest ---
sealed trait AuNumeric
@AvroSortPriority(1)
@avroSortPriority(1)
case object AuRationalNumber extends AuNumeric
@AvroSortPriority(0)
@avroSortPriority(0)
case object AuRealNumber extends AuNumeric
@AvroSortPriority(2)
@avroSortPriority(2)
case object AuNaturalNumber extends AuNumeric
sealed trait AuFightingStyle
@AvroSortPriority(2)
@avroSortPriority(2)
case class AuAggressive(agressiveness: Float) extends AuFightingStyle
@AvroSortPriority(10)
@avroSortPriority(10)
case class AuDefensive(has_armor: Boolean) extends AuFightingStyle
case class AuFightingStyleWrapper(fightingstyle: AuFightingStyle)

sealed trait AuPosNumeric
@AvroUnionPosition(2)
@avroUnionPosition(2)
case object AuPosRational extends AuPosNumeric
@AvroUnionPosition(3)
@avroUnionPosition(3)
case object AuPosReal extends AuPosNumeric
@AvroUnionPosition(1)
@avroUnionPosition(1)
case object AuPosNatural extends AuPosNumeric
sealed trait AuPosStyle
@AvroUnionPosition(2)
@avroUnionPosition(2)
case class AuPosAggressive(agressiveness: Float) extends AuPosStyle
@AvroUnionPosition(1)
@avroUnionPosition(1)
case class AuPosDefensive(has_armor: Boolean) extends AuPosStyle
case class AuPosStyleWrapper(fightingstyle: AuPosStyle)

// --- schema/ByteArraySchemaTest, record/{encoder,decoder}/ByteArray*Test ---
case class AuByteArray(z: Array[Byte])
case class AuByteSeq(z: Seq[Byte])
case class AuByteList(z: List[Byte])
case class AuByteVector(z: Vector[Byte])
case class AuByteBuffer(z: java.nio.ByteBuffer)

// --- schema/ArraySchemaTest, record/{encoder,decoder}/Array*Test ---
case class AuArrRecord(str: String, double: Double)
case class AuArrays(
    booleans: Vector[Boolean],
    records: Vector[AuArrRecord],
    seq: Seq[String],
    array: Array[Boolean],
    arrayOfRecords: Array[AuArrRecord],
    set: Set[AuArrRecord],
    setOfStrings: Set[String],
    setOfDoubles: Set[Double]
)
case class AuTupleA(parameter: Int)
case class AuTupleB(parameter: Int)
case class AuSeqTuple2(first: String, second: Seq[(AuTupleA, AuTupleB)])
case class AuCollectionsOfMaps(
    array: Array[Map[String, String]],
    list: List[Map[String, String]],
    seq: Seq[Map[String, String]],
    vector: Vector[Map[String, String]]
)

// --- schema/DefaultValueSchemaTest, schema/DefaultValueRecordTest, schema/UUIDSchemaTest ---
case class AuDefaultPrimitives(
    s: String = "foo",
    i: Int = 123,
    b: Boolean = true,
    d: Double = 123.456,
    l: Long = 1468920998000L,
    f: Float = 123.458f
)
case class AuDefaultInstants(
    min: java.time.Instant = java.time.Instant.MIN,
    max: java.time.Instant = java.time.Instant.MAX,
    epoch: java.time.Instant = java.time.Instant.EPOCH
)
case class AuSong(title: String)
case class AuDefaultValues(
    name: String = "sammy",
    age: Int = 21,
    isFemale: Boolean = false,
    length: Double = 6.2,
    timestamp: Long = 1468920998000L,
    address: Map[String, String] = Map("home" -> "sammy's home address", "work" -> "sammy's work address"),
    traits: Seq[String] = Seq("Adventurous", "Helpful"),
    luckyNumbers: Set[Int] = Set(7, 9),
    favoriteSongs: Set[AuSong] = Set.empty[AuSong]
)
case class AuOptionalDefaultValues(
    id: Option[String] = None,
    name: Option[String] = Some("sammy"),
    age: Option[Int] = Some(21),
    isFemale: Option[Boolean] = Some(false),
    length: Option[Double] = Some(6.2),
    timestamp: Option[Long] = Some(1468920998000L),
    address: Option[Map[String, String]] = Some(
      Map("home" -> "sammy's home address", "work" -> "sammy's work address")
    ),
    traits: Option[Seq[String]] = Some(Seq("Adventurous", "Helpful")),
    luckyNumbers: Option[Set[Int]] = Some(Set(7, 9)),
    favoriteSongs: Option[Set[AuSong]] = Some(Set.empty[AuSong])
)
sealed trait AuDog
case class AuUnderDog(how_unfortunate: Double) extends AuDog
case class AuUpperDog(how_fortunate: Double) extends AuDog
case class AuDogProspect(dog: Option[AuDog] = None)
sealed trait AuCupcatAdt
case object AuRendal extends AuCupcatAdt
case class AuSnoutleyAdt(snoutley: String, age: Option[Int]) extends AuCupcatAdt
case class AuCuppers(cupcat: AuCupcatAdt = AuSnoutleyAdt("hates varg", None))
case class AuNoVarg(cupcat: AuCupcatAdt = AuRendal)
case class AuUuidDefault(uuid: java.util.UUID = java.util.UUID.fromString("86da265c-95bd-443c-8860-9381efca059d"))
case class AuUuids(uuids: Seq[java.util.UUID], uuid: Option[java.util.UUID])
case class AuBigDecimalDefault(decimal: BigDecimal = 964.55)
case class AuBigDecimals(biggies: Seq[BigDecimal], optional: Seq[Option[BigDecimal]], opt: Option[BigDecimal])
// github/Github411
case class AuDefaultOfDefault(value: String)
case class AuDefaultValue(property: AuDefaultOfDefault = AuDefaultOfDefault("some-default"))
case class AuDefaultsInDefaults(property: AuDefaultValue = AuDefaultValue())
// github/GithubIssue890
case class AuInnerDefaultNone(a: Option[String] = None)
case class AuNestedDefaultNone(b: AuInnerDefaultNone = AuInnerDefaultNone())
// schema/OptionSchemaTest, record/decoder/OptionDecoderTest
case class AuOptionWithDefault(name: Option[String] = Some("f"))
sealed trait AuOptionEnum
case object AuCuppersOptionEnum extends AuOptionEnum
case object AuSnoutleyOptionEnum extends AuOptionEnum
case class AuOptionDefaults(
    s: Option[String] = Option("cupcat"),
    e: Option[AuOptionEnum] = Option(AuCuppersOptionEnum),
    t: String
)
case class AuOptionOfSeqOfCaseClass(foo: Option[Seq[AuSong]])
// schema/EnumSchemaTest (sealed trait enums as field defaults)
sealed trait AuCupcatEnum
@AvroSortPriority(0)
@avroSortPriority(0)
case object AuSnoutleyEnum extends AuCupcatEnum
@AvroSortPriority(1)
@avroSortPriority(1)
case object AuCuppersEnum extends AuCupcatEnum
case class AuEnumDefaults(
    cupcat: AuCupcatEnum = AuCuppersEnum,
    none: Option[AuCupcatEnum] = None,
    some: Option[AuCupcatEnum] = Option(AuSnoutleyEnum)
)

// --- schema/EitherSchemaTest, record/{encoder,decoder}/Either*Test ---
@AvroNamespace("mm")
@avroNamespace("mm")
case class AuGoo(s: String)
@AvroNamespace("nn")
@avroNamespace("nn")
case class AuFoo(b: Boolean)
case class AuEitherRecords(either: Either[AuGoo, AuFoo], prim: Either[String, Double])
case class AuEitherCollections(
    seqOrMap: Either[Seq[String], Map[String, Int]],
    bytesOrString: Either[Array[Byte], String],
    longOrInt: Either[Long, Int],
    uuidOrBoolean: Either[java.util.UUID, Boolean]
)

// --- schema/GenericSchemaTest ---
case class AuThing2Id(id: java.util.UUID) extends AnyVal
case class AuInnerId(id: String) extends AnyVal
case class AuGenInner[LEFT, RIGHT](id: AuInnerId, left: LEFT, right: Option[RIGHT] = None)
case class AuGenOuter[LEFT, RIGHT](
    id: AuThing2Id,
    @AvroName("innerThing") @fieldName("innerThing") theInnerThing: AuGenInner[LEFT, RIGHT]
)
case class AuGeneric[T](t: T)
case class AuSameGeneric(gi: AuGeneric[Int], gs: AuGeneric[String])
@AvroErasedName
@avroErasedName
case class AuGenericDisabled[T](t: T)
case class AuSameGenericDisabled(gi: AuGenericDisabled[Int], gs: AuGenericDisabled[String])
object AuFirst {
  case class TypeArg(a: String)
}
object AuSecond {
  @AvroName("supertypearg")
  @avroName("supertypearg")
  case class TypeArg(a: String)
}
case class AuDibDabDob(a: AuGeneric[AuFirst.TypeArg], b: AuGeneric[AuSecond.TypeArg])
case class AuGenericOfCollections(a: AuGeneric[List[Int]], b: AuGeneric[Option[String]], c: AuGeneric[Map[String, Int]])

// --- schema/MapSchemaTest ---
case class AuMaps(
    strings: Map[String, String],
    options: Map[String, Option[Boolean]],
    seqs: Map[String, Seq[String]],
    records: Map[String, Seq[AuSong]]
)

// --- schema/OptionSchemaTest ---
sealed trait AuOptT
case class AuOptA(a: String) extends AuOptT
case class AuOptB(b: String) extends AuOptT
case class AuOptC(c: String) extends AuOptT
case class AuOptions(
    either: Option[Either[String, Boolean]],
    enumm: Option[AuOptionEnum],
    adt: Option[AuOptT]
)
case class AuNone(option: None.type)
// github/GithubIssue885
sealed trait AuSingleSubtype
case class AuOnlySubtype(v: String) extends AuSingleSubtype

// --- schema/SealedTraitSchemaTest, record/{encoder,decoder}/SealedTrait*Test ---
sealed trait AuDibble
case object AuDabble extends AuDibble
@AvroName("Dobble")
@avroName("Dobble")
case object AuDobbles extends AuDibble
sealed trait AuTibble
case class AuTabble(str: Double, age: Int) extends AuTibble
case class AuTobble(str: String, place: String) extends AuTibble
case class AuTrapper(tibble: AuTibble)
sealed trait AuNibble
case class AuNabble(str: String, age: Int) extends AuNibble
case class AuNobble(str: String, place: String) extends AuNibble
case class AuNapper(nibble: AuNibble)
// github/Github254
sealed trait AuC254
case object c1 extends AuC254
case object c2 extends AuC254
case class AuA254(c: AuC254)
case class AuB254(as: List[AuA254])
// github/GithubIssue281
sealed trait AuInnerTrait
case class AuInnerTraitConcrete(v: Int) extends AuInnerTrait
case class AuInnerTraitConcrete2(v: Int) extends AuInnerTrait
case class AuInner281(t: AuInnerTrait)
case class AuOuter281(inners: Seq[AuInner281])

// --- schema/TransientSchemaTest, record/decoder/TransientDecoderTest ---
case class AuTransient(a: String, @AvroTransient @transientField b: Option[String], c: String)

// --- schema/TupleSchemaTest, record/{encoder,decoder}/Tuple*Test ---
case class AuTuple4(z: (String, Int, Boolean, Double))
case class AuTuple5(z: (String, Int, Boolean, Double, Long))
case class AuTupleSeqs(z: (Seq[String], Option[Int]))

// --- schema/UtfSchemaTest, record/StringDecoderTest ---
case class AuUtf8(name: org.apache.avro.util.Utf8, age: Int)
case class AuCharSeq(a: CharSequence)

// --- schema/FieldMapperFieldTest ---
case class AuNamingStrategy2(camelCase: String)
case class AuNamingStrategy(
    camelCase: String,
    lower: String,
    multipleWordsInThis: String,
    StartsWithUpper: String,
    nested: AuNamingStrategy2
)
// github/Github396: "@AvroName should override fieldmapping"
case class AuFieldMapperWithName(
    @AvroName("inner_field_name") @fieldName("inner_field_name") fooBar: String,
    bazQux: Int
)

// --- schema/AvroNoDefaultTest ---
case class AuNoDefaultTest(@AvroNoDefault @avroNoDefault a: String = "foowoo")

// --- github/GithubIssue12: private vals ---
case class AuSalesRecord(saleId: Int, private val createDateStr: String, tML: Option[String])

// --- record/decoder/SchemaEvolutionTest, github/GithubIssue110, record/FromRecordTest ---
case class AuP1(name: String, age: Int = 18)
case class AuP2(name: String)
case class AuOptionalStringTest(a: String, b: Option[String])
case class AuDefaultStringTest(a: String, b: String = "foo")

// --- record/{encoder,decoder}/DateEncoderTest, github/GithubIssue387 ---
case class AuLocalTime(s: java.time.LocalTime)
case class AuLocalDateTime(s: java.time.LocalDateTime)

// --- schema/EnumSchemaTest: scala.Enumeration (avro4s 4 only) ---
object AuColours extends Enumeration {
  val Red, Amber, Green = Value
}
case class AuScalaEnums(
    colours: AuColours.Value,
    withDefault: AuColours.Value = AuColours.Red,
    optional: Option[AuColours.Value],
    optionalNone: Option[AuColours.Value] = None,
    optionalSome: Option[AuColours.Value] = Option(AuColours.Red)
)
