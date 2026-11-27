package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{AvroName, AvroNamespace, AvroProp}
import hearth.kindlings.avroderivation.annotations.{avroName, avroNamespace, avroProp, fieldName}

// More fixtures mirroring avro4s' own test suites (see avroUpstreamTestFixtures.scala).

// --- field-level @AvroNamespace applies to every named type below the field (SchemaHelper.overrideNamespace) ---
case class AuNsDeepLeaf(i: Int)
case class AuNsDeepMid(
    leaf: AuNsDeepLeaf,
    leaves: List[AuNsDeepLeaf],
    letter: AuOptionEnum,
    byKey: Map[String, AuNsDeepLeaf],
    maybe: Option[AuNsDeepLeaf]
)
case class AuNsDeep(@AvroNamespace("deep.ns") @avroNamespace("deep.ns") mid: AuNsDeepMid, plain: AuNsDeepLeaf)

// --- schema/EnumSchemaTest: java enums ---
case class AuJavaEnums(
    wine: AuWine,
    withDefault: AuWine = AuWine.CabSav,
    optional: Option[AuWine],
    optionalNone: Option[AuWine] = None,
    optionalSome: Option[AuWine] = Some(AuWine.CabSav)
)

// --- record/decoder/*: hand-built inputs ---
case class AuFooString(str: String)
case class AuDecBytes(z: Array[Byte])
case class AuDecByteVector(z: Vector[Byte])
case class AuDecByteBuffer(z: java.nio.ByteBuffer)
case class AuDecStrings(strings: Seq[String])
case class AuDecBooleans(booleans: Vector[Boolean])
case class AuDecMap(map: Map[String, String])
case class AuDecUuid(uuid: java.util.UUID)
case class AuDecLocalDate(date: java.time.LocalDate)
case class AuDecLocalTime(time: java.time.LocalTime)
case class AuDecInstant(instant: java.time.Instant)
case class AuDecBigDecimal(decimal: BigDecimal)
case class AuDecEnum(color: AuOptionEnum)
case class AuDecEither(either: Either[String, Double])
case class AuDecOption(o: Option[String])
case class AuDecLong(l: Long)
case class AuDecDouble(d: Double)
case class AuDecFloat(f: Float)
case class AuDecInt(i: Int)
case class AuDecBoolean(b: Boolean)

// --- github/Github396: @AvroName overrides the field mapper ---
case class AuMapperWithName(@AvroName("inner_field_name") @fieldName("inner_field_name") fooBar: String, bazQux: Int)

// --- record/decoder/SealedTraitDecoderTest: "use @AvroNamespace and @AvroName with sealed traits of case objects" ---
@AvroNamespace("market")
@avroNamespace("market")
@AvroName("Fruit")
@avroName("Fruit")
sealed trait AuFruit
@AvroName("apple")
@avroName("apple")
case object AuApple extends AuFruit
@AvroName("orange")
@avroName("orange")
case object AuOrange extends AuFruit
case class AuBasket(fruit: AuFruit, more: List[AuFruit])

// --- @AvroProp values that look like JSON ---
case class AuPropValues(
    @AvroProp("text", "plain") @avroProp("text", "plain") a: String,
    @AvroProp("number", "1") @avroProp("number", "1") b: String,
    @AvroProp("flag", "true") @avroProp("flag", "true") c: String
)

// --- record/encoder/BigDecimalEncoderTest, record/BigDecimalRoundTrip, streams ---
case class AuDecimal(decimal: BigDecimal)
case class AuComposer(name: String, birthplace: String, compositions: Seq[String])
