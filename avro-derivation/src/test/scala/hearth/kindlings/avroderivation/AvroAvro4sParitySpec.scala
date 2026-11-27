package hearth.kindlings.avroderivation

import hearth.MacroSuite
import hearth.kindlings.avroderivation.annotations.*
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.util.Utf8

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

// Fixtures of AvroAvro4sParitySpec (top level, so the schemas' namespace is the package).

sealed trait PaOcean
@avroName("atlan") case object PaAtlantic extends PaOcean
@avroName("pacif") case object PaPacific extends PaOcean
case class PaShip(location: PaOcean, visited: List[PaOcean])

sealed trait PaPosition
@avroUnionPosition(2) case object PaSecond extends PaPosition
@avroUnionPosition(3) case object PaThird extends PaPosition
@avroUnionPosition(1) case object PaFirst extends PaPosition
sealed trait PaStyle
@avroUnionPosition(2) case class PaAggressive(level: Float) extends PaStyle
@avroUnionPosition(1) case class PaDefensive(armor: Boolean) extends PaStyle
@avroSortPriority(5) @avroUnionPosition(9) case class PaPriorityWins(i: Int) extends PaStyle

sealed trait PaStark
case class PaSansa(i: Int) extends PaStark
case class PaBran(s: String) extends PaStark
case class PaLeaf(i: Int)
case class PaMid(leaf: PaLeaf, leaves: List[PaLeaf], maybe: Option[PaLeaf], byKey: Map[String, PaLeaf])
case class PaNamespaced(
    @avroNamespace("the.north") stark: PaStark,
    @avroNamespace("deep.ns") mid: PaMid,
    @avroNamespace("cup.cat") maybe: Option[PaLeaf],
    plain: PaLeaf
)

@avroNamespace("foobar")
sealed trait PaParent
object PaParent {
  case class Inherits(i: Int) extends PaParent
  @avroNamespace("own") case class Own(i: Int) extends PaParent
}

case class PaPrivate(id: Int, private val secret: String, tag: Option[String]) {
  def reveal: String = secret
}

case class PaGeneric[T](t: T)
object PaFirstNs { case class Arg(a: String) }
object PaSecondNs { @avroName("renamed") case class Arg(a: String) }
case class PaGenerics(a: PaGeneric[PaFirstNs.Arg], b: PaGeneric[PaSecondNs.Arg])

case class PaFixedString(@avroFixed(7) s: String)
@avroFixed(8) case class PaSha(bytes: Array[Byte]) extends AnyVal
@avroFixed(8) case class PaCode(value: String) extends AnyVal
case class PaFixedValues(sha: PaSha, code: Option[PaCode])

@avroDoc("an id") case class PaDocumentedId(value: String) extends AnyVal
case class PaUsesDocumentedId(id: PaDocumentedId, @avroDoc("own doc") other: PaDocumentedId)

case class PaTransient(a: String, @transientField cache: Option[String], c: String)

case class PaStrings(utf8: Utf8, chars: CharSequence, s: String)
case class PaBytes(arr: Array[Byte], vec: Vector[Byte], buf: ByteBuffer)
case class PaItems(items: List[Int])
case class PaTime(time: java.time.LocalTime, date: java.time.LocalDate)
case class PaDecimal(@avroScalePrecision(8, 2) amount: BigDecimal)
case class PaInstant(at: java.time.Instant)

case class PaError(code: Int)
case class PaVeryCross(@avroError error: PaError)

object PaColours extends Enumeration {
  val Red, Amber, Green = Value
}

/** Behaviour found by replaying avro4s' own test suites against Kindlings (the differential tests are in
  * `upstream-oracle-tests`: `AvroUpstreamTestsOracleSpec`, `AvroUpstreamDecodingOracleSpec`,
  * `AvroUpstreamExtrasOracleSpec`); these tests pin it down without the upstream dependency.
  */
final class AvroAvro4sParitySpec extends MacroSuite {

  private def roundTrip[A](value: A)(implicit encoder: AvroEncoder[A], decoder: AvroDecoder[A]): A =
    AvroIO.fromBinary[A](AvroIO.toBinary(value))

  private def fixed(bytes: Array[Byte]): GenericData.Fixed =
    new GenericData.Fixed(Schema.createFixed("f", null, null, bytes.length), bytes)

  group("ENUM symbols") {

    test("@avroName on a case object renames its symbol (schema, encoder, decoder)") {
      implicit val encoder: AvroEncoder[PaShip] = AvroEncoder.derived[PaShip]
      implicit val decoder: AvroDecoder[PaShip] = AvroDecoder.derived[PaShip]
      encoder.schema.getField("location").schema.getEnumSymbols.toString ==> "[atlan, pacif]"
      val value = PaShip(PaPacific, List(PaAtlantic, PaPacific))
      encoder.encode(value).asInstanceOf[GenericRecord].get("location").toString ==> "pacif"
      roundTrip(value) ==> value
    }

    test("@avroName on a symbol is not passed through transformConstructorNames") {
      implicit val config: AvroConfig = AvroConfig.default.withTransformConstructorNames(_.toUpperCase)
      AvroSchemaFor.schemaOf[PaOcean].getEnumSymbols.toString ==> "[atlan, pacif]"
    }

    test("@avroUnionPosition orders the symbols of an ENUM by ascending position") {
      AvroSchemaFor.schemaOf[PaPosition].getEnumSymbols.toString ==> "[PaFirst, PaSecond, PaThird]"
    }

    test("@avroUnionPosition orders the members of a UNION; @avroSortPriority takes precedence") {
      import scala.jdk.CollectionConverters.*
      // positions are priorities of `999999 - position`, so a subtype with a (small) @avroSortPriority sorts after them
      AvroSchemaFor.schemaOf[PaStyle].getTypes.asScala.map(_.getName).toList ==>
        List("PaDefensive", "PaAggressive", "PaPriorityWins")
    }

    test("scala.Enumeration: symbols in the order of their ids") {
      AvroSchemaFor.schemaOf[PaColours.Value].getEnumSymbols.toString ==> "[Red, Amber, Green]"
    }
  }

  group("namespaces") {

    test("@avroNamespace on a field moves every named type below the field, and the encoded values with them") {
      implicit val encoder: AvroEncoder[PaNamespaced] = AvroEncoder.derived[PaNamespaced]
      implicit val decoder: AvroDecoder[PaNamespaced] = AvroDecoder.derived[PaNamespaced]
      val schema = encoder.schema
      import scala.jdk.CollectionConverters.*
      schema.getField("stark").schema.getTypes.asScala.map(_.getFullName).toList ==>
        List("the.north.PaBran", "the.north.PaSansa")
      val mid = schema.getField("mid").schema
      mid.getFullName ==> "deep.ns.PaMid"
      mid.getField("leaf").schema.getFullName ==> "deep.ns.PaLeaf"
      mid.getField("leaves").schema.getElementType.getFullName ==> "deep.ns.PaLeaf"
      mid.getField("byKey").schema.getValueType.getFullName ==> "deep.ns.PaLeaf"
      schema.getField("maybe").schema.getTypes.get(1).getFullName ==> "cup.cat.PaLeaf"
      schema.getField("plain").schema.getFullName ==> "hearth.kindlings.avroderivation.PaLeaf"
      // the schema survives a JSON round trip (three different `PaLeaf` records)
      new Schema.Parser().parse(schema.toString) ==> schema

      val value = PaNamespaced(
        PaBran("b"),
        PaMid(PaLeaf(1), List(PaLeaf(2)), Some(PaLeaf(3)), Map("k" -> PaLeaf(4))),
        Some(PaLeaf(5)),
        PaLeaf(6)
      )
      // Avro resolves union branches by the full name of the datum's schema: binary encoding needs the moved names
      roundTrip(value) ==> value
      roundTrip(value.copy(stark = PaSansa(1), maybe = None)) ==> value.copy(stark = PaSansa(1), maybe = None)
      val record = encoder.encode(value).asInstanceOf[GenericRecord]
      record.get("stark").asInstanceOf[GenericRecord].getSchema.getFullName ==> "the.north.PaBran"
    }

    test("a subtype takes the @avroNamespace of its sealed parent, on its own and as a union member") {
      import scala.jdk.CollectionConverters.*
      AvroSchemaFor.schemaOf[PaParent.Inherits].getFullName ==> "foobar.Inherits"
      AvroSchemaFor.schemaOf[PaParent.Own].getFullName ==> "own.Own"
      AvroSchemaFor.schemaOf[PaParent].getTypes.asScala.map(_.getFullName).toList ==>
        List("foobar.Inherits", "own.Own")
    }

    test("a subtype with its sealed parent's @avroNamespace round-trips as a union member") {
      implicit val encoder: AvroEncoder[PaParent] = AvroEncoder.derived[PaParent]
      implicit val decoder: AvroDecoder[PaParent] = AvroDecoder.derived[PaParent]
      roundTrip[PaParent](PaParent.Inherits(1)) ==> PaParent.Inherits(1)
      roundTrip[PaParent](PaParent.Own(2)) ==> PaParent.Own(2)
    }

    test("@avroName of a type argument is used in the generic record's name") {
      val schema = AvroSchemaFor.schemaOf[PaGenerics]
      schema.getField("a").schema.getName ==> "PaGeneric__Arg"
      schema.getField("b").schema.getName ==> "PaGeneric__renamed"
      new Schema.Parser().parse(schema.toString) ==> schema
    }
  }

  group("records") {

    test("a private val field is encoded and decoded") {
      implicit val encoder: AvroEncoder[PaPrivate] = AvroEncoder.derived[PaPrivate]
      implicit val decoder: AvroDecoder[PaPrivate] = AvroDecoder.derived[PaPrivate]
      val record = encoder.encode(PaPrivate(1, "s3cret", Some("t"))).asInstanceOf[GenericRecord]
      record.get("id") ==> 1
      record.get("secret").toString ==> "s3cret"
      record.get("tag").toString ==> "t"
      val decoded = roundTrip(PaPrivate(1, "s3cret", Some("t")))
      decoded ==> PaPrivate(1, "s3cret", Some("t"))
      decoded.reveal ==> "s3cret"
    }

    test("@transientField on an Option without a default decodes as None") {
      implicit val encoder: AvroEncoder[PaTransient] = AvroEncoder.derived[PaTransient]
      implicit val decoder: AvroDecoder[PaTransient] = AvroDecoder.derived[PaTransient]
      import scala.jdk.CollectionConverters.*
      encoder.schema.getFields.asScala.map(_.name).toList ==> List("a", "c")
      roundTrip(PaTransient("a", Some("dropped"), "c")) ==> PaTransient("a", None, "c")
    }

    test("@transientField on a field that is neither optional nor defaulted is a compile error") {
      compileErrors(
        """
        import hearth.kindlings.avroderivation.AvroSchemaFor
        import hearth.kindlings.avroderivation.annotations.transientField
        case class BadTransient(@transientField cache: String)
        AvroSchemaFor.schemaOf[BadTransient]
        """
      ).check("@transientField on field 'cache'", "requires a default value (or an Option type, decoded as None)")
    }

    test("@avroDoc of a value class documents the fields of that type, unless they have their own") {
      val schema = AvroSchemaFor.schemaOf[PaUsesDocumentedId]
      schema.getField("id").doc ==> "an id"
      schema.getField("other").doc ==> "own doc"
    }

    test("@avroError on a field makes the field's record an error record") {
      val schema = AvroSchemaFor.schemaOf[PaVeryCross]
      assert(schema.getField("error").schema.isError)
      assert(!schema.isError)
      assert(!AvroSchemaFor.schemaOf[PaError].isError)
    }
  }

  group("FIXED") {

    test("@avroFixed on a String field: UTF-8 bytes padded with zeros, decoded with the padding") {
      implicit val encoder: AvroEncoder[PaFixedString] = AvroEncoder.derived[PaFixedString]
      implicit val decoder: AvroDecoder[PaFixedString] = AvroDecoder.derived[PaFixedString]
      val schema = encoder.schema.getField("s").schema
      schema.getType ==> Schema.Type.FIXED
      schema.getFixedSize ==> 7
      val record = encoder.encode(PaFixedString("sam")).asInstanceOf[GenericRecord]
      record.get("s").asInstanceOf[GenericData.Fixed].bytes().toList ==> List[Byte](115, 97, 109, 0, 0, 0, 0)
      roundTrip(PaFixedString("sam")) ==> PaFixedString("sam\u0000\u0000\u0000\u0000")
      val _ = intercept[org.apache.avro.AvroRuntimeException](encoder.encode(PaFixedString("too long!")))
    }

    test("@avroFixed on a value class: a FIXED type named after the class") {
      implicit val encoder: AvroEncoder[PaFixedValues] = AvroEncoder.derived[PaFixedValues]
      implicit val decoder: AvroDecoder[PaFixedValues] = AvroDecoder.derived[PaFixedValues]
      val sha = encoder.schema.getField("sha").schema
      sha.getType ==> Schema.Type.FIXED
      sha.getFullName ==> "hearth.kindlings.avroderivation.PaSha"
      sha.getFixedSize ==> 8
      encoder.schema.getField("code").schema.getTypes.get(1).getFullName ==> "hearth.kindlings.avroderivation.PaCode"
      val value = PaFixedValues(PaSha(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)), Some(PaCode("12345678")))
      val decoded = roundTrip(value)
      decoded.sha.bytes.toList ==> value.sha.bytes.toList
      decoded.code ==> value.code
      roundTrip(value.copy(code = None)).code ==> None
    }
  }

  group("strings and bytes") {

    test("Utf8 and CharSequence fields are strings") {
      implicit val encoder: AvroEncoder[PaStrings] = AvroEncoder.derived[PaStrings]
      implicit val decoder: AvroDecoder[PaStrings] = AvroDecoder.derived[PaStrings]
      import scala.jdk.CollectionConverters.*
      encoder.schema.getFields.asScala.map(_.schema.getType).toList ==>
        List(Schema.Type.STRING, Schema.Type.STRING, Schema.Type.STRING)
      val decoded = roundTrip(PaStrings(new Utf8("a"), "b", "c"))
      decoded.utf8 ==> new Utf8("a")
      decoded.chars.toString ==> "b"
      decoded.s ==> "c"
    }

    test("a String is decoded from a CharSequence, bytes or a FIXED value; anything else is rejected") {
      val decoder = AvroDecoder.derived[PaStrings]
      def decode(s: Any): String = {
        val record = new GenericData.Record(decoder.schema)
        record.put("utf8", "u")
        record.put("chars", "c")
        record.put("s", s)
        decoder.decode(record).s
      }
      decode("hello") ==> "hello"
      decode(new Utf8("hello")) ==> "hello"
      decode("hello".getBytes(UTF_8)) ==> "hello"
      decode(ByteBuffer.wrap("hello".getBytes(UTF_8))) ==> "hello"
      decode(fixed("hello".getBytes(UTF_8))) ==> "hello"
      val _ = intercept[org.apache.avro.AvroTypeException](decode(Integer.valueOf(1)))
      val _ = intercept[org.apache.avro.AvroTypeException](decode(null))
    }

    test("bytes are decoded from a ByteBuffer (which keeps its position), a byte array or a FIXED value") {
      val decoder = AvroDecoder.derived[PaBytes]
      def decode(bytes: Any): PaBytes = {
        val record = new GenericData.Record(decoder.schema)
        record.put("arr", bytes)
        record.put("vec", bytes)
        record.put("buf", bytes)
        decoder.decode(record)
      }
      val buffer = ByteBuffer.wrap(Array[Byte](1, 4, 9))
      List[Any](buffer, buffer, buffer.asReadOnlyBuffer(), Array[Byte](1, 4, 9), fixed(Array[Byte](1, 4, 9))).foreach {
        input =>
          val decoded = decode(input)
          decoded.arr.toList ==> List[Byte](1, 4, 9)
          decoded.vec ==> Vector[Byte](1, 4, 9)
          decoded.buf.duplicate() ==> ByteBuffer.wrap(Array[Byte](1, 4, 9))
      }
      val _ = intercept[org.apache.avro.AvroTypeException](decode("x"))
    }

    test("an ARRAY is decoded from a Java collection, a Java array or a Scala collection") {
      val decoder = AvroDecoder.derived[PaItems]
      def decode(items: Any): List[Int] = {
        val record = new GenericData.Record(decoder.schema)
        record.put("items", items)
        decoder.decode(record).items
      }
      decode(java.util.Arrays.asList(1, 2)) ==> List(1, 2)
      decode(Array(1, 2)) ==> List(1, 2)
      decode(Array[Integer](1, 2)) ==> List(1, 2)
      decode(Vector(1, 2)) ==> List(1, 2)
      val _ = intercept[org.apache.avro.AvroTypeException](decode("x"))
    }
  }

  group("logical types") {

    test("time-micros and date are decoded with integral widening") {
      val decoder = AvroDecoder.derived[PaTime]
      val record = new GenericData.Record(decoder.schema)
      record.put("time", Integer.valueOf(46245000))
      record.put("date", Integer.valueOf(17784))
      decoder.decode(record) ==> PaTime(
        java.time.LocalTime.ofNanoOfDay(46245000000L),
        java.time.LocalDate.of(2018, 9, 10)
      )
    }

    test("a decimal is decoded from a ByteBuffer, a byte array or a FIXED value") {
      val decoder = AvroDecoder.derived[PaDecimal]
      def decode(amount: Any): BigDecimal = {
        val record = new GenericData.Record(decoder.schema)
        record.put("amount", amount)
        decoder.decode(record).amount
      }
      val unscaled = BigInt(12345).toByteArray
      decode(ByteBuffer.wrap(unscaled)) ==> BigDecimal("123.45")
      decode(unscaled) ==> BigDecimal("123.45")
      decode(fixed(unscaled)) ==> BigDecimal("123.45")
    }

    test("an Instant out of the range of a long is an ArithmeticException, not a wrapped value") {
      val encoder = AvroEncoder.derived[PaInstant]
      val _ = intercept[ArithmeticException](encoder.encode(PaInstant(java.time.Instant.MAX)))
      val _ = intercept[ArithmeticException](encoder.encode(PaInstant(java.time.Instant.MIN)))
      val nanos = {
        implicit val config: AvroConfig = AvroConfig.default.withInstantPrecision(TimestampPrecision.Nanos)
        AvroEncoder.derived[PaInstant]
      }
      val _ = intercept[ArithmeticException](nanos.encode(PaInstant(java.time.Instant.parse("2300-01-01T00:00:00Z"))))
    }
  }

  group("Either") {

    test("an Either whose side is a union itself (Either[String, Option[Int]])") {
      implicit val encoder: AvroEncoder[Either[String, Option[Int]]] = AvroEncoder.derived
      implicit val decoder: AvroDecoder[Either[String, Option[Int]]] = AvroDecoder.derived
      encoder.schema.toString ==> """["null","string","int"]"""
      List[Either[String, Option[Int]]](Left("a"), Right(Some(1)), Right(None)).foreach { value =>
        roundTrip(value) ==> value
      }
    }
  }

  group("AvroIO data files") {

    test("toDataFile / fromDataFile round trip, with a codec") {
      implicit val encoder: AvroEncoder[PaLeaf] = AvroEncoder.derived[PaLeaf]
      implicit val decoder: AvroDecoder[PaLeaf] = AvroDecoder.derived[PaLeaf]
      val values = List(PaLeaf(1), PaLeaf(2), PaLeaf(3))
      AvroIO.fromDataFile[PaLeaf](AvroIO.toDataFile(values)) ==> values
      val deflated = AvroIO.toDataFile(values, org.apache.avro.file.CodecFactory.deflateCodec(6))
      assert(new String(deflated, "ISO-8859-1").contains("deflate"))
      AvroIO.fromDataFile[PaLeaf](deflated) ==> values
      AvroIO.fromDataFile[PaLeaf](AvroIO.toDataFile(List.empty[PaLeaf])) ==> Nil
    }
  }
}
