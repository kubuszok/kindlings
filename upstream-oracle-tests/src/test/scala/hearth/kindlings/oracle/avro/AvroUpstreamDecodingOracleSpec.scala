package hearth.kindlings.oracle.avro

import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder}
import org.apache.avro.{Conversions, LogicalTypes, Schema}
import org.apache.avro.generic.GenericData
import org.apache.avro.util.Utf8

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** avro4s' decoder tests (avro4s-core/src/test/scala/com/sksamuel/avro4s/record/decoder), replayed against Kindlings:
  * the same hand-built Avro values are decoded by both libraries. avro4s' decoders accept whatever holds a value (a
  * String as a `Utf8`, a `String`, bytes or a FIXED value; bytes as a ByteBuffer, an array or a FIXED value; an ARRAY
  * as a Java collection or an array; ...), regardless of the schema - and so do Kindlings'.
  *
  * Each case lists the inputs for which the outcomes differ; everywhere Kindlings accepts more than avro4s (the inputs
  * avro4s 4 and 5 accept differ too), except for one avro4s quirk (`null` decoded as `false`).
  */
final class AvroUpstreamDecodingOracleSpec extends AvroOracleSuite {

  private def k[A](implicit e: AvroEncoder[A], d: AvroDecoder[A]): AvroSide[A] = AvroSide.kindlings[A]

  private object K {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults
    lazy val fooString = k[AuFooString]
    lazy val bytes = k[AuDecBytes]
    lazy val byteVector = k[AuDecByteVector]
    lazy val byteBuffer = k[AuDecByteBuffer]
    lazy val strings = k[AuDecStrings]
    lazy val booleans = k[AuDecBooleans]
    lazy val map = k[AuDecMap]
    lazy val uuid = k[AuDecUuid]
    lazy val localDate = k[AuDecLocalDate]
    lazy val localTime = k[AuDecLocalTime]
    lazy val instant = k[AuDecInstant]
    lazy val bigDecimal = k[AuDecBigDecimal]
    lazy val enumm = k[AuDecEnum]
    lazy val either = k[AuDecEither]
    lazy val option = k[AuDecOption]
    lazy val long = k[AuDecLong]
    lazy val double = k[AuDecDouble]
    lazy val float = k[AuDecFloat]
    lazy val int = k[AuDecInt]
    lazy val boolean = k[AuDecBoolean]
    lazy val p1 = k[AuP1]
    lazy val p2 = k[AuP2]
    lazy val optionalString = k[AuOptionalStringTest]
    lazy val defaultString = k[AuDefaultStringTest]
    lazy val fixedString = k[AuFixedString]
  }

  private object U {
    lazy val fooString = u[AuFooString]
    lazy val bytes = u[AuDecBytes]
    lazy val byteVector = u[AuDecByteVector]
    lazy val byteBuffer = u[AuDecByteBuffer]
    lazy val strings = u[AuDecStrings]
    lazy val booleans = u[AuDecBooleans]
    lazy val map = u[AuDecMap]
    lazy val uuid = u[AuDecUuid]
    lazy val localDate = u[AuDecLocalDate]
    lazy val localTime = u[AuDecLocalTime]
    lazy val instant = u[AuDecInstant]
    lazy val bigDecimal = u[AuDecBigDecimal]
    lazy val enumm = u[AuDecEnum]
    lazy val either = u[AuDecEither]
    lazy val option = u[AuDecOption]
    lazy val long = u[AuDecLong]
    lazy val double = u[AuDecDouble]
    lazy val float = u[AuDecFloat]
    lazy val int = u[AuDecInt]
    lazy val boolean = u[AuDecBoolean]
    lazy val p1 = u[AuP1]
    lazy val p2 = u[AuP2]
    lazy val optionalString = u[AuOptionalStringTest]
    lazy val defaultString = u[AuDefaultStringTest]
    lazy val fixedString = u[AuFixedString]
  }

  private val KindlingsAcceptsMore = "kindlings accepts, avro4s rejects"
  private val Avro4sAcceptsMore = "kindlings rejects, avro4s accepts"
  private val DifferentValues = "both accept, different values"

  private def byVersion[A](avro4s4: Seq[A], avro4s5: Seq[A]): Seq[A] = if (isAvro4s5) avro4s5 else avro4s4

  /** Decodes, with both sides, records of the side's schema whose `field` is each of `inputs` in turn, and asserts the
    * exact list of inputs (by index) for which the outcomes differ.
    */
  private def dec[A](
      name: String,
      k: => AvroSide[A],
      u: => AvroSide[A],
      field: String,
      eq: (A, A) => Boolean = (a: A, b: A) => a == b
  )(inputs: Any*)(expectedDifferences: (Int, String)*): Unit =
    test(name) {
      def decode(side: AvroSide[A], input: Any): Try[A] = Try {
        val r = new GenericData.Record(side.schema)
        // a fresh view per decoder: reading a buffer may move its position
        r.put(
          field,
          input match {
            case buffer: ByteBuffer => buffer.duplicate()
            case other              => other
          }
        )
        side.decode(r)
      }
      val differences = inputs.toList.zipWithIndex.flatMap { case (input, index) =>
        (decode(k, input), decode(u, input)) match {
          case (Success(a), Success(b)) if eq(a, b) => None
          case (Success(_), Success(_))             => Some(index -> DifferentValues)
          case (Failure(_), Failure(_))             => None
          case (Success(_), Failure(_))             => Some(index -> KindlingsAcceptsMore)
          case (Failure(_), Success(_))             => Some(index -> Avro4sAcceptsMore)
        }
      }
      assertEquals(differences, expectedDifferences.toList)
    }

  private def fixed(size: Int, bytes: Array[Byte]): GenericData.Fixed =
    new GenericData.Fixed(Schema.createFixed("f", null, null, size), bytes)

  group("avro4s decoder tests: strings") {
    dec("StringDecoderTest", K.fooString, U.fooString, "str")(
      "hello",
      new Utf8("hello"),
      "hello".subSequence(1, 2),
      ByteBuffer.wrap("hello".getBytes(UTF_8)),
      "hello".getBytes(UTF_8),
      fixed(5, "hello".getBytes(UTF_8)),
      Integer.valueOf(1),
      null,
      java.lang.Boolean.TRUE
    )(
      // avro4s 4 does not decode a String from a ByteBuffer (avro4s 5 does)
      byVersion(avro4s4 = Seq(3 -> KindlingsAcceptsMore), avro4s5 = Nil)*
    )
    dec("UUIDDecoderTest", K.uuid, U.uuid, "uuid")(
      "86da265c-95bd-443c-8860-9381efca059d",
      new Utf8("86da265c-95bd-443c-8860-9381efca059d"),
      "nope",
      null
    )()
    dec("FixedDecoderTest: bytes to String", K.fixedString, U.fixedString, "mystring")(
      fixed(7, Array[Byte](115, 97, 109, 0, 0, 0, 0)),
      "sam".getBytes(UTF_8),
      "sam"
    )()
  }

  group("avro4s decoder tests: bytes") {
    dec[AuDecBytes]("ByteArrayDecoderTest: Array[Byte]", K.bytes, U.bytes, "z", (a, b) => a.z.toList == b.z.toList)(
      Array[Byte](1, 4, 9),
      ByteBuffer.wrap(Array[Byte](1, 4, 9)),
      ByteBuffer.wrap(Array[Byte](1, 4, 9)).asReadOnlyBuffer(),
      fixed(3, Array[Byte](1, 4, 9)),
      "x",
      null
    )(
      // a read-only buffer: avro4s 4 fails (ReadOnlyBufferException)
      byVersion(avro4s4 = Seq(2 -> KindlingsAcceptsMore), avro4s5 = Nil)*
    )
    dec("ByteArrayDecoderTest: Vector[Byte]", K.byteVector, U.byteVector, "z")(
      Array[Byte](1, 4, 9),
      ByteBuffer.wrap(Array[Byte](1, 4, 9)),
      ByteBuffer.wrap(Array[Byte](1, 4, 9)).asReadOnlyBuffer()
    )(
      byVersion(avro4s4 = Seq(2 -> KindlingsAcceptsMore), avro4s5 = Nil)*
    )
    dec[AuDecByteBuffer](
      "ByteArrayDecoderTest: ByteBuffer",
      K.byteBuffer,
      U.byteBuffer,
      "z",
      (a, b) => a.z.duplicate() == b.z.duplicate()
    )(
      Array[Byte](1, 4, 9),
      ByteBuffer.wrap(Array[Byte](1, 4, 9)),
      fixed(3, Array[Byte](1, 4, 9))
    )(
      // avro4s 4 does not decode a ByteBuffer from a FIXED value (avro4s 5 does)
      byVersion(avro4s4 = Seq(2 -> KindlingsAcceptsMore), avro4s5 = Nil)*
    )
  }

  group("avro4s decoder tests: collections") {
    dec("ArrayDecoderTest: Seq[String]", K.strings, U.strings, "strings")(
      List("a", "34", "fgD").asJava,
      Array("a", "34", "fgD"),
      List(new Utf8("a")).asJava,
      new GenericData.Array[Any](Schema.createArray(Schema.create(Schema.Type.STRING)), List[Any]("a").asJava),
      "x",
      null
    )()
    dec("ArrayDecoderTest: Vector[Boolean]", K.booleans, U.booleans, "booleans")(
      List(true, false, true).asJava,
      Array(true, false, true),
      Array[java.lang.Boolean](true, false)
    )()
    dec("MapDecoderTest", K.map, U.map, "map")(
      Map("a" -> "b").asJava,
      Map(new Utf8("a") -> new Utf8("b")).asJava,
      Map("a" -> 1).asJava,
      "x",
      null
    )()
  }

  group("avro4s decoder tests: primitives") {
    dec("BasicDecoderTest: longs", K.long, U.long, "l")(
      java.lang.Long.valueOf(1L),
      Integer.valueOf(1),
      java.lang.Short.valueOf(1.toShort),
      java.lang.Byte.valueOf(1.toByte),
      java.lang.Double.valueOf(1.0),
      "1",
      null
    )()
    dec("BasicDecoderTest: doubles", K.double, U.double, "d")(
      java.lang.Double.valueOf(1.5),
      java.lang.Float.valueOf(1.5f),
      Integer.valueOf(1),
      java.lang.Long.valueOf(1L),
      "1.5",
      null
    )()
    dec("BasicDecoderTest: floats", K.float, U.float, "f")(
      java.lang.Float.valueOf(1.5f),
      java.lang.Double.valueOf(1.5),
      Integer.valueOf(1),
      null
    )()
    dec("BasicDecoderTest: ints", K.int, U.int, "i")(
      Integer.valueOf(1),
      java.lang.Long.valueOf(1L),
      java.lang.Short.valueOf(1.toShort),
      java.lang.Byte.valueOf(1.toByte),
      "1",
      null
    )()
    dec("BasicDecoderTest: booleans", K.boolean, U.boolean, "b")(
      java.lang.Boolean.TRUE,
      "true",
      Integer.valueOf(1),
      null
    )(
      // avro4s unboxes a `null` Boolean as `false`; Kindlings rejects `null` for every primitive
      // (feature-parity.md, avro decoding)
      3 -> Avro4sAcceptsMore
    )
  }

  group("avro4s decoder tests: logical types") {
    dec("DateDecoderTest: LocalDate", K.localDate, U.localDate, "date")(
      Integer.valueOf(17784),
      java.lang.Long.valueOf(17784L),
      java.time.LocalDate.of(2018, 9, 10),
      "x"
    )()
    dec("DateDecoderTest: LocalTime", K.localTime, U.localTime, "time")(
      java.lang.Long.valueOf(46245000000L),
      Integer.valueOf(46245000),
      "x"
    )()
    dec("DateDecoderTest: Instant", K.instant, U.instant, "instant")(
      java.lang.Long.valueOf(1538312231000L),
      Integer.valueOf(5),
      java.time.Instant.ofEpochMilli(5),
      "x"
    )()
    val decimal = LogicalTypes.decimal(8, 2)
    val decimalBytes = new Conversions.DecimalConversion().toBytes(BigDecimal("123.45").underlying(), null, decimal)
    dec("BigDecimalDecoderTest", K.bigDecimal, U.bigDecimal, "decimal")(
      decimalBytes,
      decimalBytes.array(),
      "123.45",
      new Utf8("123.45"),
      java.lang.Long.valueOf(5L),
      java.lang.Double.valueOf(1.5),
      fixed(2, Array[Byte](48, 57))
    )(
      // avro4s 5 decodes a decimal from a FIXED value only when the schema is FIXED (avro4s 4 always does)
      byVersion(avro4s4 = Nil, avro4s5 = Seq(6 -> KindlingsAcceptsMore))*
    )
  }

  group("avro4s decoder tests: sum types") {
    def enumSchema = U.enumm.schema.getField("color").schema()
    dec("EnumDecoderTest / SealedTraitDecoderTest: enum", K.enumm, U.enumm, "color")(
      "AuCuppersOptionEnum",
      new Utf8("AuSnoutleyOptionEnum"),
      "nope",
      null
    )(
      // Kindlings reads the symbol from anything that prints as it; avro4s 4 needs an EnumSymbol or a String, avro4s 5
      // an EnumSymbol
      byVersion(
        avro4s4 = Seq(1 -> KindlingsAcceptsMore),
        avro4s5 = Seq(0 -> KindlingsAcceptsMore, 1 -> KindlingsAcceptsMore)
      )*
    )
    test("EnumDecoderTest: enum symbols") {
      def input(symbol: String) = {
        val r = new GenericData.Record(U.enumm.schema)
        r.put("color", new GenericData.EnumSymbol(enumSchema, symbol))
        r
      }
      assertDecodes(K.enumm, U.enumm, input("AuCuppersOptionEnum"))()
      assertDecodes(K.enumm, U.enumm, input("nope"))()
    }
    dec("EitherDecoderTest", K.either, U.either, "either")(
      new Utf8("a"),
      "a",
      java.lang.Double.valueOf(1.5),
      Integer.valueOf(1),
      java.lang.Boolean.TRUE,
      null
    )()
    dec("OptionDecoderTest", K.option, U.option, "o")("hello", new Utf8("hello"), null, Integer.valueOf(1))()
  }

  group("avro4s decoder tests: schema evolution") {
    test("SchemaEvolutionTest / GithubIssue110: missing field with a scala default") {
      assertDecodes(K.p1, U.p1, U.p2.encode(AuP2("foo")))()
      assertEquals(K.p1.decode(K.p2.encode(AuP2("foo"))), AuP1("foo", 18))
    }
    test("SchemaEvolutionTest: missing field, default from the schema / None for an Option") {
      val schema = org.apache.avro.SchemaBuilder.record("foo").fields().requiredString("a").endRecord()
      val record = new GenericData.Record(schema)
      record.put("a", new Utf8("hello"))
      assertDecodes(K.defaultString, U.defaultString, record)()
      assertDecodes(K.optionalString, U.optionalString, record)()
      assertEquals(K.defaultString.decode(record), AuDefaultStringTest("hello"))
      assertEquals(K.optionalString.decode(record), AuOptionalStringTest("hello", None))
    }
  }
}
