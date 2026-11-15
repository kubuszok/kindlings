package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{Decoder as UDecoder, Encoder as UEncoder, ScalePrecision, SchemaFor as USchemaFor}
import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder, TimestampPrecision}
import hearth.kindlings.oracle.Oracle
import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord}

import scala.util.{Failure, Success, Try}

/** Kindlings' avro-derivation vs avro4s (v4 on Scala 2.13, v5 on Scala 3 - each Scala version is compared with its own
  * avro4s line): the schemas (as JSON), the binary encoding of sample values, each side decoding the other's bytes
  * (Avro schema resolution with the writer's schema), and the decoding of hand-built and invalid inputs.
  *
  * Each case lists the *exact* differences it expects; each one must be a documented, intentional difference (see the
  * avro section of `docs/user-guide/feature-parity.md`). A difference that appears or disappears fails the test.
  */
final class AvroOracleSpec extends MacroSuite {

  private val isAvro4s5 = AvroUpstream.majorVersion == 5

  private def schemaDiff(k: AvroSide[?], u: AvroSide[?]): List[String] =
    Oracle.diff(AvroOracle.schemaTree(k.schema), AvroOracle.schemaTree(u.schema)).map("schema " + _)

  /** Encodes `value` with both sides, compares the bytes, and decodes each side's bytes with both sides. */
  private def valueDiff[A](k: AvroSide[A], u: AvroSide[A], value: A, eq: (A, A) => Boolean): List[String] = {
    val kb = AvroOracle.encodeBinary(k, value)
    val ub = AvroOracle.encodeBinary(u, value)
    val bytes = (kb, ub) match {
      case (Success(a), Success(b)) =>
        if (java.util.Arrays.equals(a, b)) Nil
        else List(s"bytes: kindlings=${AvroOracle.hex(a)} upstream=${AvroOracle.hex(b)}")
      case _ => List(s"bytes: kindlings=$kb upstream=$ub")
    }
    def check(label: String, decoded: Try[A]): List[String] = decoded match {
      case Success(d) if eq(d, value) => Nil
      case other                      => List(s"$label: $other")
    }
    val cross = (kb.toOption, ub.toOption) match {
      case (Some(a), Some(b)) =>
        check("kindlings decodes upstream bytes", AvroOracle.decodeBinary(k, u.schema, b)) ++
          check("upstream decodes kindlings bytes", AvroOracle.decodeBinary(u, k.schema, a)) ++
          check("kindlings round trip", AvroOracle.decodeBinary(k, k.schema, a)) ++
          check("upstream round trip", AvroOracle.decodeBinary(u, u.schema, b))
      case _ => Nil
    }
    bytes ++ cross
  }

  private def compareWith[A](k: AvroSide[A], u: AvroSide[A], values: List[A], eq: (A, A) => Boolean)(
      expectedDifferences: String*
  ): Unit = {
    val differences = schemaDiff(k, u) ++ values.flatMap(valueDiff(k, u, _, eq)).distinct
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs avro4s ${AvroUpstream.majorVersion} differences:\n${differences.mkString("\n")}\n" +
        s"kindlings schema: ${k.schema}\nupstream schema:  ${u.schema}"
    )
  }

  private def compare[A](k: AvroSide[A], u: AvroSide[A], values: A*)(expectedDifferences: String*): Unit =
    compareWith(k, u, values.toList, (a: A, b: A) => a == b)(expectedDifferences*)

  /** For documented differences of the default config: only the schemas are compared (the encodings then differ by
    * construction); the same fixture is compared in full under `AvroConfig.avro4sDefaults`.
    */
  private def compareSchemas(k: AvroSide[?], u: AvroSide[?])(expectedDifferences: String*): Unit = {
    val differences = schemaDiff(k, u)
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs avro4s ${AvroUpstream.majorVersion} schema differences:\n${differences.mkString("\n")}"
    )
  }

  private def u[A](implicit s: USchemaFor[A], e: UEncoder[A], d: UDecoder[A]): AvroSide[A] = AvroUpstream.side[A]

  private object K {
    implicit val config: AvroConfig = AvroConfig.default
    lazy val person = AvroSide.kindlings(AvroEncoder.derived[AvPerson], AvroDecoder.derived[AvPerson])
    lazy val primitives = AvroSide.kindlings(AvroEncoder.derived[AvPrimitives], AvroDecoder.derived[AvPrimitives])
    lazy val options = AvroSide.kindlings(AvroEncoder.derived[AvOptions], AvroDecoder.derived[AvOptions])
    lazy val collections = AvroSide.kindlings(AvroEncoder.derived[AvCollections], AvroDecoder.derived[AvCollections])
    lazy val maps = AvroSide.kindlings(AvroEncoder.derived[AvMaps], AvroDecoder.derived[AvMaps])
    lazy val nested = AvroSide.kindlings(AvroEncoder.derived[AvNested], AvroDecoder.derived[AvNested])
    lazy val drawing = AvroSide.kindlings(AvroEncoder.derived[AvDrawing], AvroDecoder.derived[AvDrawing])
    lazy val palette = AvroSide.kindlings(AvroEncoder.derived[AvPalette], AvroDecoder.derived[AvPalette])
    lazy val eithers = AvroSide.kindlings(AvroEncoder.derived[AvEithers], AvroDecoder.derived[AvEithers])
    lazy val tuples = AvroSide.kindlings(AvroEncoder.derived[AvTuples], AvroDecoder.derived[AvTuples])
    lazy val twoTuples = AvroSide.kindlings(AvroEncoder.derived[AvTwoTuples], AvroDecoder.derived[AvTwoTuples])
    lazy val tree = AvroSide.kindlings(AvroEncoder.derived[AvTree], AvroDecoder.derived[AvTree])
    lazy val usesId = AvroSide.kindlings(AvroEncoder.derived[AvUsesId], AvroDecoder.derived[AvUsesId])
    lazy val bytes = AvroSide.kindlings(AvroEncoder.derived[AvBytes], AvroDecoder.derived[AvBytes])
    lazy val money = AvroSide.kindlings(AvroEncoder.derived[AvMoney], AvroDecoder.derived[AvMoney])
    lazy val times = AvroSide.kindlings(AvroEncoder.derived[AvTimes], AvroDecoder.derived[AvTimes])
    lazy val legacyTimes = AvroSide.kindlings(AvroEncoder.derived[AvLegacyTimes], AvroDecoder.derived[AvLegacyTimes])
    lazy val offset = AvroSide.kindlings(AvroEncoder.derived[AvOffset], AvroDecoder.derived[AvOffset])
    lazy val uuid = AvroSide.kindlings(AvroEncoder.derived[AvUuid], AvroDecoder.derived[AvUuid])
    lazy val defaults = AvroSide.kindlings(AvroEncoder.derived[AvDefaults], AvroDecoder.derived[AvDefaults])
    lazy val generics = AvroSide.kindlings(AvroEncoder.derived[AvGenerics], AvroDecoder.derived[AvGenerics])
    lazy val erased = AvroSide.kindlings(AvroEncoder.derived[AvUsesErased], AvroDecoder.derived[AvUsesErased])
    lazy val annotated = AvroSide.kindlings(AvroEncoder.derived[AvAnnotated], AvroDecoder.derived[AvAnnotated])
    lazy val fixed = AvroSide.kindlings(AvroEncoder.derived[AvFixed], AvroDecoder.derived[AvFixed])
    lazy val noDefault = AvroSide.kindlings(AvroEncoder.derived[AvNoDefault], AvroDecoder.derived[AvNoDefault])
    lazy val priority = AvroSide.kindlings(AvroEncoder.derived[AvPriority], AvroDecoder.derived[AvPriority])
    lazy val direction = AvroSide.kindlings(AvroEncoder.derived[AvDirection], AvroDecoder.derived[AvDirection])
    lazy val widenedLong = AvroSide.kindlings(AvroEncoder.derived[AvWidenedLong], AvroDecoder.derived[AvWidenedLong])
    lazy val widenedDouble =
      AvroSide.kindlings(AvroEncoder.derived[AvWidenedDouble], AvroDecoder.derived[AvWidenedDouble])
    lazy val weekday = AvroSide.kindlings(AvroEncoder.derived[AvUsesWeekday], AvroDecoder.derived[AvUsesWeekday])
    lazy val mixed = AvroSide.kindlings(AvroEncoder.derived[AvUsesMixed], AvroDecoder.derived[AvUsesMixed])
    lazy val order = AvroSide.kindlings(AvroEncoder.derived[AvUsesOrder], AvroDecoder.derived[AvUsesOrder])
    lazy val optionalSums =
      AvroSide.kindlings(AvroEncoder.derived[AvOptionalSums], AvroDecoder.derived[AvOptionalSums])
    lazy val fieldNs = AvroSide.kindlings(AvroEncoder.derived[AvFieldNs], AvroDecoder.derived[AvFieldNs])
    lazy val evolV1 = AvroSide.kindlings(AvroEncoder.derived[AvEvolV1], AvroDecoder.derived[AvEvolV1])
    lazy val evolV2 = AvroSide.kindlings(AvroEncoder.derived[AvEvolV2], AvroDecoder.derived[AvEvolV2])
  }

  /** `AvroConfig.avro4sDefaults`: avro4s' behaviour where Kindlings' documented defaults differ. */
  private object KAvro4s {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults
    lazy val money = AvroSide.kindlings(AvroEncoder.derived[AvMoney], AvroDecoder.derived[AvMoney])
    lazy val offset = AvroSide.kindlings(AvroEncoder.derived[AvOffset], AvroDecoder.derived[AvOffset])
  }

  /** The opt-out `withoutScalaDefaultsInSchema` (a Kindlings extension). */
  private object KNoSchemaDefaults {
    implicit val config: AvroConfig = AvroConfig.default.withoutScalaDefaultsInSchema
    lazy val evolV1 = AvroSide.kindlings(AvroEncoder.derived[AvEvolV1], AvroDecoder.derived[AvEvolV1])
    lazy val evolV2 = AvroSide.kindlings(AvroEncoder.derived[AvEvolV2], AvroDecoder.derived[AvEvolV2])
  }

  private object KSnake {
    implicit val config: AvroConfig = AvroConfig.default.withSnakeCaseFieldNames
    lazy val camel = AvroSide.kindlings(AvroEncoder.derived[AvCamel], AvroDecoder.derived[AvCamel])
  }

  private object KDecimal {
    implicit val config: AvroConfig = AvroConfig.default.withDecimalConfig(precision = 10, scale = 4)
    lazy val money = AvroSide.kindlings(AvroEncoder.derived[AvMoney], AvroDecoder.derived[AvMoney])
  }

  private object KMicros {
    implicit val config: AvroConfig = AvroConfig.default.withInstantPrecision(TimestampPrecision.Micros)
    lazy val instants = AvroSide.kindlings(AvroEncoder.derived[AvInstants], AvroDecoder.derived[AvInstants])
  }

  private object U {
    lazy val person = u[AvPerson]
    lazy val primitives = u[AvPrimitives]
    lazy val options = u[AvOptions]
    lazy val collections = u[AvCollections]
    lazy val maps = u[AvMaps]
    lazy val nested = u[AvNested]
    lazy val drawing = u[AvDrawing]
    lazy val palette = u[AvPalette]
    lazy val eithers = u[AvEithers]
    lazy val tuples = u[AvTuples]
    lazy val twoTuples = u[AvTwoTuples]
    lazy val bytes = u[AvBytes]
    lazy val money = u[AvMoney]
    lazy val times = u[AvTimes]
    lazy val legacyTimes = u[AvLegacyTimes]
    lazy val offset = u[AvOffset]
    lazy val uuid = u[AvUuid]
    lazy val defaults = u[AvDefaults]
    lazy val generics = u[AvGenerics]
    lazy val erased = u[AvUsesErased]
    lazy val annotated = u[AvAnnotated]
    lazy val fixed = u[AvFixed]
    lazy val noDefault = u[AvNoDefault]
    lazy val priority = u[AvPriority]
    lazy val direction = u[AvDirection]
    lazy val widenedLong = u[AvWidenedLong]
    lazy val widenedDouble = u[AvWidenedDouble]
    lazy val mixed = u[AvUsesMixed]
    lazy val order = u[AvUsesOrder]
    lazy val optionalSums = u[AvOptionalSums]
    lazy val fieldNs = u[AvFieldNs]
    lazy val evolV1 = u[AvEvolV1]
    lazy val evolV2 = u[AvEvolV2]
  }

  private object UDecimal {
    implicit val scalePrecision: ScalePrecision = ScalePrecision(scale = 4, precision = 10)
    lazy val money = u[AvMoney]
  }

  group("avro4s oracle: records and primitives") {
    test("simple record")(compare(K.person, U.person, AvPerson("Ann", 30))())
    test("primitives")(compare(K.primitives, U.primitives, AvPrimitives(true, 1, 2, 3, 4L, 5.5f, 6.5, "s"))())
    test("options")(
      compare(
        K.options,
        U.options,
        AvOptions(Some("a"), Some(1), Some(AvPerson("p", 2))),
        AvOptions(None, None, None)
      )()
    )
    test("collections")(
      compare(
        K.collections,
        U.collections,
        AvCollections(List(1, 2), Vector("a"), Set(3L), Seq(1.5), List(List(1), Nil))
      )()
    )
    test("maps")(
      compare(K.maps, U.maps, AvMaps(Map("a" -> 1), Map("x" -> List("y")), Map("p" -> AvPerson("q", 3))))()
    )
    test("nested records")(
      compare(K.nested, U.nested, AvNested(AvPerson("a", 1), List(AvPerson("b", 2)), Some(AvPerson("c", 3))))()
    )
    test("snake_case field names (avro4s 4's FieldMapper)") {
      // avro4s 5's FieldMapper renames the schema's fields, but its record encoders still look fields up by their
      // Scala names, so it cannot encode with it: compared on Scala 2.13 only.
      AvroUpstream.camel.foreach(upstream => compare(KSnake.camel, upstream, AvCamel("a", 1))())
    }
  }

  group("avro4s oracle: sum types") {
    test("sealed trait of case classes (UNION)")(
      compare(K.drawing, U.drawing, AvDrawing(AvCircle(1.0), List(AvRect(1, 2), AvCircle(2)), Some(AvRect(3, 4))))()
    )
    test("sealed trait of case objects (ENUM)")(
      compare(K.palette, U.palette, AvPalette(AvRed, List(AvGreen, AvBlue)))()
    )
    test("sealed trait mixing case objects and case classes")(
      compare(K.mixed, U.mixed, AvUsesMixed(AvNone), AvUsesMixed(AvSome(1)))()
    )
    test("subtype order: UNION by full name, ENUM by declaration (avro4s 5) / by name (avro4s 4)")(
      compare(K.order, U.order, AvUsesOrder(AvAnt(6), AvZ), AvUsesOrder(AvZebra(1), AvA))()
    )
    test("@avroSortPriority")(compare(K.priority, U.priority, AvLow, AvMid, AvHigh)())
    test("@avroEnumDefault") {
      if (isAvro4s5)
        // avro4s 5 never implemented @AvroEnumDefault (left as a TODO in its SealedTraits schema), so its ENUM has no
        // default; Kindlings keeps avro4s 4's behaviour on both Scala versions (feature-parity.md, avro annotations).
        compare(K.direction, U.direction, AvNorth, AvSouth)("schema $[default]: only in kindlings: AvSouth")
      else compare(K.direction, U.direction, AvNorth, AvSouth)()
    }
    test("Either")(
      compare(
        K.eithers,
        U.eithers,
        AvEithers(Left("l"), Right(AvRect(1, 2))),
        AvEithers(Right(3), Left(AvPerson("a", 1)))
      )()
    )
    test("optional sum types")(
      compare(
        K.optionalSums,
        U.optionalSums,
        AvOptionalSums(Some(AvCircle(1)), Some(Left("a")), Some(AvRed)),
        AvOptionalSums(None, Some(Right(1)), None),
        AvOptionalSums(Some(AvRect(1, 2)), None, Some(AvBlue))
      )()
    )
    test("scala.Enumeration") {
      // avro4s 5 has no scala.Enumeration support: compared on Scala 2.13 only.
      AvroUpstream.weekday.foreach(upstream =>
        compare(K.weekday, upstream, AvUsesWeekday(AvWeekday.Tue, List(AvWeekday.Mon, AvWeekday.Wed)))()
      )
    }
  }

  group("avro4s oracle: other types") {
    test("tuples") {
      // Documented difference (feature-parity.md, "Generic schema names"): avro4s names every tuple record
      // `scala.TupleN`, so two different tuple types cannot coexist in one schema (see the next test); Kindlings
      // encodes the type arguments as for any generic type. The binary encoding is the same.
      compare(K.tuples, U.tuples, AvTuples(("a", 1), (2, "b", true)))(
        "schema $[fields][0][type][name]: kindlings=Tuple2__String_Int upstream=Tuple2",
        "schema $[fields][1][type][name]: kindlings=Tuple3__Int_String_Boolean upstream=Tuple3"
      )
    }
    test("tuples: two tuple types in one schema survive a JSON round trip only with Kindlings' names") {
      def reparsed(schema: Schema): Schema = new Schema.Parser().parse(schema.toString)
      assertEquals(reparsed(K.twoTuples.schema), K.twoTuples.schema)
      // avro4s: the second `scala.Tuple2` is written as a reference to the first one, i.e. becomes (String, Int)
      assertNotEquals(reparsed(U.twoTuples.schema), U.twoTuples.schema)
    }
    test("recursive types") {
      // avro4s 5 cannot derive recursive types (its auto-derivation does not recurse into itself): Scala 2.13 only.
      AvroUpstream.tree.foreach(upstream => compare(K.tree, upstream, AvTree(1, List(AvTree(2, Nil))))())
    }
    test("value classes") {
      // avro4s 5 cannot derive value classes (Scala 3 has no Mirror for them): Scala 2.13 only.
      AvroUpstream.usesId.foreach(upstream =>
        compare(K.usesId, upstream, AvUsesId(AvId("a"), List(AvId("b")), Some(AvId("c"))))()
      )
    }
    test("bytes")(
      compareWith(
        K.bytes,
        U.bytes,
        List(AvBytes(Array[Byte](1, 2), Vector[Byte](3))),
        (a: AvBytes, b: AvBytes) => a.arr.toList == b.arr.toList && a.vec == b.vec
      )()
    )
    test("UUID")(compare(K.uuid, U.uuid, AvUuid(java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")))())
    test("generic schema names")(
      compare(K.generics, U.generics, AvGenerics(AvPair("a", 1), AvBox(AvPair("b", 2)), AvBox(3)))()
    )
    test("@avroErasedName")(compare(K.erased, U.erased, AvUsesErased(AvErased(1)))())
  }

  group("avro4s oracle: BigDecimal") {
    test("default: STRING vs avro4s' decimal(8, 2)") {
      // Documented difference (feature-parity.md, BigDecimal row): Kindlings encodes BigDecimal as a STRING unless a
      // DecimalConfig is given; avro4s defaults to ScalePrecision(scale = 2, precision = 8).
      compareSchemas(K.money, U.money)(
        "schema $[fields][0][type]: kindlings=string upstream=ListMap(type -> bytes, logicalType -> decimal, precision -> 8, scale -> 2)"
      )
    }
    test("AvroConfig.avro4sDefaults")(compare(KAvro4s.money, U.money, AvMoney(BigDecimal("12.34")))())
    test("withDecimalConfig vs an implicit ScalePrecision")(
      compare(KDecimal.money, UDecimal.money, AvMoney(BigDecimal("12.3456")))()
    )
  }

  group("avro4s oracle: temporal types") {
    test("java.time")(
      compare(
        K.times,
        U.times,
        AvTimes(
          java.time.Instant.ofEpochMilli(1700000000123L),
          java.time.LocalDate.of(2024, 1, 15),
          java.time.LocalTime.of(10, 30, 15, 123456000),
          java.time.LocalDateTime.of(2024, 1, 15, 10, 30, 15, 123456789)
        )
      )()
    )
    test("java.sql.Date and java.sql.Timestamp")(
      compare(
        K.legacyTimes,
        U.legacyTimes,
        AvLegacyTimes(
          java.sql.Date.valueOf("2024-01-15"),
          java.sql.Timestamp.from(java.time.Instant.ofEpochMilli(1700000000123L))
        )
      )()
    )
    test("timestamp-micros (withInstantPrecision vs a timestamp-micros SchemaFor)")(
      compare(
        KMicros.instants,
        AvroUpstream.instantsMicros,
        AvInstants(
          java.time.Instant.ofEpochSecond(1700000000L, 123456000L),
          java.sql.Timestamp.from(java.time.Instant.ofEpochSecond(1700000000L, 123456000L))
        )
      )()
    )
    test("OffsetDateTime: default timestamp-millis vs avro4s' ISO-8601 string") {
      // Documented difference (feature-parity.md, OffsetDateTime row): Kindlings encodes OffsetDateTime as a UTC
      // timestamp-millis long by default (withOffsetDateTimeAsIsoString / avro4sDefaults switch to avro4s' encoding).
      compareSchemas(K.offset, U.offset)(
        "schema $[fields][0][type][type]: kindlings=long upstream=string",
        "schema $[fields][0][type][logicalType]: kindlings=timestamp-millis upstream=datetime-with-offset"
      )
    }
    test("OffsetDateTime: AvroConfig.avro4sDefaults")(
      compare(KAvro4s.offset, U.offset, AvOffset(java.time.OffsetDateTime.parse("2024-01-15T10:30:15.123+02:00")))()
    )
  }

  group("avro4s oracle: annotations") {
    test("record and field annotations")(compare(K.annotated, U.annotated, AvAnnotated("n", 1, "t", "c"))())
    test("@avroFixed")(
      compareWith(
        K.fixed,
        U.fixed,
        List(AvFixed(Array[Byte](1, 2, 3, 4))),
        (a: AvFixed, b: AvFixed) => a.token.toList == b.token.toList
      )()
    )
    test("field @avroNamespace")(compare(K.fieldNs, U.fieldNs, AvFieldNs(AvPerson("a", 1)))())
  }

  group("avro4s oracle: default values") {
    test("Scala defaults are schema defaults (the default's union branch first)")(
      compare(K.defaults, U.defaults, AvDefaults(), AvDefaults("y", 1, Some("o"), None, Nil))()
    )
    test("@avroNoDefault")(compare(K.noDefault, U.noDefault, AvNoDefault(1, "b"))())
  }

  /** Decodes the same Avro value with both decoders and compares the outcomes (the value, or a failure). */
  private def assertDecodes[A](k: AvroSide[A], u: AvroSide[A], input: Any)(expectedDifferences: String*): Unit = {
    val kd = Try(k.decode(input))
    val ud = Try(u.decode(input))
    val differences = (kd, ud) match {
      case (Success(a), Success(b)) if a == b => Nil
      case (Failure(_), Failure(_))           => Nil
      case _                                  => List(s"decoded: kindlings=$kd upstream=$ud")
    }
    assertEquals(differences, expectedDifferences.toList, differences.mkString("\n"))
  }

  private def record(schemaJson: String)(fields: (String, Any)*): GenericRecord = {
    val r = new GenericData.Record(new Schema.Parser().parse(schemaJson))
    fields.foreach { case (n, v) => r.put(n, v) }
    r
  }

  private def recordSchema(name: String, fields: String*): String =
    s"""{"type":"record","name":"$name","namespace":"hearth.kindlings.oracle.avro","fields":[${fields.mkString(
        ","
      )}]}"""

  group("avro4s oracle: decoding") {
    test("integral widening: an int decodes as Long") {
      val input = record(recordSchema("AvWidenedLong", """{"name":"i","type":"int"}"""))("i" -> Integer.valueOf(1))
      assertDecodes(K.widenedLong, U.widenedLong, input)()
    }
    test("no floating-point widening: a float does not decode as Double") {
      val input =
        record(recordSchema("AvWidenedDouble", """{"name":"d","type":"float"}"""))("d" -> java.lang.Float.valueOf(1.5f))
      assertDecodes(K.widenedDouble, U.widenedDouble, input)()
    }
    test("a record missing fields that have Scala defaults") {
      val input = record(recordSchema("AvEvolV2", """{"name":"name","type":"string"}"""))("name" -> "a")
      assertDecodes(K.evolV2, U.evolV2, input)()
    }
    test("a field of the wrong type that has a Scala default") {
      val input = record(
        recordSchema(
          "AvEvolV2",
          """{"name":"name","type":"string"}""",
          """{"name":"nickname","type":["null","string"]}""",
          """{"name":"score","type":"string"}"""
        )
      )("name" -> "a", "nickname" -> null, "score" -> "high")
      assertDecodes(K.evolV2, U.evolV2, input)()
    }
    test("a record missing a field without a default is rejected") {
      val input = record(recordSchema("AvPerson", """{"name":"name","type":"string"}"""))("name" -> "a")
      assertDecodes(K.person, U.person, input)()
    }
    test("a field of the wrong type without a default is rejected") {
      val input =
        record(recordSchema("AvPerson", """{"name":"name","type":"string"}""", """{"name":"age","type":"string"}"""))(
          "name" -> "a",
          "age" -> "old"
        )
      assertDecodes(K.person, U.person, input)()
    }
    test("not a record is rejected") {
      assertDecodes(K.person, U.person, "not a record")()
      assertDecodes(K.person, U.person, null)()
    }
    test("an unknown enum symbol is rejected") {
      val schema = U.palette.schema
      val input = new GenericData.Record(schema)
      input.put("main", new GenericData.EnumSymbol(schema.getField("main").schema(), "AvPurple"))
      input.put("others", new java.util.ArrayList[Any]())
      assertDecodes(K.palette, U.palette, input)()
    }
    test("schema evolution: old data read with a reader schema that adds fields with Scala defaults") {
      def evolve(v1: AvroSide[AvEvolV1], v2: AvroSide[AvEvolV2]): Try[AvEvolV2] =
        AvroOracle.decodeBinary(v2, v1.schema, AvroOracle.encodeBinary(v1, AvEvolV1("a")).get)
      val upstream = evolve(U.evolV1, U.evolV2)
      assertEquals(upstream, Success(AvEvolV2("a")))
      assertEquals(evolve(K.evolV1, K.evolV2), upstream)
      // The opt-out (withoutScalaDefaultsInSchema): without schema defaults Avro's schema resolution rejects the data,
      // since the added fields are required in the reader schema.
      assert(evolve(KNoSchemaDefaults.evolV1, KNoSchemaDefaults.evolV2).isFailure)
    }
  }
}
