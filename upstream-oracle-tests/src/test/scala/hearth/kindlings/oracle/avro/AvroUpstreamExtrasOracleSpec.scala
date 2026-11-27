package hearth.kindlings.oracle.avro

import com.sksamuel.avro4s.{Decoder as UDecoder, Encoder as UEncoder, SchemaFor as USchemaFor}
import hearth.kindlings.avroderivation.{AvroConfig, AvroDecoder, AvroEncoder, AvroIO}

import scala.util.{Success, Try}

/** More of avro4s' own tests replayed against Kindlings (see [[AvroUpstreamTestsOracleSpec]]): the cases only one
  * avro4s line can derive (value classes, `scala.Enumeration`, Java enums and field mappers with avro4s 4), deep field
  * namespaces, and avro4s' streams against `AvroIO`.
  */
final class AvroUpstreamExtrasOracleSpec extends AvroOracleSuite {

  private def k[A](implicit e: AvroEncoder[A], d: AvroDecoder[A]): AvroSide[A] = AvroSide.kindlings[A]

  private def cmp[A](name: String, k: => AvroSide[A], u: => AvroSide[A], values: A*)(expected: String*): Unit =
    test(name)(compareWith(k, u, values.toList, (a: A, b: A) => a == b)(expected*))

  /** A case only avro4s 4 can derive: compared on Scala 2.13, Kindlings round-trips the values on Scala 3. */
  private def cmp4[A](name: String, k: => AvroSide[A], u: => Option[AvroSide[A]], values: A*)(
      eq: (A, A) => Boolean = (a: A, b: A) => a == b
  )(expected: String*): Unit =
    test(name) {
      u match {
        case Some(upstream) => compareWith(k, upstream, values.toList, eq)(expected*)
        case None           =>
          values.foreach { v =>
            val decoded = AvroOracle.decodeBinary(k, k.schema, AvroOracle.encodeBinary(k, v).get)
            assert(decoded.toOption.exists(eq(_, v)), s"Kindlings round trip of $v: $decoded")
          }
      }
    }

  private object K {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults
    lazy val transient = k[AuTransient]
    lazy val nsDeep = k[AuNsDeep]
    lazy val basket = k[AuBasket]
    lazy val javaEnums = k[AuJavaEnums]
    lazy val topJavaEnum = k[AuWine]
    lazy val annotatedJavaEnum = k[AuWineWithAnnotations]
    lazy val docValueType = k[AuDocUsesValueType]
    lazy val fixedMessage = k[AuFixedMessage]
    lazy val optionOfFixed = k[AuOptionOfFixed]
    lazy val genOuter = k[AuGenOuter[String, Int]]
    lazy val scalaEnums = k[AuScalaEnums]
    lazy val topScalaEnum = k[AuColours.Value]
    lazy val propValues = k[AuPropValues]
    lazy val decimal = k[AuDecimal]
    lazy val composer = k[AuComposer]
    implicit lazy val composerEncoder: AvroEncoder[AuComposer] = AvroEncoder.derived[AuComposer]
    implicit lazy val composerDecoder: AvroDecoder[AuComposer] = AvroDecoder.derived[AuComposer]
  }
  private object KHalfUp {
    implicit val config: AvroConfig =
      AvroConfig.avro4sDefaults.withDecimalConfig(precision = 8, scale = 2, java.math.RoundingMode.HALF_UP)
    lazy val decimal = k[AuDecimal]
  }
  private object KPascal {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults.withPascalCaseFieldNames
    lazy val naming = k[AuNamingStrategy]
  }
  private object KSnake {
    implicit val config: AvroConfig = AvroConfig.avro4sDefaults.withSnakeCaseFieldNames
    lazy val naming = k[AuNamingStrategy]
    lazy val mapperWithName = k[AuMapperWithName]
  }

  private object U {
    lazy val transient = u[AuTransient]
    lazy val nsDeep = u[AuNsDeep]
    lazy val basket = u[AuBasket]
    lazy val propValues = u[AuPropValues]
    lazy val decimal = u[AuDecimal]
  }

  private val uuid = java.util.UUID.fromString("86da265c-95bd-443c-8860-9381efca059d")
  private val naming = AuNamingStrategy("a", "b", "c", "d", AuNamingStrategy2("e"))

  group("avro4s tests: annotations (2)") {
    cmp(
      "TransientDecoderTest: a transient Option without a default is None",
      K.transient,
      U.transient,
      AuTransient("a", None, "c")
    )()
    test("field @AvroNamespace: every named type below the field (SchemaHelper.overrideNamespace)") {
      val value = AuNsDeep(
        AuNsDeepMid(
          AuNsDeepLeaf(1),
          List(AuNsDeepLeaf(2)),
          AuCuppersOptionEnum,
          Map("a" -> AuNsDeepLeaf(3)),
          Some(AuNsDeepLeaf(4))
        ),
        AuNsDeepLeaf(5)
      )
      compareSchemas(K.nsDeep, U.nsDeep)()
      // avro4s 4 cannot encode it (the record it builds for the Option is not a member of the namespaced union)
      if (isAvro4s5) compare(K.nsDeep, U.nsDeep, value)()
      else {
        assert(AvroOracle.encodeBinary(U.nsDeep, value).isFailure, "avro4s 4 encodes this case now: compare it")
        assertEquals(
          AvroOracle.decodeBinary(K.nsDeep, K.nsDeep.schema, AvroOracle.encodeBinary(K.nsDeep, value).get),
          Success(value)
        )
      }
    }
    cmp(
      "SealedTraitDecoderTest: @AvroNamespace and @AvroName on a sealed trait of case objects",
      K.basket,
      U.basket,
      AuBasket(AuApple, List(AuOrange, AuApple))
    )()
    test("@AvroProp values that look like JSON stay Strings") {
      def props(side: AvroSide[AuPropValues]): List[AnyRef] =
        List("a" -> "text", "b" -> "number", "c" -> "flag").map { case (field, key) =>
          side.schema.getField(field).getObjectProp(key)
        }
      assertEquals(props(U.propValues), List[AnyRef]("plain", "1", "true"))
      assertEquals(props(K.propValues), props(U.propValues))
      compare(K.propValues, U.propValues, AuPropValues("a", "b", "c"))()
    }
  }

  group("avro4s tests: BigDecimal (BigDecimalEncoderTest, BigDecimalRoundTrip)") {
    cmp(
      "a smaller scale is widened to the schema's",
      K.decimal,
      U.decimal,
      AuDecimal(BigDecimal("123.4")),
      AuDecimal(BigDecimal(12))
    )()
    test("a larger scale is rejected by default (RoundingMode.UNNECESSARY, as avro4s 4)") {
      val value = AuDecimal(BigDecimal("12.345"))
      val kindlings = AvroOracle.encodeBinary(K.decimal, value).failed.toOption.map(_.getClass.getName)
      val upstream = AvroOracle.encodeBinary(U.decimal, value).failed.toOption.map(_.getClass.getName)
      assertEquals(kindlings, Some("java.lang.ArithmeticException"))
      // avro4s 5 differs from avro4s 4: it always rounds HALF_UP (feature-parity.md, "Decimal rounding")
      assertEquals(upstream, if (isAvro4s5) None else kindlings)
    }
    test("a larger scale is rounded with the configured rounding mode (avro4s 4's implicit RoundingMode)") {
      val rounded =
        (a: AuDecimal, b: AuDecimal) => a.decimal == b.decimal.setScale(2, scala.math.BigDecimal.RoundingMode.HALF_UP)
      compareWith(
        KHalfUp.decimal,
        AvroUpstream.decimalHalfUp,
        List(AuDecimal(BigDecimal("12.345")), AuDecimal(BigDecimal("-0.005")), AuDecimal(BigDecimal("1.5"))),
        rounded
      )()
    }
    test("a value with more digits than the precision is rejected") {
      val value = AuDecimal(BigDecimal("12345678.90"))
      val kindlings = AvroOracle.encodeBinary(K.decimal, value).failed.toOption.map(_.getClass.getName)
      assertEquals(kindlings, Some("org.apache.avro.AvroTypeException"))
      assertEquals(kindlings, AvroOracle.encodeBinary(U.decimal, value).failed.toOption.map(_.getClass.getName))
    }
  }

  group("avro4s tests: enums of other kinds (EnumSchemaTest)") {
    cmp4(
      "java enums",
      K.javaEnums,
      AvroUpstream.javaEnums,
      AuJavaEnums(AuWine.Malbec, AuWine.Merlot, Some(AuWine.Shiraz), None, None),
      AuJavaEnums(wine = AuWine.Malbec, optional = None)
    )()()
    cmp4[AuWine]("top level java enum", K.topJavaEnum, AvroUpstream.topJavaEnum, AuWine.CabSav)()()
    cmp4[AuWineWithAnnotations](
      "java enum with @AvroJavaName, @AvroJavaNamespace, @AvroJavaProp, @AvroJavaEnumDefault",
      K.annotatedJavaEnum,
      AvroUpstream.annotatedJavaEnum,
      AuWineWithAnnotations.CabSav
    )()()
    test("java enum annotations (both Scala versions)") {
      assertEquals(
        K.annotatedJavaEnum.schema.toString,
        """{"type":"enum","name":"Wine","namespace":"test","symbols":["Malbec","Shiraz","CabSav","Merlot"],"default":"CabSav","hello":"world"}"""
      )
    }
    cmp4(
      "scala enums: symbols in the order of their ids",
      K.scalaEnums,
      AvroUpstream.scalaEnums,
      AuScalaEnums(AuColours.Amber, AuColours.Green, Some(AuColours.Red), None, None),
      AuScalaEnums(colours = AuColours.Red, optional = None)
    )()()
    cmp4[AuColours.Value]("top level scala enum", K.topScalaEnum, AvroUpstream.topScalaEnum, AuColours.Amber)()()
    test("scala enum symbols (both Scala versions)") {
      assertEquals(K.topScalaEnum.schema.getEnumSymbols.toString, "[Red, Amber, Green]")
    }
  }

  group("avro4s tests: value classes and field mappers (avro4s 4)") {
    cmp4(
      "AvroDocSchemaTest: the doc of a value type documents the field",
      K.docValueType,
      AvroUpstream.docValueType,
      AuDocUsesValueType(AuDocValueType("a"))
    )()()
    cmp4[AuFixedMessage](
      "FixedEncoderTest: @AvroFixed value class of bytes",
      K.fixedMessage,
      AvroUpstream.fixedMessage,
      AuFixedMessage(AuQuarterSha(Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)), Array[Byte](0, 1))
    )((a, b) => a.q.bytes.toList == b.q.bytes.toList && a.payload.toList == b.payload.toList)()
    cmp4(
      "FixedEncoderTest: Option of a @AvroFixed value class of a String",
      K.optionOfFixed,
      AvroUpstream.optionOfFixed,
      AuOptionOfFixed(Some(AuFixedValueType("12345678"))),
      AuOptionOfFixed(None)
    )()()
    cmp4(
      "GenericSchemaTest: value types in generics",
      K.genOuter,
      AvroUpstream.genOuter,
      AuGenOuter[String, Int](AuThing2Id(uuid), AuGenInner(AuInnerId("i"), "l", Some(1)))
    )()()
    cmp4("FieldMapperFieldTest: pascal case", KPascal.naming, AvroUpstream.pascalCase, naming)()()
    cmp4("FieldMapperFieldTest: snake case", KSnake.naming, AvroUpstream.snakeCase, naming)()()
    cmp4(
      "Github396: @AvroName overrides the field mapper",
      KSnake.mapperWithName,
      AvroUpstream.mapperWithName,
      AuMapperWithName("a", 1)
    )()()
  }

  group("avro4s streams vs AvroIO (streams/*)") {
    import K.{composerDecoder, composerEncoder}
    val ennio = AuComposer("ennio morricone", "rome", Seq("legend of 1900", "ecstasy of gold"))
    val hans = AuComposer("hans zimmer", "frankfurt", Seq("batman begins"))
    def upstream[R](f: USchemaFor[AuComposer] => UEncoder[AuComposer] => UDecoder[AuComposer] => R): R =
      f(implicitly[USchemaFor[AuComposer]])(implicitly[UEncoder[AuComposer]])(implicitly[UDecoder[AuComposer]])

    test("data files: each side reads the other's") {
      upstream { implicit s => implicit e => implicit d =>
        assertEquals(AvroIO.fromDataFile[AuComposer](AvroUpstream.writeData(Seq(ennio, hans))), List(ennio, hans))
        assertEquals(AvroUpstream.readData[AuComposer](AvroIO.toDataFile(List(ennio, hans))), List(ennio, hans))
      }
    }
    test("data files: the schema is in the header, with the codec (AvroDataOutputStreamCodecTest)") {
      val plain = new String(AvroIO.toDataFile(List(ennio)), "UTF-8")
      assert(plain.contains("birthplace") && plain.contains("compositions"))
      val codec =
        org.apache.avro.file.CodecFactory.deflateCodec(org.apache.avro.file.CodecFactory.DEFAULT_DEFLATE_LEVEL)
      val deflated = AvroIO.toDataFile(List(ennio, hans), codec)
      assert(new String(deflated, "UTF-8").contains("deflate"))
      assertEquals(AvroIO.fromDataFile[AuComposer](deflated), List(ennio, hans))
      upstream { implicit s => _ => implicit d =>
        assertEquals(AvroUpstream.readData[AuComposer](deflated), List(ennio, hans))
      }
    }
    test("binary: the same bytes, no schema (BinaryOutputStreamTest)") {
      upstream { implicit s => implicit e => implicit d =>
        val bytes = AvroIO.toBinary(ennio)
        assertEquals(AvroOracle.hex(bytes), AvroOracle.hex(AvroUpstream.writeBinary(Seq(ennio))))
        assert(!new String(bytes, "UTF-8").contains("birthplace"))
        assertEquals(AvroUpstream.readBinary[AuComposer](bytes), List(ennio))
        assertEquals(AvroIO.fromBinary[AuComposer](AvroUpstream.writeBinary(Seq(ennio))), ennio)
      }
    }
    test("json: the same document") {
      upstream { implicit s => implicit e => implicit d =>
        val json = AvroIO.toJson(ennio)
        assertEquals(json, new String(AvroUpstream.writeJson(Seq(ennio)), "UTF-8").trim)
        assertEquals(AvroUpstream.readJson[AuComposer](json.getBytes("UTF-8")), List(ennio))
        assertEquals(Try(AvroIO.fromJson[AuComposer](json)), Success(ennio))
      }
    }
  }
}
