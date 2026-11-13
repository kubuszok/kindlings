package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** Plain (non-case) classes and multiple parameter lists, aligned with jsoniter-scala's `JsonCodecMaker.make`
  * (v2.42.3). The `Spec ~N` references point at upstream's `JsonCodecMakerSpec.scala`.
  *
  * Config-dependent scenarios run on the optimized codegen (`JsoniterConfig.default` or `OptimizedCodecs`) and on the
  * generic one (configs from `ClassesRuntimeConfigs`, unknown at compile time).
  */
final class JsoniterScalaClassesParitySpec extends MacroSuite {

  private def decodeError(fragment: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(error.getMessage.contains(fragment), s"expected an error containing '$fragment', got: ${error.getMessage}")
  }

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => A): A = derive(config)

  group("jsoniter-scala parity: plain classes and parameter lists") {

    group("plain classes with val/var constructor parameters (Spec ~612)") {

      test("are encoded and decoded through their accessors") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityNonCaseClass] }
        List[JsonValueCodec[ParityNonCaseClass]](KindlingsJsonValueCodec.derived[ParityNonCaseClass], cRuntime)
          .foreach { c =>
            writeToString(new ParityNonCaseClass(1, "VVV"))(c) ==> """{"id":1,"name":"VVV"}"""
            readFromString[ParityNonCaseClass]("""{"id":1,"name":"VVV"}""")(c) ==> new ParityNonCaseClass(1, "VVV")
            readFromString[ParityNonCaseClass]("""{"name":"VVV","id":1}""")(c) ==> new ParityNonCaseClass(1, "VVV")
            decodeError("""missing required field "name"""")(readFromString[ParityNonCaseClass]("""{"id":1}""")(c))
          }
      }

      test("use defaults and absent-field rules like case classes") {
        val cRuntime = runtime(ClassesRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityNonCaseWithDefaults]
        }
        List[JsonValueCodec[ParityNonCaseWithDefaults]](OptimizedCodecs.nonCaseDefaults, cRuntime).foreach { c =>
          readFromString[ParityNonCaseWithDefaults]("""{"id":1}""")(c) ==> new ParityNonCaseWithDefaults(1)
          writeToString(new ParityNonCaseWithDefaults(1))(c) ==> """{"id":1}"""
          writeToString(new ParityNonCaseWithDefaults(1, List("b"), Some("n")))(c) ==>
            """{"id":1,"tags":["b"],"note":"n"}"""
        }
      }

      test("are supported as nested fields and collection elements") {
        val value = ParityHasNonCaseClass(new ParityNonCaseClass(1, "a"), List(new ParityNonCaseClass(2, "b")))
        val json = """{"inner":{"id":1,"name":"a"},"list":[{"id":2,"name":"b"}]}"""
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityHasNonCaseClass] }
        List[JsonValueCodec[ParityHasNonCaseClass]](KindlingsJsonValueCodec.derived[ParityHasNonCaseClass], cRuntime)
          .foreach { c =>
            writeToString(value)(c) ==> json
            readFromString[ParityHasNonCaseClass](json)(c) ==> value
          }
      }

      test("parameters without a public accessor are a compile error (Spec ~3597)") {
        compileErrors(
          """
          class ParamHasNoAccessor(val i: Int, a: String)
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[ParamHasNoAccessor]
          """
        ).check("'a' parameter of")
        compileErrors(
          """
          case class MultiListOfArgsWithNonPublicParam(i: Int)(l: Long)
          hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[MultiListOfArgsWithNonPublicParam]
          """
        ).check("should be defined as 'val' or 'var' in the primary constructor")
      }
    }

    group("ADT leaves which are not case classes or case objects (Spec ~2839)") {
      val all = List[ParityPlainAdt](new ParityPlainLeaf(), new ParityPlainLeafWithFields(1, "a"), ParityPlainObject)

      test("with a discriminator") {
        val cRuntime = runtime(ClassesRuntimeConfigs.discriminator) { implicit c =>
          KindlingsJsonValueCodec.derived[List[ParityPlainAdt]]
        }
        val json = """[{"type":"ParityPlainLeaf"},{"type":"ParityPlainLeafWithFields","x":1,"y":"a"},{"type":"ParityPlainObject"}]"""
        List[JsonValueCodec[List[ParityPlainAdt]]](OptimizedCodecs.plainAdtDiscriminator, cRuntime).foreach { c =>
          writeToString(all)(c) ==> json
          readFromString[List[ParityPlainAdt]](json)(c) ==> all
        }
      }

      test("wrapper-style") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[List[ParityPlainAdt]] }
        val json = """[{"ParityPlainLeaf":{}},{"ParityPlainLeafWithFields":{"x":1,"y":"a"}},"ParityPlainObject"]"""
        List[JsonValueCodec[List[ParityPlainAdt]]](KindlingsJsonValueCodec.derived[List[ParityPlainAdt]], cRuntime)
          .foreach { c =>
            writeToString(all)(c) ==> json
            readFromString[List[ParityPlainAdt]](json)(c) ==> all
          }
      }
    }

    group("ADTs with a non-abstract sealed base (Spec ~2856)") {
      val all = List[ParitySealedBase](new ParitySealedBase(), ParitySealedB(1), ParitySealedC("VVV"))
      def describe(values: List[ParitySealedBase]): List[String] = values.map {
        case ParitySealedB(n) => s"B($n)"
        case ParitySealedC(s) => s"C($s)"
        case b                => b.getClass.getSimpleName
      }

      test("with a discriminator") {
        val cRuntime = runtime(ClassesRuntimeConfigs.discriminator) { implicit c =>
          KindlingsJsonValueCodec.derived[List[ParitySealedBase]]
        }
        val json = """[{"type":"ParitySealedBase"},{"type":"ParitySealedB","n":1},{"type":"ParitySealedC","s":"VVV"}]"""
        List[JsonValueCodec[List[ParitySealedBase]]](OptimizedCodecs.sealedBaseDiscriminator, cRuntime).foreach { c =>
          writeToString(all)(c) ==> json
          describe(readFromString[List[ParitySealedBase]](json)(c)) ==> describe(all)
        }
      }

      test("wrapper-style") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[List[ParitySealedBase]] }
        val json = """[{"ParitySealedBase":{}},{"ParitySealedB":{"n":1}},{"ParitySealedC":{"s":"VVV"}}]"""
        List[JsonValueCodec[List[ParitySealedBase]]](KindlingsJsonValueCodec.derived[List[ParitySealedBase]], cRuntime)
          .foreach { c =>
            writeToString(all)(c) ==> json
            describe(readFromString[List[ParitySealedBase]](json)(c)) ==> describe(all)
          }
      }
    }

    group("multiple parameter lists (Spec ~3518)") {

      test("all parameter lists are encoded and decoded") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityMultiList] }
        List[JsonValueCodec[ParityMultiList]](KindlingsJsonValueCodec.derived[ParityMultiList], cRuntime).foreach { c =>
          writeToString(new ParityMultiList(1)(2)("VVV"))(c) ==> """{"i":1,"l":2,"s":"VVV"}"""
          val decoded = readFromString[ParityMultiList]("""{"i":1,"l":2,"s":"VVV"}""")(c)
          (decoded.i, decoded.l, decoded.s) ==> ((1, 2L, "VVV"))
        }
      }

      test("defaults in the first parameter list") {
        val cRuntime = runtime(ClassesRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityMultiListDefault]
        }
        List[JsonValueCodec[ParityMultiListDefault]](OptimizedCodecs.multiListDefault, cRuntime).foreach { c =>
          writeToString(new ParityMultiListDefault()(2)("VVV"))(c) ==> """{"l":2,"s":"VVV"}"""
          val decoded = readFromString[ParityMultiListDefault]("""{"l":2,"s":"VVV"}""")(c)
          (decoded.i, decoded.l, decoded.s) ==> ((1, 2L, "VVV"))
        }
        val requireRuntime = runtime(ClassesRuntimeConfigs.requireDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityMultiListDefault]
        }
        List[JsonValueCodec[ParityMultiListDefault]](OptimizedCodecs.multiListDefaultRequired, requireRuntime).foreach {
          c =>
            writeToString(new ParityMultiListDefault()(2)("VVV"))(c) ==> """{"i":1,"l":2,"s":"VVV"}"""
            decodeError("""missing required field "i"""")(
              readFromString[ParityMultiListDefault]("""{"l":2,"s":"VVV"}""")(c)
            )
        }
      }

      test("defaults of later parameter lists may depend on preceding parameters") {
        def check(c: JsonValueCodec[ParityMultiListDependent], json: String, i: Int, l: Long, s: String): Unit = {
          val x = readFromString[ParityMultiListDependent](json)(c)
          (x.i, x.l, x.s) ==> ((i, l, s))
        }
        val cRuntime = runtime(ClassesRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityMultiListDependent]
        }
        List[JsonValueCodec[ParityMultiListDependent]](OptimizedCodecs.multiListDependent, cRuntime).foreach { c =>
          check(c, "{}", 1, 0L, "1-0")
          check(c, """{"i":3}""", 3, 2L, "3-2")
          check(c, """{"l":5}""", 1, 5L, "1-5")
          check(c, """{"s":"VVV","l":5,"i":3}""", 3, 5L, "VVV")
          decodeError("""duplicated field "i"""")(readFromString[ParityMultiListDependent]("""{"i":3,"i":3}""")(c))
          writeToString(new ParityMultiListDependent(3)(2L)("3-2"))(c) ==> """{"i":3}"""
          writeToString(new ParityMultiListDependent(3)(5L)("3-5"))(c) ==> """{"i":3,"l":5}"""
          writeToString(new ParityMultiListDependent(1)(0L)("VVV"))(c) ==> """{"s":"VVV"}"""
        }
        val defaultRuntime = runtime(ClassesRuntimeConfigs.default) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityMultiListDependent]
        }
        List[JsonValueCodec[ParityMultiListDependent]](
          KindlingsJsonValueCodec.derived[ParityMultiListDependent],
          defaultRuntime
        ).foreach { c =>
          check(c, """{"i":3}""", 3, 2L, "3-2")
          check(c, """{"l":5}""", 1, 5L, "1-5")
          writeToString(new ParityMultiListDependent(3)(2L)("3-2"))(c) ==> """{"i":3,"l":2,"s":"3-2"}"""
        }
      }

      test("generic class with dependent defaults and a transient parameter") {
        val cRuntime = runtime(ClassesRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityGenericMultiList[String]]
        }
        List[JsonValueCodec[ParityGenericMultiList[String]]](OptimizedCodecs.genericMultiList, cRuntime).foreach { c =>
          val x = readFromString[ParityGenericMultiList[String]]("""{"a":"A"}""")(c)
          (x.n, x.as, x.m) ==> ((2, List("A", "A"), 4))
          val y = readFromString[ParityGenericMultiList[String]]("""{"a":"A","as":["B"]}""")(c)
          (y.as, y.m) ==> ((List("B"), 2))
          writeToString(new ParityGenericMultiList("A")()()())(c) ==> """{"a":"A"}"""
          writeToString(new ParityGenericMultiList("A")()(List("B"))())(c) ==> """{"a":"A","as":["B"]}"""
          writeToString(new ParityGenericMultiList("A")()(List("B"))(3))(c) ==> """{"a":"A","as":["B"],"m":3}"""
        }
      }

      test("ADT leaves with multiple parameter lists") {
        val cRuntime = runtime(ClassesRuntimeConfigs.discriminator) { implicit c =>
          KindlingsJsonValueCodec.derived[List[ParityMultiListAdt]]
        }
        val json = """[{"type":"ParityMultiData1","i":1,"pos":11},{"type":"ParityMultiData2","i":2,"pos":22}]"""
        List[JsonValueCodec[List[ParityMultiListAdt]]](OptimizedCodecs.multiListAdt, cRuntime).foreach { c =>
          writeToString(List[ParityMultiListAdt](ParityMultiData1(1)(11), ParityMultiData2(2)(22)))(c) ==> json
          readFromString[List[ParityMultiListAdt]](json)(c).map {
            case d: ParityMultiData1 => (d.i, d.pos)
            case d: ParityMultiData2 => (d.i, d.pos)
          } ==> List((1, 11), (2, 22))
        }
      }
    }

    group("nested options without skipNestedOptionValues (Spec ~1345)") {
      val none = ParityNestedOptions3(None, None)
      val someSomeSome = ParityNestedOptions3(None, Some(Some(Some("VVV"))))
      val someNone = ParityNestedOptions3(None, Some(None))
      val someSomeNone = ParityNestedOptions3(None, Some(Some(None)))

      test("inner options are encoded as the ADT Option, with a discriminator") {
        val cRuntime = runtime(NestedOptionsRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityNestedOptions3]
        }
        List[JsonValueCodec[ParityNestedOptions3]](OptimizedCodecs.nestedOptions3, cRuntime).foreach { c =>
          readFromString[ParityNestedOptions3]("""{"y":null,"x":null}""")(c) ==> none
          writeToString(ParityNestedOptions3(Some("VVV"), None))(c) ==> """{"y":"VVV"}"""
          writeToString(someSomeSome)(c) ==> """{"x":{"type":"Some","value":{"type":"Some","value":"VVV"}}}"""
          writeToString(someNone)(c) ==> """{"x":{"type":"None"}}"""
          writeToString(someSomeNone)(c) ==> """{"x":{"type":"Some","value":{"type":"None"}}}"""
          List(someSomeSome, someNone, someSomeNone).foreach { v =>
            readFromString[ParityNestedOptions3](writeToString(v)(c))(c) ==> v
          }
          decodeError("""missing required field "value"""")(
            readFromString[ParityNestedOptions3]("""{"x":{"type":"Some"}}""")(c)
          )
          decodeError("""illegal value of discriminator field "type"""")(
            readFromString[ParityNestedOptions3]("""{"x":{"type":"X"}}""")(c)
          )
        }
      }

      test("inner options are encoded as the ADT Option, wrapper-style") {
        val cRuntime =
          runtime(NestedOptionsRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityNestedOptions3] }
        List[JsonValueCodec[ParityNestedOptions3]](KindlingsJsonValueCodec.derived[ParityNestedOptions3], cRuntime)
          .foreach { c =>
            writeToString(someSomeSome)(c) ==> """{"y":null,"x":{"Some":{"value":{"Some":{"value":"VVV"}}}}}"""
            writeToString(someNone)(c) ==> """{"y":null,"x":"None"}"""
            writeToString(someSomeNone)(c) ==> """{"y":null,"x":{"Some":{"value":"None"}}}"""
            List(none, someSomeSome, someNone, someSomeNone).foreach { v =>
              readFromString[ParityNestedOptions3](writeToString(v)(c))(c) ==> v
            }
          }
        val circeRuntime = runtime(NestedOptionsRuntimeConfigs.circeLike) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityNestedOptions3]
        }
        List[JsonValueCodec[ParityNestedOptions3]](OptimizedCodecs.nestedOptions3CirceLike, circeRuntime).foreach { c =>
          writeToString(someNone)(c) ==> """{"y":null,"x":{"None":{}}}"""
          readFromString[ParityNestedOptions3](writeToString(someSomeNone)(c))(c) ==> someSomeNone
        }
      }
    }

    group("user-provided codecs of ADT leaves with a discriminator") {

      test("encode and decode the whole leaf object") {
        val values = List[ParityCustomAdt](ParityCustomLeaf(1), ParityRegularLeaf(2))
        val json = """[{"type":"ParityCustomLeaf","custom":1},{"type":"ParityRegularLeaf","a":2}]"""
        val cRuntime = runtime(ClassesRuntimeConfigs.discriminator) { implicit c =>
          KindlingsJsonValueCodec.derived[List[ParityCustomAdt]]
        }
        List[JsonValueCodec[List[ParityCustomAdt]]](OptimizedCodecs.customLeafDiscriminator, cRuntime).foreach { c =>
          writeToString(values)(c) ==> json
          readFromString[List[ParityCustomAdt]](json)(c) ==> values
        }
        readFromString[List[ParityCustomAdt]]("""[{"custom":3,"type":"ParityCustomLeaf"},{"a":2,"type":"ParityRegularLeaf"}]""")(
          OptimizedCodecs.customLeafDiscriminatorAnywhere
        ) ==> List(ParityCustomLeaf(3), ParityRegularLeaf(2))
      }
    }

    group("Iterator fields") {
      test("transientEmpty does not consume a non-empty iterator") {
        val cRuntime = runtime(ClassesRuntimeConfigs.jsoniterScalaDefaults) { implicit c =>
          KindlingsJsonValueCodec.derived[ParityIteratorField]
        }
        val cOptimized = OptimizedCodecs.iteratorField
        List[JsonValueCodec[ParityIteratorField]](cOptimized, cRuntime).foreach { c =>
          writeToString(ParityIteratorField(Iterator(1, 2), 3))(c) ==> """{"it":[1,2],"n":3}"""
          writeToString(ParityIteratorField(Iterator.empty, 3))(c) ==> """{"n":3}"""
        }
      }
    }

    group("user-provided key codecs") {
      test("take precedence over the built-in key types, also for String keys") {
        import ParityKeyCodecs.*
        val strings = KindlingsJsonValueCodec.derived[Map[String, Int]]
        writeToString(Map("ab" -> 1))(strings) ==> """{"AB":1}"""
        readFromString[Map[String, Int]]("""{"AB":1}""")(strings) ==> Map("ab" -> 1)
        val ints = KindlingsJsonValueCodec.derived[Map[Int, Int]]
        writeToString(Map(255 -> 1))(ints) ==> """{"ff":1}"""
        readFromString[Map[Int, Int]]("""{"ff":1}""")(ints) ==> Map(255 -> 1)
      }
    }

    group("inlineOneValueClasses") {
      test("applies to map keys") {
        val cRuntime =
          runtime(OneValueRuntimeConfigs.inline) { implicit c => KindlingsJsonValueCodec.derived[Map[ParityUserKey, Int]] }
        List[JsonValueCodec[Map[ParityUserKey, Int]]](OptimizedCodecs.inlineKeys, cRuntime).foreach { c =>
          writeToString(Map(ParityUserKey("abc") -> 1))(c) ==> """{"abc":1}"""
          readFromString[Map[ParityUserKey, Int]]("""{"abc":1}""")(c) ==> Map(ParityUserKey("abc") -> 1)
        }
      }

      test("fills the defaults of transient parameters") {
        val cRuntime =
          runtime(OneValueRuntimeConfigs.inline) { implicit c => KindlingsJsonValueCodec.derived[List[ParityTaggedId]] }
        List[JsonValueCodec[List[ParityTaggedId]]](OptimizedCodecs.inlineTagged, cRuntime).foreach { c =>
          writeToString(List(ParityTaggedId(1, "x")))(c) ==> "[1]"
          readFromString[List[ParityTaggedId]]("[1]")(c) ==> List(ParityTaggedId(1))
        }
      }
    }

    group("contradictory runtime configs") {
      test("are rejected when the codec is created, with jsoniter-scala's messages") {
        def rejected(config: JsoniterConfig): String =
          intercept[IllegalArgumentException] {
            implicit val c: JsoniterConfig = config
            KindlingsJsonValueCodec.derived[ParityMultiListAdt]
          }.getMessage
        rejected(ContradictoryRuntimeConfigs.bothOnly) ==>
          "'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously"
        rejected(ContradictoryRuntimeConfigs.requireAndTransientEmpty) ==>
          "'requireCollectionFields' and 'transientEmpty' cannot be 'true' simultaneously"
        rejected(ContradictoryRuntimeConfigs.alwaysEmitWithoutDiscriminator) ==>
          "'discriminatorFieldName' should not be 'None' when 'alwaysEmitDiscriminator' is 'true'"
        val inline = intercept[IllegalArgumentException] {
          implicit val c: JsoniterConfig = ContradictoryRuntimeConfigs.bothOnly
          KindlingsJsonValueCodec.writeToString(1)
        }
        inline.getMessage ==> "'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously"
      }
    }

    group("bit sets (Spec ~2185-2201)") {
      test("isStringified writes and reads their values as strings; values outside the limit are rejected") {
        type M = scala.collection.mutable.LongMap[scala.collection.immutable.IntMap[scala.collection.mutable.BitSet]]
        val value: M = scala.collection.mutable.LongMap(
          1L -> scala.collection.immutable.IntMap(
            2 -> scala.collection.mutable.BitSet(4, 5, 6),
            3 -> scala.collection.mutable.BitSet.empty
          )
        )
        val c = runtime(StringifiedRuntimeConfigs.stringified) { implicit c => KindlingsJsonValueCodec.derived[M] }
        writeToString(value)(c) ==> """{"1":{"2":["4","5","6"],"3":[]}}"""
        readFromString[M]("""{"1":{"2":["4","5","6"],"3":[]}}""")(c) ==> value
        val plain = KindlingsJsonValueCodec.derived[scala.collection.immutable.BitSet]
        readFromString[scala.collection.immutable.BitSet]("[1,200]")(plain) ==> scala.collection.immutable.BitSet(1, 200)
        decodeError("illegal value for bit set")(readFromString[scala.collection.immutable.BitSet]("[1024]")(plain))
      }
    }

    group("porting helpers") {
      test("@named on fields renames them, as @fieldName") {
        val c = KindlingsJsonValueCodec.derived[ParityNamedField]
        writeToString(ParityNamedField(1, 2))(c) ==> """{"renamed":1,"other":2}"""
        readFromString[ParityNamedField]("""{"renamed":1,"other":2}""")(c) ==> ParityNamedField(1, 2)
      }

      test("withDiscriminatorFieldName and the JsonCodecMaker.makeXxx presets") {
        JsoniterConfig.jsoniterScalaDefaults.withDiscriminatorFieldName(None).discriminatorFieldName ==> None
        JsoniterConfig.makeWithoutDiscriminator.discriminatorFieldName ==> None
        JsoniterConfig.makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName.discriminatorFieldName ==> Some("name")
        writeToString(List[ParityAdt](ParityAAA(1), ParityDDD))(OptimizedCodecs.circeLikePreset) ==>
          """[{"ParityAAA":{"a":1}},{"ParityDDD":{}}]"""
        writeToString(ParityPresetRecord(Nil))(OptimizedCodecs.openapiPreset) ==> """{"l":[],"d":1}"""
        decodeError("""missing required field "l"""")(readFromString[ParityPresetRecord]("{}")(OptimizedCodecs.openapiPreset))
        val adt = OptimizedCodecs.openapiPresetAdt
        writeToString(List[ParityAdt](ParityAAA(1), ParityDDD))(adt) ==> """[{"kind":"aaa","a":1},{"kind":"ParityDDD"}]"""
        readFromString[List[ParityAdt]]("""[{"a":1,"kind":"aaa"}]""")(adt) ==> List(ParityAAA(1))
      }
    }

    group("null and default values") {
      test("nullValue is None for options, empty for collections, zero for primitives (genNullValue)") {
        KindlingsJsonValueCodec.derived[Option[Int]].nullValue ==> None
        KindlingsJsonValueCodec.derived[List[Int]].nullValue ==> Nil
        KindlingsJsonValueCodec.derived[Map[String, Int]].nullValue ==> Map.empty[String, Int]
        KindlingsJsonValueCodec.derived[Int].nullValue ==> 0
        KindlingsJsonValueCodec.derived[java.lang.Integer].nullValue ==> java.lang.Integer.valueOf(0)
        assert(KindlingsJsonValueCodec.derived[ParityDefaults2].nullValue == null)
      }

      test("a top-level JSON null decodes as the default argument when it is not null") {
        readFromString[List[Int]]("null")(KindlingsJsonValueCodec.derived[List[Int]]) ==> Nil
        val inner = KindlingsJsonValueCodec.derived[ParityDefaults2]
        val withDefault = new JsonValueCodec[ParityDefaults2] {
          def nullValue: ParityDefaults2 = ParityDefaults2(st = "D")
          def decodeValue(in: com.github.plokhotnyuk.jsoniter_scala.core.JsonReader, d: ParityDefaults2): ParityDefaults2 =
            inner.decodeValue(in, d)
          def encodeValue(x: ParityDefaults2, out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
            inner.encodeValue(x, out)
        }
        readFromString[ParityDefaults2]("null")(withDefault) ==> ParityDefaults2(st = "D")
        decodeError("expected '{'")(readFromString[ParityDefaults2]("null")(inner))
      }

      test("null for defaulted fields (also a Some-defaulted Option) is the default (Defaults2, Spec ~2530)") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityDefaults2] }
        List[JsonValueCodec[ParityDefaults2]](KindlingsJsonValueCodec.derived[ParityDefaults2], cRuntime).foreach { c =>
          readFromString[ParityDefaults2]("{}")(c) ==> ParityDefaults2()
          readFromString[ParityDefaults2]("""{"st":null,"bi":null,"oc":null,"l":null}""")(c) ==> ParityDefaults2()
        }
      }

      test("an empty JSON collection for a collection field with a default decodes as the default (Defaults2, Spec ~2530)") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityDefaults2] }
        val cRuntimeRequire =
          runtime(ClassesRuntimeConfigs.requireDefaults) { implicit c => KindlingsJsonValueCodec.derived[ParityDefaults2] }
        List[JsonValueCodec[ParityDefaults2]](KindlingsJsonValueCodec.derived[ParityDefaults2], cRuntime).foreach { c =>
          readFromString[ParityDefaults2]("""{"l":[],"ab":[],"m":{},"s":[],"v":[]}""")(c) ==> ParityDefaults2()
          readFromString[ParityDefaults2]("""{"l":null,"ab":null,"m":null,"s":null}""")(c) ==> ParityDefaults2()
          readFromString[ParityDefaults2]("""{"l":[1],"m":{"2":false},"v":[3]}""")(c) ==>
            ParityDefaults2(l = List(1), m = Map(2 -> false), v = Vector(3))
        }
        // requireDefaultFields only rejects missing fields: an empty collection is still the default
        List[JsonValueCodec[ParityDefaults2]](OptimizedCodecs.defaults2RequireDefaults, cRuntimeRequire).foreach { c =>
          readFromString[ParityDefaults2](
            """{"st":"VVV","bi":-1,"oc":"X","l":[],"ab":[],"m":{},"s":[],"v":[]}"""
          )(c) ==> ParityDefaults2()
          decodeError("""missing required field "st"""")(readFromString[ParityDefaults2]("""{"l":[]}""")(c))
        }
        val adtRuntime =
          runtime(ClassesRuntimeConfigs.discriminator) { implicit c => KindlingsJsonValueCodec.derived[ParityDefaultsAdt] }
        List[JsonValueCodec[ParityDefaultsAdt]](OptimizedCodecs.defaultsAdt, adtRuntime).foreach { c =>
          readFromString[ParityDefaultsAdt]("""{"type":"ParityDefaultsLeaf","l":[]}""")(c) ==> ParityDefaultsLeaf()
        }
      }

      test("a user-provided field codec receives the field's default as the default argument (Spec ~674, ~3673)") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityTagged] }
        List[JsonValueCodec[ParityTagged]](KindlingsJsonValueCodec.derived[ParityTagged], cRuntime).foreach { c =>
          readFromString[ParityTagged]("""{"a":"keep","b":"keep"}""")(c) ==>
            ParityTagged(ParityTag("dflt"), ParityTag("null-value"))
          readFromString[ParityTagged]("""{"a":null,"b":null}""")(c) ==>
            ParityTagged(ParityTag("dflt"), ParityTag("null-value"))
          readFromString[ParityTagged]("""{"b":"x"}""")(c) ==> ParityTagged(ParityTag("dflt"), ParityTag("x"))
          readFromString[ParityTagged]("""{"a":"y","b":"x"}""")(c) ==> ParityTagged(ParityTag("y"), ParityTag("x"))
        }
        val adtRuntime =
          runtime(ClassesRuntimeConfigs.discriminator) { implicit c => KindlingsJsonValueCodec.derived[ParityDefaultsAdt] }
        List[JsonValueCodec[ParityDefaultsAdt]](OptimizedCodecs.defaultsAdt, adtRuntime).foreach { c =>
          readFromString[ParityDefaultsAdt]("""{"type":"ParityDefaultsLeaf","t":"keep"}""")(c) ==> ParityDefaultsLeaf()
        }
      }

      test("a custom Option codec decides what null is, the default fills a missing field (Spec ~674)") {
        import ParityCustomOptionCodecs.*
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityCustomOptionEx] }
        List[JsonValueCodec[ParityCustomOptionEx]](KindlingsJsonValueCodec.derived[ParityCustomOptionEx], cRuntime)
          .foreach { c =>
            readFromString[ParityCustomOptionEx]("""{"opt":null,"level":null}""")(c) ==> ParityCustomOptionEx(None, None)
            writeToString(ParityCustomOptionEx(None, None))(c) ==> """{"opt":null,"level":null}"""
            readFromString[ParityCustomOptionEx]("{}")(c) ==> ParityCustomOptionEx()
            readFromString[ParityCustomOptionEx]("""{"opt":"pakikisama","level":5}""")(c) ==>
              ParityCustomOptionEx(Some("pakikisama"), Some(5))
          }
      }
    }

    group("user-provided field codecs") {
      test("receive the field's default value as `default`, otherwise their nullValue") {
        val cRuntime =
          runtime(ClassesRuntimeConfigs.default) { implicit c => KindlingsJsonValueCodec.derived[ParityHasCounter] }
        List[JsonValueCodec[ParityHasCounter]](KindlingsJsonValueCodec.derived[ParityHasCounter], cRuntime).foreach { c =>
          readFromString[ParityHasCounter]("""{"c":{},"d":{}}""")(c) ==> ParityHasCounter(ParityCounter(7), ParityCounter(-1))
          readFromString[ParityHasCounter]("""{"c":1,"d":2}""")(c) ==> ParityHasCounter(ParityCounter(1), ParityCounter(2))
        }
      }
    }
  }
}
