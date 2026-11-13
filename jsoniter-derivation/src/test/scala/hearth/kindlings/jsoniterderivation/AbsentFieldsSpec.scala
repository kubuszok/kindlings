package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec, ReaderConfig}
import hearth.MacroSuite

/** Absent-field decoding semantics, aligned with jsoniter-scala's `JsonCodecMaker.make` (issue #264):
  *
  *   - a missing `Option` field decodes to `None`,
  *   - a missing field with a default value decodes to the default (unless `requireDefaultFields`),
  *   - a missing collection/map field decodes to an empty one (unless `requireCollectionFields`),
  *   - any other missing field fails with `missing required field "<name>"`,
  *   - `transientDefault`/`transientEmpty`/`transientNone` only affect encoding.
  *
  * Every scenario is checked twice: with a config known at compile time (optimized codegen: `JsoniterConfig.default` or
  * the explicitly configured `OptimizedCodecs`) and with a config only known at runtime (generic codegen: the
  * `@volatile var`s of `AbsentFieldsRuntimeConfigs`). The "codegen paths" group proves which path each one takes.
  */
final class AbsentFieldsSpec extends MacroSuite {

  private def missingField(name: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(
      error.getMessage.contains(s"""missing required field "$name""""),
      s"expected missing required field '$name', got: ${error.getMessage}"
    )
  }

  private def checkAll(codec: JsonValueCodec[AbsentFieldsRecord], codecRuntime: JsonValueCodec[AbsentFieldsRecord])(
      f: JsonValueCodec[AbsentFieldsRecord] => Unit
  ): Unit = {
    f(codec)
    f(codecRuntime)
  }

  group("absent fields (jsoniter-scala parity)") {

    group("codegen paths") {
      // Both codegens report unexpected keys with jsoniter-scala's wording; the generic one decodes inside
      // `JsoniterDerivationUtils.readObject`, the optimized one inlines the read loop. If one of these fails, the codecs
      // below no longer test the path they claim to test.
      test("OptimizedCodecs use the optimized codegen, runtime configs the generic one") {
        val json = """{"label":"a","count":1,"extra":1}"""
        val withStackTrace = ReaderConfig.withThrowReaderExceptionWithStackTrace(true)
        // Stack frames name classes on the JVM and Scala Native, not in (optimized) Scala.js output: there only the
        // error messages are checked.
        val framesNameClasses = new Exception().getStackTrace.exists(_.toString.contains("AbsentFieldsSpec"))
        def readsViaRuntimeHelper(e: Throwable): Boolean =
          !framesNameClasses || e.getStackTrace.exists { f =>
            val frame = f.toString
            frame.contains("JsoniterDerivationUtils") && frame.contains("readObject")
          }
        val optimized = intercept[JsonReaderException](
          readFromString[AbsentPrimitive](json, withStackTrace)(OptimizedCodecs.strictPrimitive)
        )
        assert(optimized.getMessage.contains("unexpected field \"extra\""), optimized.getMessage)
        assert(!framesNameClasses || !readsViaRuntimeHelper(optimized), "expected the optimized codegen")
        val runtimeCodec = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.strict
          KindlingsJsonValueCodec.derived[AbsentPrimitive]
        }
        val runtime =
          intercept[JsonReaderException](readFromString[AbsentPrimitive](json, withStackTrace)(runtimeCodec))
        assert(runtime.getMessage.contains("unexpected field \"extra\""), runtime.getMessage)
        assert(readsViaRuntimeHelper(runtime), "expected the generic (runtime config) codegen")
      }
    }

    group("issue #264 reproduction") {

      test("missing Option field decodes as None") {
        val codec = KindlingsJsonValueCodec.derived[AbsentOptionOnly]
        readFromString[AbsentOptionOnly]("""{"next":"/page/2"}""")(codec) ==> AbsentOptionOnly(Some("/page/2"))
        readFromString[AbsentOptionOnly]("""{"next":null}""")(codec) ==> AbsentOptionOnly(None)
        readFromString[AbsentOptionOnly]("{}")(codec) ==> AbsentOptionOnly(None)
      }

      test("missing Option field with None default decodes as None") {
        val codec = KindlingsJsonValueCodec.derived[AbsentOptionWithNoneDefault]
        readFromString[AbsentOptionWithNoneDefault]("{}")(codec) ==> AbsentOptionWithNoneDefault(None)
      }

      test("missing Option field with Some default decodes as the default") {
        val codec = KindlingsJsonValueCodec.derived[AbsentOptionWithSomeDefault]
        readFromString[AbsentOptionWithSomeDefault]("{}")(codec) ==> AbsentOptionWithSomeDefault(Some("d"))
        // as jsoniter-scala (Defaults2, Spec ~2530): JSON null is the field's default too
        readFromString[AbsentOptionWithSomeDefault]("""{"opt":null}""")(codec) ==> AbsentOptionWithSomeDefault(Some("d"))
      }
    }

    group("default config") {
      val codec = KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.default
        KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
      }

      test("all fields present") {
        checkAll(codec, codecRuntime) { c =>
          readFromString[AbsentFieldsRecord](
            """{"name":"a","opt":"o","n":1,"xs":[1],"ys":[2],"m":{"k":3}}"""
          )(c) ==> AbsentFieldsRecord("a", Some("o"), 1, List(1), Vector(2), Map("k" -> 3))
        }
      }

      test("Option, defaulted and collection fields absent") {
        checkAll(codec, codecRuntime) { c =>
          readFromString[AbsentFieldsRecord]("""{"name":"a"}""")(c) ==>
            AbsentFieldsRecord("a", None, 7, Nil, Vector.empty, Map.empty)
        }
      }

      test("missing required field fails") {
        checkAll(codec, codecRuntime) { c =>
          missingField("name")(readFromString[AbsentFieldsRecord]("""{"opt":"x"}""")(c))
          missingField("name")(readFromString[AbsentFieldsRecord]("{}")(c))
        }
      }

      test("explicit null for a String field without default is rejected (as in jsoniter-scala)") {
        checkAll(codec, codecRuntime) { c =>
          val error = intercept[JsonReaderException](readFromString[AbsentFieldsRecord]("""{"name":null}""")(c))
          assert(!error.getMessage.contains("missing required field"), error.getMessage)
        }
      }

      test("generic case class: defaults, Option and collections of a type parameter") {
        val c = KindlingsJsonValueCodec.derived[AbsentGeneric[String]]
        val cRuntime = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.default
          KindlingsJsonValueCodec.derived[AbsentGeneric[String]]
        }
        List(c, cRuntime).foreach { codec =>
          readFromString[AbsentGeneric[String]]("""{"value":"v"}""")(codec) ==>
            AbsentGeneric[String]("v", None, Nil, 5)
          missingField("value")(readFromString[AbsentGeneric[String]]("{}")(codec))
        }
      }

      test("named tuple style: case class with only optional fields decodes from {}") {
        val c = KindlingsJsonValueCodec.derived[AllOptionalWithDefaults]
        readFromString[AllOptionalWithDefaults]("{}")(c) ==> AllOptionalWithDefaults()
      }

      test("missing required primitive field fails") {
        val c = KindlingsJsonValueCodec.derived[AbsentPrimitive]
        val cRuntime = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.default
          KindlingsJsonValueCodec.derived[AbsentPrimitive]
        }
        missingField("count")(readFromString[AbsentPrimitive]("""{"label":"x"}""")(c))
        missingField("count")(readFromString[AbsentPrimitive]("""{"label":"x"}""")(cRuntime))
      }

      test("missing field error uses the mapped field name") {
        missingField("first_name")(readFromString[AbsentCamelCase]("{}")(OptimizedCodecs.absentSnakeCase))
        implicit val config: JsoniterConfig = JsoniterConfig.default.withSnakeCaseFieldNames
        val c = KindlingsJsonValueCodec.derived[AbsentCamelCase]
        missingField("first_name")(readFromString[AbsentCamelCase]("{}")(c))
        val cRenamed = KindlingsJsonValueCodec.derived[AbsentRenamed]
        missingField("the_name")(readFromString[AbsentRenamed]("{}")(cRenamed))
      }

      test("writing still emits every field") {
        writeToString(AbsentFieldsRecord("a", None, 7, Nil, Vector.empty, Map.empty))(codec) ==>
          """{"name":"a","opt":null,"n":7,"xs":[],"ys":[],"m":{}}"""
      }
    }

    group("transient flags only affect writing") {
      val codec = OptimizedCodecs.absentAllTransient
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.allTransient
        KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
      }

      test("absent fields filled the same way") {
        checkAll(codec, codecRuntime) { c =>
          readFromString[AbsentFieldsRecord]("""{"name":"a"}""")(c) ==>
            AbsentFieldsRecord("a", None, 7, Nil, Vector.empty, Map.empty)
        }
      }

      test("missing required field still fails") {
        checkAll(codec, codecRuntime) { c =>
          missingField("name")(readFromString[AbsentFieldsRecord]("""{"opt":"x"}""")(c))
        }
      }

      test("writing omits None, default and empty fields") {
        checkAll(codec, codecRuntime) { c =>
          writeToString(AbsentFieldsRecord("a", None, 7, Nil, Vector.empty, Map.empty))(c) ==> """{"name":"a"}"""
        }
      }
    }

    group("requireDefaultFields") {
      val codec = OptimizedCodecs.absentRequireDefault
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.requireDefault
        KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
      }

      test("missing defaulted field fails") {
        checkAll(codec, codecRuntime) { c =>
          missingField("n")(readFromString[AbsentFieldsRecord]("""{"name":"a"}""")(c))
        }
      }

      test("Option and collection fields are still optional") {
        checkAll(codec, codecRuntime) { c =>
          readFromString[AbsentFieldsRecord]("""{"name":"a","n":1}""")(c) ==>
            AbsentFieldsRecord("a", None, 1, Nil, Vector.empty, Map.empty)
        }
      }

      test("Option field with a default is still optional") {
        val cRuntime = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.requireDefault
          KindlingsJsonValueCodec.derived[AbsentOptionWithNoneDefault]
        }
        List(OptimizedCodecs.absentRequireDefaultOptionNone, cRuntime).foreach { c =>
          readFromString[AbsentOptionWithNoneDefault]("{}")(c) ==> AbsentOptionWithNoneDefault(None)
        }
      }
    }

    group("requireCollectionFields") {
      val codec = OptimizedCodecs.absentRequireCollection
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.requireCollection
        KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
      }

      test("missing collection without default fails") {
        checkAll(codec, codecRuntime) { c =>
          missingField("ys")(readFromString[AbsentFieldsRecord]("""{"name":"a"}""")(c))
          missingField("m")(readFromString[AbsentFieldsRecord]("""{"name":"a","ys":[]}""")(c))
        }
      }

      test("Option fields are not affected, defaulted collections use the default") {
        checkAll(codec, codecRuntime) { c =>
          readFromString[AbsentFieldsRecord]("""{"name":"a","ys":[],"m":{}}""")(c) ==>
            AbsentFieldsRecord("a", None, 7, Nil, Vector.empty, Map.empty)
        }
      }
    }

    group("optimized and runtime codecs agree on strictness flags") {

      test("skipUnexpectedFields = false rejects unknown fields") {
        val c = OptimizedCodecs.absentStrict
        val cRuntime = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.strict
          KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
        }
        List(c, cRuntime).foreach { codec =>
          val _ = intercept[JsonReaderException](readFromString[AbsentFieldsRecord]("""{"name":"a","extra":1}""")(codec))
        }
      }

      test("checkFieldDuplication rejects duplicated fields") {
        val c = OptimizedCodecs.absentCheckDuplication
        val cRuntime = {
          implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.checkDuplication
          KindlingsJsonValueCodec.derived[AbsentFieldsRecord]
        }
        List(c, cRuntime).foreach { codec =>
          val _ = intercept[JsonReaderException](readFromString[AbsentFieldsRecord]("""{"name":"a","name":"b"}""")(codec))
        }
      }
    }

    group("sealed trait with discriminator") {
      val codec = OptimizedCodecs.absentDiscriminator
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.discriminator
        KindlingsJsonValueCodec.derived[AbsentAdt]
      }

      test("absent fields filled in a discriminated leaf") {
        List(codec, codecRuntime).foreach { c =>
          readFromString[AbsentAdt]("""{"type":"AbsentLeaf","name":"a"}""")(c) ==>
            (AbsentLeaf("a", None, 3, Nil): AbsentAdt)
        }
      }

      test("missing required field in a discriminated leaf fails") {
        List(codec, codecRuntime).foreach { c =>
          missingField("name")(readFromString[AbsentAdt]("""{"type":"AbsentLeaf"}""")(c))
        }
      }
    }

    group("sealed trait wrapper encoding") {
      val codec = KindlingsJsonValueCodec.derived[AbsentAdt]
      val codecRuntime = {
        implicit val config: JsoniterConfig = AbsentFieldsRuntimeConfigs.default
        KindlingsJsonValueCodec.derived[AbsentAdt]
      }

      test("absent fields filled in a wrapped leaf") {
        List(codec, codecRuntime).foreach { c =>
          readFromString[AbsentAdt]("""{"AbsentLeaf":{"name":"a"}}""")(c) ==> (AbsentLeaf("a", None, 3, Nil): AbsentAdt)
        }
      }

      test("missing required field in a wrapped leaf fails") {
        List(codec, codecRuntime).foreach { c =>
          missingField("name")(readFromString[AbsentAdt]("""{"AbsentLeaf":{}}""")(c))
        }
      }
    }
  }
}
