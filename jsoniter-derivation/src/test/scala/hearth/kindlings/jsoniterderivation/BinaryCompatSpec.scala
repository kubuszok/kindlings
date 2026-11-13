package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReader, JsonReaderException}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.internal.runtime.{JsoniterDerivationFactories, JsoniterDerivationUtils}

import scala.annotation.nowarn

object BinaryCompatSpecEnum extends Enumeration {
  val A, B = Value
}

/** The members kept (deprecated) for binary compatibility with Kindlings 0.3.x keep their 0.3.2 behaviour. */
@nowarn("cat=deprecation")
final class BinaryCompatSpec extends MacroSuite {

  private def decodeWith[A](json: String)(decode: JsonReader => A): A =
    readFromString[A](json)(
      JsoniterDerivationFactories.codecInstance[A](null.asInstanceOf[A], (in, _) => decode(in), (_, _) => ())
    )

  group("JsoniterConfig of 0.3.2") {

    test("the old-arity constructor and apply fill the new fields with their defaults") {
      val f: String => String = _.toUpperCase
      val viaApply = JsoniterConfig(
        f,
        f,
        Some("t"),
        false,
        true,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        true,
        10,
        11,
        12,
        13,
        14,
        15,
        f,
        true,
        false,
        false
      )
      val viaNew = new JsoniterConfig(
        f,
        f,
        Some("t"),
        false,
        true,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        true,
        10,
        11,
        12,
        13,
        14,
        15,
        f,
        true,
        false,
        false
      )
      for (c <- List(viaApply, viaNew)) {
        c.discriminatorFieldName ==> Some("t")
        c.checkFieldDuplication ==> true
        c.bitSetValueLimit ==> 15
        c.skipNestedOptionValues ==> true
        c.requireDiscriminatorFirst ==> true
        c.bigIntDigitsLimit ==> 308
        c.scalaTransientSupport ==> false
        c.transientNull ==> false
      }
    }

    test("the old-arity copy keeps the new fields") {
      val base = JsoniterConfig.default.withRequireDiscriminatorFirst(false).withBigIntDigitsLimit(5)
      val c = base.copy(
        identity,
        identity,
        None,
        true,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        34,
        6178,
        308,
        1,
        2,
        1024,
        identity,
        false,
        false,
        false
      )
      c.mapMaxInsertNumber ==> 1
      c.requireDiscriminatorFirst ==> false
      c.bigIntDigitsLimit ==> 5
    }

    test("the 0.3.2 name mappers forward to the jsoniter-scala ones") {
      JsoniterConfig.snakeCase("fooBar") ==> "foo_bar"
      JsoniterConfig.kebabCase("fooBar") ==> "foo-bar"
      JsoniterConfig.pascalCase("fooBar") ==> "FooBar"
    }
  }

  group("JsoniterDerivationFactories / JsoniterDerivationUtils of 0.3.2") {

    test("codecInstance / jsonCodecInstance take a strict null value") {
      JsoniterDerivationFactories
        .codecInstance[Int](7, (in, _) => in.readInt(), (x, out) => out.writeVal(x))
        .nullValue ==> 7
      val codec = JsoniterDerivationFactories.jsonCodecInstance[Int](
        7,
        (in, _) => in.readInt(),
        (x, out) => out.writeVal(x),
        in => in.readKeyAsInt(),
        (x, out) => out.writeKey(x)
      )
      codec.nullValue ==> 7
      writeToString(1)(codec) ==> "1"
    }

    test("Scala Enumeration ids") {
      decodeWith("1")(in => JsoniterDerivationUtils.readScalaEnumValueId(in)(BinaryCompatSpecEnum(_))) ==>
        BinaryCompatSpecEnum.B
      val codec = JsoniterDerivationFactories.codecInstance[BinaryCompatSpecEnum.Value](
        null,
        (in, _) => BinaryCompatSpecEnum(in.readInt()),
        (x, out) => JsoniterDerivationUtils.writeScalaEnumValueId(out, x)
      )
      writeToString(BinaryCompatSpecEnum.B)(codec) ==> "1"
    }

    test("discriminator, inline object and remainder") {
      decodeWith("""{"type":"A","x":1,"y":2}""") { in =>
        JsoniterDerivationUtils.readWithDiscriminator(in, "type") { name =>
          JsoniterDerivationUtils.readObjectInline[(String, Int)](in, 1, arr => (name, arr(0).asInstanceOf[Int])) {
            (key, arr, r) => if (key == "x") arr(0) = r.readInt() else r.skip()
          }
        }
      } ==> (("A", 1))
      decodeWith("""{"type":"A","x":1}""") { in =>
        JsoniterDerivationUtils.readWithDiscriminator(in, "type") { name =>
          JsoniterDerivationUtils.skipInlineObjectRemainder(in)
          name
        }
      } ==> "A"
      intercept[JsonReaderException](
        decodeWith("""{"x":1,"type":"A"}""")(in => JsoniterDerivationUtils.readWithDiscriminator(in, "type")(identity))
      )
    }

    test("validation helpers") {
      intercept[IllegalArgumentException](JsoniterDerivationUtils.throwMissingField("a")).getMessage ==>
        "missing required field: a"
      decodeWith("1.5")(in =>
        JsoniterDerivationUtils.validateBigDecimal(in, in.readBigDecimal(null), 34, 6178, 308)
      ) ==>
        BigDecimal("1.5")
      intercept[JsonReaderException](
        decodeWith("123456")(in => JsoniterDerivationUtils.validateBigInt(in, in.readBigInt(null), 3))
      )
    }
  }
}
