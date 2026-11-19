package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite
import hearth.kindlings.ubjsonderivation.annotations.fieldName
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

case class UbKeyed(a: Int, @fieldName("bee") b: String = "d", c: Option[Long] = None, d: List[Int] = Nil)
case class UbKeyedOuter(inner: UbKeyed, more: List[UbKeyed], n: Int = 0)

// wider than the 8 keys compared one by one: found in a per-instance index also with a config known at compile time
case class UbKeyed10(
    k0: Int,
    k1: String = "a",
    k2: Int = 2,
    k3: Int = 3,
    k4: Int = 4,
    k5: Int = 5,
    k6: Int = 6,
    k7: Int = 7,
    k8: Option[Int] = None,
    k9: List[Int] = Nil
)

sealed trait UbKeyedAdt
object UbKeyedAdt {
  case class Leaf(a: Int, b: String = "x") extends UbKeyedAdt
  case object Empty extends UbKeyedAdt
}

/** Configs only known at runtime (`@volatile var`s: the macro cannot evaluate them). */
object UbRuntimeConfigDecoderConfigs {
  @volatile var mapperCalls: Int = 0
  @volatile var upperCase: UBJsonConfig = UBJsonConfig().withFieldNameMapper { (name: String) =>
    mapperCalls += 1
    name.toUpperCase
  }
  @volatile var strict: UBJsonConfig = UBJsonConfig().withSkipUnexpectedFields(false).withCheckFieldDuplication
  @volatile var discriminator: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
}

/** The case class decoder finding keys in a per-instance index: used when the config is only known at runtime, and for
  * classes with more than 8 keys.
  */
final class UBJsonIndexedDecoderSpec extends MacroSuite {

  private def obj(fields: (String, UBJsonWriter => Unit)*): Array[Byte] = {
    val w = new UBJsonWriter()
    w.writeObjectStart()
    fields.foreach { case (key, write) =>
      w.writeFieldName(key)
      write(w)
    }
    w.writeObjectEnd()
    w.toByteArray
  }

  private def decode[A](bytes: Array[Byte])(codec: UBJsonValueCodec[A]): A = codec.decode(new UBJsonReader(bytes))

  test("FieldIndex finds the first field with a key, by comparisons (up to 8 names) or by hash") {
    List(3, 7, 8, 100).foreach { size => // (+1 duplicate: 4, 8, 9 and 101 names)
      val names = Array.tabulate(size)(i => s"k$i") :+ "k0" // a duplicate: the first field wins
      val index = UBJsonDerivationUtils.fieldIndex(names)
      names.indices.dropRight(1).foreach(i => index.indexOf(s"k$i") ==> i)
      index.indexOf("k0") ==> 0
      index.indexOf("unknown") ==> -1
      index.indexOf("") ==> -1
      index.name(1) ==> "k1"
    }
    UBJsonDerivationUtils.fieldIndex(Array.empty[String]).indexOf("k0") ==> -1
  }

  test("keys in any order, @fieldName, absent fields and unknown keys") {
    val codec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.strict.copy(skipUnexpectedFields = true)
      UBJsonValueCodec.derived[UbKeyed]
    }
    val list: UBJsonWriter => Unit = { w =>
      w.writeArrayStart()
      w.writeInt(5)
      w.writeArrayEnd()
    }
    decode(
      obj("d" -> list, "unknown" -> (_.writeString("skipped")), "bee" -> (_.writeString("B")), "a" -> (_.writeInt(1)))
    )(codec) ==> UbKeyed(1, "B", d = List(5))
    decode(obj("c" -> (_.writeLong(3L)), "a" -> (_.writeInt(1))))(codec) ==> UbKeyed(1, c = Some(3L))
    val missing = intercept[UBJsonReaderException](decode(obj("d" -> list, "bee" -> (_.writeString("B"))))(codec))
    assert(missing.getMessage.contains("missing required field \"a\""), missing.getMessage)
    UbRender.roundTrip(UbKeyed(2, "e", Some(4L), List(1, 2)))(codec) ==> UbKeyed(2, "e", Some(4L), List(1, 2))
  }

  test("nested case classes") {
    val codec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.upperCase
      UBJsonValueCodec.derived[UbKeyedOuter]
    }
    val value = UbKeyedOuter(UbKeyed(1, "B"), List(UbKeyed(2, c = Some(3L)), UbKeyed(4, d = List(5))), 6)
    UbRender.encode(value)(codec) ==>
      """{"INNER":{"A":1,"bee":"B","C":null,"D":[]},""" +
      """"MORE":[{"A":2,"bee":"d","C":3,"D":[]},{"A":4,"bee":"d","C":null,"D":[5]}],"N":6}"""
    UbRender.roundTrip(value)(codec) ==> value
  }

  test("unknown and duplicated keys are rejected when configured") {
    val codec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.strict
      UBJsonValueCodec.derived[UbKeyed]
    }
    val unknown = intercept[UBJsonReaderException](decode(obj("a" -> (_.writeInt(1)), "x" -> (_.writeInt(1))))(codec))
    assert(unknown.getMessage.contains("unexpected field: x"), unknown.getMessage)
    val duplicated =
      intercept[UBJsonReaderException](decode(obj("a" -> (_.writeInt(1)), "a" -> (_.writeInt(2))))(codec))
    assert(duplicated.getMessage.contains("duplicate field: a"), duplicated.getMessage)
  }

  test("fieldNameMapper is called once per field per codec instance, not per key per decode") {
    val codec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.upperCase
      UBJsonValueCodec.derived[UbKeyed]
    }
    val bytes = obj("A" -> (_.writeInt(1)), "bee" -> (_.writeString("B")), "C" -> (_.writeLong(2L)))
    UbRuntimeConfigDecoderConfigs.mapperCalls = 0
    (1 to 3).foreach(_ => decode(bytes)(codec) ==> UbKeyed(1, "B", Some(2L)))
    // a, c and d (b has @fieldName)
    UbRuntimeConfigDecoderConfigs.mapperCalls ==> 3
  }

  test("more than 8 keys with a config known at compile time") {
    val codec = {
      implicit val config: UBJsonConfig = UBJsonConfig().withSkipUnexpectedFields(false).withCheckFieldDuplication
      UBJsonValueCodec.derived[UbKeyed10]
    }
    val list: UBJsonWriter => Unit = { w =>
      w.writeArrayStart()
      w.writeInt(1)
      w.writeArrayEnd()
    }
    decode(obj("k9" -> list, "k5" -> (_.writeInt(0)), "k0" -> (_.writeInt(1))))(codec) ==>
      UbKeyed10(1, k5 = 0, k9 = List(1))
    UbRender.roundTrip(UbKeyed10(1, "b", k8 = Some(8)))(codec) ==> UbKeyed10(1, "b", k8 = Some(8))
    val unknown = intercept[UBJsonReaderException](decode(obj("k0" -> (_.writeInt(1)), "x" -> (_.writeInt(1))))(codec))
    assert(unknown.getMessage.contains("unexpected field: x"), unknown.getMessage)
    val duplicated =
      intercept[UBJsonReaderException](decode(obj("k3" -> (_.writeInt(1)), "k3" -> (_.writeInt(2))))(codec))
    assert(duplicated.getMessage.contains("duplicate field: k3"), duplicated.getMessage)
    val missing = intercept[UBJsonReaderException](decode(obj("k1" -> (_.writeString("b"))))(codec))
    assert(missing.getMessage.contains("missing required field \"k0\""), missing.getMessage)
  }

  test("discriminator-mode leaves") {
    val codec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.discriminator
      UBJsonValueCodec.derived[List[UbKeyedAdt]]
    }
    val values = List[UbKeyedAdt](UbKeyedAdt.Leaf(1, "y"), UbKeyedAdt.Empty, UbKeyedAdt.Leaf(2))
    UbRender.roundTrip(values)(codec) ==> values
    val leafCodec = {
      implicit val config: UBJsonConfig = UbRuntimeConfigDecoderConfigs.discriminator
      UBJsonValueCodec.derived[UbKeyedAdt]
    }
    decode(obj("type" -> (_.writeString("Leaf")), "x" -> (_.writeInt(0)), "a" -> (_.writeInt(3))))(leafCodec) ==>
      UbKeyedAdt.Leaf(3)
  }
}
