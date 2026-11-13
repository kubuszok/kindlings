package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

case class UbNullableFields(s: String | Null, l: Int | Null, n: Int, ls: List[Int] | Null = null)
object UbNullableRuntimeConfigs {
  @volatile var default: UBJsonConfig = UBJsonConfig()
  @volatile var transientNull: UBJsonConfig = UBJsonConfig().withTransientNull
}

/** Scala 3 `T | Null` fields (jsoniter-scala's nullable types): `null` or a `T`, absent is `null` (never required), and
  * `transientNull` skips them when `null`.
  */
final class UBJsonNullableSpec extends MacroSuite {

  private def bytes(fields: (String, UBJsonWriter => Unit)*): Array[Byte] = {
    val w = new UBJsonWriter()
    w.writeObjectStart()
    fields.foreach { case (k, v) => w.writeFieldName(k); v(w) }
    w.writeObjectEnd()
    w.toByteArray
  }

  test("T | Null fields are null or a T, absent is null") {
    val runtime = {
      given UBJsonConfig = UbNullableRuntimeConfigs.default
      UBJsonValueCodec.derived[UbNullableFields]
    }
    List(UBJsonValueCodec.derived[UbNullableFields](using UBJsonConfig.default), runtime).foreach { codec =>
      codec.decode(new UBJsonReader(bytes("n" -> (_.writeInt(1))))) ==> UbNullableFields(null, null, 1)
      codec.decode(new UBJsonReader(bytes("s" -> (_.writeNull()), "l" -> (_.writeNull()), "n" -> (_.writeInt(1))))) ==>
        UbNullableFields(null, null, 1)
      UbRender.encode(UbNullableFields("a", 1, 2))(codec) ==> """{"s":"a","l":1,"n":2,"ls":null}"""
      UbRender.encode(UbNullableFields(null, null, 1))(codec) ==> """{"s":null,"l":null,"n":1,"ls":null}"""
      UbRender.roundTrip(UbNullableFields("a", 1, 2, List(3)))(codec) ==> UbNullableFields("a", 1, 2, List(3))
    }
  }

  test("transientNull skips them when null") {
    val runtime = {
      given UBJsonConfig = UbNullableRuntimeConfigs.transientNull
      UBJsonValueCodec.derived[UbNullableFields]
    }
    List(UBJsonValueCodec.derived[UbNullableFields](using UBJsonConfig.default.withTransientNull), runtime).foreach {
      codec =>
        UbRender.encode(UbNullableFields(null, null, 1))(codec) ==> """{"n":1}"""
        UbRender.roundTrip(UbNullableFields("a", 1, 2))(codec) ==> UbNullableFields("a", 1, 2)
        UbRender.encode(UbNullableFields("a", 1, 2))(codec) ==> """{"s":"a","l":1,"n":2}"""
    }
  }
}
