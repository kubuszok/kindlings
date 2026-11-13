package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

/** Scala 2 only: a config passed explicitly which defines code (a partial-function literal) is bound once (it would
  * otherwise be duplicated in the generated code, failing with `double definition` errors).
  */
final class UBJsonScala2ConfigSpec extends MacroSuite {

  test("a config argument defining a partial function is evaluated once") {
    val codec = UBJsonValueCodec.derived[UbMixed](UBJsonConfig.default.withDiscriminator("type").withFieldNameMapper {
      case "x" => "X"
    })
    UbRender.encode(UbMixed.Point(1, 2): UbMixed)(codec) ==> """{"type":"Point","X":1,"y":2}"""
    UbRender.roundTrip(UbMixed.Point(1, 2): UbMixed)(codec) ==> (UbMixed.Point(1, 2): UbMixed)
  }
}
