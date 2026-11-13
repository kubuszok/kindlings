package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

final case class UbJavaEnumHolder(c: UbJavaColor)

object UbJavaEnumRuntimeConfigs {
  @volatile var lowerCase: UBJsonConfig =
    UBJsonConfig().withDiscriminator("type").withJavaEnumValueNameMapper((s: String) => s.toLowerCase)
}

/** Java enums as in jsoniter-scala: strings whatever the discriminator, named by `javaEnumValueNameMapper`. JVM-only,
  * because the Java enum fixture is a Java source.
  */
final class UBJsonJavaEnumJvmSpec extends MacroSuite {

  test("Java enums are strings whatever the discriminator") {
    val static = {
      implicit val c: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
      UBJsonValueCodec.derived[UbJavaEnumHolder]
    }
    UbRender.encode(UbJavaEnumHolder(UbJavaColor.GREEN))(static) ==> """{"c":"GREEN"}"""
    UbRender.roundTrip(UbJavaEnumHolder(UbJavaColor.GREEN))(static) ==> UbJavaEnumHolder(UbJavaColor.GREEN)
  }

  test("javaEnumValueNameMapper renames the values") {
    val runtime = {
      implicit val c: UBJsonConfig = UbJavaEnumRuntimeConfigs.lowerCase
      UBJsonValueCodec.derived[UbJavaEnumHolder]
    }
    UbRender.encode(UbJavaEnumHolder(UbJavaColor.RED))(runtime) ==> """{"c":"red"}"""
    UbRender.roundTrip(UbJavaEnumHolder(UbJavaColor.RED))(runtime) ==> UbJavaEnumHolder(UbJavaColor.RED)
  }
}
