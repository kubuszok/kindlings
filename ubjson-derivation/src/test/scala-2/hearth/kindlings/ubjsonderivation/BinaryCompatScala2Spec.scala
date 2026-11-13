package hearth.kindlings.ubjsonderivation

import hearth.MacroSuite

import scala.annotation.nowarn

/** The Scala 2 `unapply` of `UBJsonConfig` kept (deprecated) for binary compatibility with Kindlings 0.3.x. */
@nowarn("cat=deprecation")
final class BinaryCompatScala2Spec extends MacroSuite {

  test("UBJsonConfig.unapply of 0.3.2 extracts the fields of 0.3.2") {
    val config = UBJsonConfig.default.withDiscriminator("t").withSetMaxInsertNumber(7)
    UBJsonConfig.unapply(config).map(t => (t._3, t._16)) ==> Some((Some("t"), 7))
    UBJsonConfig.unapply(null) ==> None
  }
}
