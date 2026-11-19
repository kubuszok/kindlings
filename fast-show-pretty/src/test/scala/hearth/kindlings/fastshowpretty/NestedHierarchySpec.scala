package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

/** A sealed hierarchy nested in a class with a case object in the companion of the sealed trait: crashed the Scala 3
  * compiler with "missing outer accessor" on Hearth 0.4.5 - see
  * `docs/research/hearth-bug-enum-child-this-type-prefix.md` (fixed in Hearth 985e5e6).
  */
final class NestedHierarchySpec extends MacroSuite {

  sealed trait InnerShape
  object InnerShape {
    case object Empty extends InnerShape
  }
  final case class InnerCircle(r: Double) extends InnerShape

  test("sealed hierarchy nested in a class, case object in the companion, renders") {
    FastShowPretty.render[InnerShape](InnerShape.Empty, RenderConfig.Default) ==> "(Empty()): InnerShape"
    FastShowPretty.render[InnerShape](InnerCircle(1.0), RenderConfig.Default) ==>
      """(InnerCircle(
        |    r = 1.0d
        |  )): InnerShape""".stripMargin
  }
}
