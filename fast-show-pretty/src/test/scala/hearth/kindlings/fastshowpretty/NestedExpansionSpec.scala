package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

// Deriving `NeRoot` reads the children of the sealed `NeLevel`; on Scala 2 that makes the compiler type
// `NeUntyped.show` (whose type is inferred) by expanding its macro INSIDE the first expansion. Hearth 0.4.5 failed that
// nested, independent expansion with "MIO timeout is already set" (fixed in Hearth
// 985e5e6).
final case class NeHttp(port: Int)
final case class NeRoot(http: NeHttp, level: NeLevel)
sealed trait NeLevel
case object NeDebug extends NeLevel
object NeRoot {
  implicit val show: FastShowPretty[NeRoot] = FastShowPretty.derived[NeRoot]
}
object NeUntyped {
  val show = FastShowPretty.derived[NeHttp]
}

final class NestedExpansionSpec extends MacroSuite {

  private def render[A](show: FastShowPretty[A], value: A): String =
    show.render(new StringBuilder, RenderConfig.Default, 0)(value).toString

  test("a derivation reading sealed children next to an untyped derived val in the same file") {
    render(NeUntyped.show, NeHttp(80)) ==>
      """NeHttp(
        |  port = 80
        |)""".stripMargin
    render(NeRoot.show, NeRoot(NeHttp(80), NeDebug)) ==>
      """NeRoot(
        |  http = NeHttp(
        |    port = 80
        |  ),
        |  level = (NeDebug()): NeLevel
        |)""".stripMargin
  }
}
