package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json}

// Deriving `NeRoot` reads the children of the sealed `NeLevel`; on Scala 2 that makes the compiler complete the other
// members of this package, and type `NeOther.http` by expanding its macro INSIDE the first expansion. With an inferred
// type (`val http = KindlingsEncoder.derived[NeHttp]`) Hearth 0.4.5 fails that nested, independent expansion with
// "MIO timeout is already set" - see docs/research/hearth-bug-mio-timeout-reentry.md. The explicit type below is the
// workaround; drop it when Hearth is fixed (that test then covers the bug).
final case class NeHttp(port: Int)
final case class NeRoot(http: NeHttp, level: NeLevel)
sealed trait NeLevel
case object NeDebug extends NeLevel
object NeRoot {
  implicit val encoder: Encoder[NeRoot] = KindlingsEncoder.derived[NeRoot]
}
object NeOther {
  val http: Encoder[NeHttp] = KindlingsEncoder.derived[NeHttp]
}

final class NestedExpansionSpec extends MacroSuite {

  test("a derivation reading sealed children next to another derivation in the same file") {
    NeOther.http(NeHttp(80)) ==> Json.obj("port" -> Json.fromInt(80))
    NeRoot.encoder(NeRoot(NeHttp(80), NeDebug)) ==>
      Json.obj("http" -> Json.obj("port" -> Json.fromInt(80)), "level" -> Json.obj("NeDebug" -> Json.obj()))
  }
}
