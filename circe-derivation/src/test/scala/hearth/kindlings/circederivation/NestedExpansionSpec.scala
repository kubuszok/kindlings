package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.{Encoder, Json}

// Deriving `NeRoot` reads the children of the sealed `NeLevel`; on Scala 2 that makes the compiler complete the other
// members of this package, and type `NeUntyped.http` (whose type is inferred) by expanding its macro INSIDE the first
// expansion. Hearth 0.4.5 failed that nested, independent expansion with "MIO timeout is already set" (fixed in
// Hearth 985e5e6).
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
object NeUntyped {
  val http = KindlingsEncoder.derived[NeHttp]
}

final class NestedExpansionSpec extends MacroSuite {

  test("a derivation reading sealed children next to another derivation in the same file") {
    NeOther.http(NeHttp(80)) ==> Json.obj("port" -> Json.fromInt(80))
    NeRoot.encoder(NeRoot(NeHttp(80), NeDebug)) ==>
      Json.obj("http" -> Json.obj("port" -> Json.fromInt(80)), "level" -> Json.obj("NeDebug" -> Json.obj()))
  }

  test("a derivation reading sealed children next to an untyped derived val in the same file") {
    NeUntyped.http(NeHttp(80)) ==> Json.obj("port" -> Json.fromInt(80))
  }
}
