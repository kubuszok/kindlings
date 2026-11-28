package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

// Deriving `NeRoot` reads the children of the sealed `NeLevel`; on Scala 2 that makes the compiler type
// `NeUntyped.codec` (whose type is inferred) by expanding its macro INSIDE the first expansion. Hearth 0.4.5 failed that
// nested, independent expansion with "MIO timeout is already set" (fixed in Hearth
// 985e5e6).
final case class NeHttp(port: Int)
final case class NeRoot(http: NeHttp, level: NeLevel)
sealed trait NeLevel
case object NeDebug extends NeLevel
object NeRoot {
  implicit val codec: KindlingsJsonValueCodec[NeRoot] = KindlingsJsonValueCodec.derived[NeRoot]
}
object NeUntyped {
  val codec = KindlingsJsonValueCodec.derived[NeHttp]
}

final class NestedExpansionSpec extends MacroSuite {

  test("a derivation reading sealed children next to an untyped derived val in the same file") {
    writeToString(NeHttp(80))(NeUntyped.codec) ==> """{"port":80}"""
    val root = NeRoot(NeHttp(80), NeDebug)
    readFromString[NeRoot](writeToString(root)(NeRoot.codec))(NeRoot.codec) ==> root
  }
}
