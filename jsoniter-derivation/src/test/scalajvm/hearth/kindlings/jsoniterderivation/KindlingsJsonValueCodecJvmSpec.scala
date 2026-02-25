package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

final class KindlingsJsonValueCodecJvmSpec extends MacroSuite {

  group("KindlingsJsonValueCodec (JVM-only)") {

    group("Java enum (enumAsStrings)") {

      test("Java enum round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[JavaColor]
        val json = writeToString[JavaColor](JavaColor.RED)(codec)
        json ==> "\"RED\""
        readFromString[JavaColor](json)(codec) ==> JavaColor.RED
      }

      test("all Java enum values round-trip") {
        implicit val config: JsoniterConfig = JsoniterConfig(enumAsStrings = true)
        val codec = KindlingsJsonValueCodec.derive[JavaColor]
        Seq(JavaColor.RED, JavaColor.GREEN, JavaColor.BLUE).foreach { v =>
          readFromString[JavaColor](writeToString[JavaColor](v)(codec))(codec) ==> v
        }
      }
    }
  }
}
