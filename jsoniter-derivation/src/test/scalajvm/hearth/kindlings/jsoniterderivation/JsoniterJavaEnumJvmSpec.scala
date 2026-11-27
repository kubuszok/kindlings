package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, ReaderConfig}
import hearth.MacroSuite

final case class SpsUnreferencedEnum(mode: java.nio.file.AccessMode)

/** JVM-only: a JDK enum read from a class file. */
final class JsoniterJavaEnumJvmSpec extends MacroSuite {

  private def decodeError[A](
      codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A],
      json: String
  ): String =
    intercept[JsonReaderException](
      readFromString[A](json, ReaderConfig.withAppendHexDumpToParseException(false))(codec)
    ).getMessage

  group("Java enums") {
    // `java.nio.file.AccessMode`'s constants are not referred to anywhere in this module: on Scala 3 the enum has
    // no children until they are completed (docs/research/hearth-findings-jsoniter-parity-2026-11.md, 4).
    test("an enum none of whose constants is referred to by the compiled code is derivable") {
      val c = KindlingsJsonValueCodec.derived[SpsUnreferencedEnum]
      val read = readFromString[SpsUnreferencedEnum]("""{"mode":"WRITE"}""")(c)
      read.mode.name ==> "WRITE"
      writeToString(read)(c) ==> """{"mode":"WRITE"}"""
      decodeError(c, """{"mode":"write"}""") ==> "illegal enum value \"write\", offset: 0x0000000e"
    }

    // as jsoniter-scala (JsonCodecMakerSpec: "serialize and deserialize Java enums" - `null` values)
    test("a null value is a NullPointerException when written (not a MatchError)") {
      val c = KindlingsJsonValueCodec.derived[SpsUnreferencedEnum]
      val _ = intercept[NullPointerException](writeToString(SpsUnreferencedEnum(null))(c))
      val list = KindlingsJsonValueCodec.derived[List[java.nio.file.AccessMode]]
      intercept[NullPointerException](writeToString(List[java.nio.file.AccessMode](null))(list))
    }
  }
}
