package hearth.kindlings.pureconfigderivation

import hearth.MacroSuite
import pureconfig.ConfigCursor

final case class PcUnrefEnumHolder(mode: java.nio.file.AccessMode)

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is derivable") {
    val reader = KindlingsConfigReader.derived[PcUnrefEnumHolder]
    val holder = PcUnrefEnumHolder(java.nio.file.AccessMode.valueOf("WRITE"))
    val written = KindlingsConfigWriter.derived[PcUnrefEnumHolder].to(holder)
    reader.from(ConfigCursor(written, Nil)) ==> Right(holder)
  }
}
