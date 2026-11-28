package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.Json

final case class CirceUnrefEnumHolder(mode: java.nio.file.AccessMode)

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumJvmSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is derivable (field and root)") {
    implicit val config: Configuration = Configuration(enumAsStrings = true)
    val json = Json.obj("mode" -> Json.fromString("WRITE"))
    val decoded = KindlingsDecoder.decode[CirceUnrefEnumHolder](json)
    decoded.map(_.mode.name) ==> Right("WRITE")
    KindlingsEncoder.encode(decoded.toOption.get) ==> json
    KindlingsEncoder.encode[java.nio.file.AccessMode](java.nio.file.AccessMode.valueOf("READ")) ==> Json.fromString(
      "READ"
    )
    KindlingsDecoder.decode[java.nio.file.AccessMode](Json.fromString("EXECUTE")).map(_.name) ==> Right("EXECUTE")
  }
}
