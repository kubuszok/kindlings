package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.Node.ScalarNode

final case class YamlUnrefEnumHolder(mode: java.nio.file.AccessMode)

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumJvmSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is derivable (field and root)") {
    val holder = YamlUnrefEnumHolder(java.nio.file.AccessMode.valueOf("WRITE"))
    KindlingsYamlDecoder.decode[YamlUnrefEnumHolder](KindlingsYamlEncoder.encode(holder)) ==> Right(holder)
    KindlingsYamlEncoder.encode[java.nio.file.AccessMode](java.nio.file.AccessMode.valueOf("READ")) ==> ScalarNode(
      "READ"
    )
    KindlingsYamlDecoder.decode[java.nio.file.AccessMode](ScalarNode("EXECUTE")).map(_.name) ==> Right("EXECUTE")
  }
}
