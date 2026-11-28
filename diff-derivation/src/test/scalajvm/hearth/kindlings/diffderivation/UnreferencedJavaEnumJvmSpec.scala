package hearth.kindlings.diffderivation

import hearth.MacroSuite

final case class DiffUnrefEnumHolder(mode: java.nio.file.AccessMode)

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumJvmSpec extends MacroSuite {

  test("a JDK enum none of whose constants is referred to is derivable") {
    val d = Diff.derived[DiffUnrefEnumHolder]
    val a = DiffUnrefEnumHolder(java.nio.file.AccessMode.valueOf("WRITE"))
    val b = DiffUnrefEnumHolder(java.nio.file.AccessMode.valueOf("READ"))
    d.diff(a, a).isIdentical ==> true
    d.diff(a, b).isIdentical ==> false
  }
}
