package hearth.kindlings.catsderivation

import hearth.MacroSuite
import hearth.kindlings.catsderivation.extensions.*

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
final class UnreferencedJavaEnumJvmSpec extends MacroSuite {

  private def mode(name: String): java.nio.file.AccessMode = java.nio.file.AccessMode.valueOf(name)

  test("Show, Eq, Order and Hash of a JDK enum none of whose constants is referred to") {
    cats.Show
      .derived[java.nio.file.AccessMode]
      .show(mode("WRITE")) ==> "WRITE()" // singletons are shown as `Name()`, like case objects
    cats.kernel.Eq.derived[java.nio.file.AccessMode].eqv(mode("WRITE"), mode("WRITE")) ==> true
    cats.kernel.Eq.derived[java.nio.file.AccessMode].eqv(mode("WRITE"), mode("READ")) ==> false
    val order = cats.kernel.Order.derived[java.nio.file.AccessMode]
    assert(order.compare(mode("READ"), mode("EXECUTE")) < 0)
    val hash = cats.kernel.Hash.derived[java.nio.file.AccessMode]
    hash.eqv(mode("READ"), mode("READ")) ==> true
  }

  // `TextStyle.SHORT_STANDALONE` is referred to below: on Scala 3 it used to be the first child of the enum (the
  // compiler registers the constants of a Java enum read from a class file in the order in which it completes them),
  // so the derived Order put it before `FULL`.
  test("Order of a JDK enum follows the declaration order even when some constants are referred to") {
    val order = cats.kernel.Order.derived[java.time.format.TextStyle]
    val referred = java.time.format.TextStyle.SHORT_STANDALONE
    val all = java.time.format.TextStyle.values().toList
    all.sortWith((a, b) => order.compare(a, b) < 0) ==> all
    assert(order.compare(java.time.format.TextStyle.valueOf("FULL"), referred) < 0)
  }
}
