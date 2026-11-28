package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Shrink}

// `java.nio.file.AccessMode`'s constants are never referred to by the code of this test module: on Scala 3 such a Java
// enum read from a class file has no children for Hearth until they are completed
// (docs/research/hearth-bug-java-enum-unreferenced-constants.md). Do not write `AccessMode.READ` & co. in this module's
// tests; values are obtained with `valueOf`.
class UnreferencedJavaEnumJvmSpec extends munit.FunSuite {

  private def mode(name: String): java.nio.file.AccessMode = java.nio.file.AccessMode.valueOf(name)

  test("Arbitrary, Cogen and Shrink of a JDK enum none of whose constants is referred to") {
    val arb: Arbitrary[java.nio.file.AccessMode] = DeriveArbitrary.derived[java.nio.file.AccessMode]
    val samples = List.fill(50)(arb.arbitrary.sample).flatten
    assertEquals(samples.map(_.name).toSet, Set("READ", "WRITE", "EXECUTE"))
    val cogen: Cogen[java.nio.file.AccessMode] = DeriveCogen.derived[java.nio.file.AccessMode]
    val seed = org.scalacheck.rng.Seed(12345L)
    assert(cogen.perturb(seed, mode("READ")) != cogen.perturb(seed, mode("WRITE")))
    val shrink: Shrink[java.nio.file.AccessMode] = DeriveShrink.derived[java.nio.file.AccessMode]
    assert(shrink.shrink(mode("READ")).isEmpty)
  }
}
