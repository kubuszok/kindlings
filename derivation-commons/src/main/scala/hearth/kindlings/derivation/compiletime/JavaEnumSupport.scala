package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Hearth's `Enum.parse` made reliable for Java enums.
  *
  * On Scala 3 a Java enum read from a class file (any JDK enum, or one from a dependency) has no children as long as
  * the infos of its constants are not completed, which happens only when some code in the same compilation refers to a
  * constant. So `Enum.parse[java.nio.file.AccessMode]` fails when the code being compiled never mentions
  * `AccessMode.READ` & co. - tests (which build sample values) do not see it, users (`case class C(mode: AccessMode)`)
  * do. [[parseEnum]] completes the constants first. See docs/research/hearth-bug-java-enum-unreferenced-constants.md.
  *
  * Every derivation module calls [[parseEnum]] instead of `Enum.parse` (Hearth caches the result of `Enum.parse` per
  * type, so a single call made before the completion would make the enum unusable for the rest of the expansion).
  */
trait JavaEnumSupport { this: MacroCommons =>

  /** `Enum.parse[A]`, after completing the constants of `A` if it is a Java enum (Scala 3). */
  protected def parseEnum[A: Type]: ClassViewResult[Enum[A]] = {
    completeJavaEnumConstants[A]()
    Enum.parse[A]
  }

  /** Completes the constants of `A` if it is a Java enum (Scala 3; a no-op otherwise). */
  protected def completeJavaEnumConstants[A: Type](): Unit = {
    val _ = JavaEnumConstants(this)(Type[A].asUntyped)
  }
}
