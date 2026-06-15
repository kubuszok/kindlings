package hearth.kindlings.di

import hearth.MacroSuite

/** Self-type providers (`this: AProvider =>`) are NOT visible to wiring on Scala 2: Hearth's `enclosingScope` derives a
  * trait's members from its own type, which on Scala 2 omits the self-type requirement, so the provider's `dep` never
  * reaches the candidate pool. macwire supports this; closing the gap requires Hearth to expose self-type members on
  * Scala 2. The positive counterpart lives in `di/src/test/scala-3`. See `docs/research/di-self-type-limitation.md`.
  */
final class SelfTypeSpec extends MacroSuite {

  group("DI.wire self-type providers (Scala 2 limitation)") {

    test("a self-type provider's members are NOT yet visible (documented limitation)") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class Dep()
        class DepHolder(val dep: Dep)
        trait DepProvider { def dep: Dep }
        trait SelfTypedModule { this: DepProvider =>
          lazy val holder: DepHolder = DI.wire[DepHolder]
        }
        """
      ).check(
        "Cannot find a value of type: [",
        "Dep"
      )
    }
  }

  group("DI.wire by-name parameters (Scala 2)") {

    test("wires a by-name (=> A) constructor parameter from scope") {
      val module = new SelfTypeSpec.ByNameModule
      module.holder.dep ==> module.dep
    }
  }
}

object SelfTypeSpec {
  // By-name (=> A) constructor parameter — wired on Scala 2.
  class ByNameDep()
  class ByNameHolder(depProvider: => ByNameDep) {
    def dep: ByNameDep = depProvider
  }
  class ByNameModule {
    val dep: ByNameDep = new ByNameDep()
    lazy val holder: ByNameHolder = DI.wire[ByNameHolder]
  }
}
