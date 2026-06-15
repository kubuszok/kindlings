package hearth.kindlings.di

import hearth.MacroSuite

/** Self-type providers (`this: AProvider =>`) ARE visible to wiring on Scala 3 — Hearth's `enclosingScope` exposes the
  * self-type members of an enclosing trait there. (On Scala 2 this is not available; see the matching negative test in
  * `di/src/test/scala-2` and `docs/research/di-self-type-limitation.md`.)
  */
final class SelfTypeSpec extends MacroSuite {

  group("DI.wire self-type providers (Scala 3)") {

    test("a self-type provider's members are usable as wiring candidates") {
      val instance = new SelfTypeSpec.Module with SelfTypeSpec.AProviderImpl {}
      instance.holder.dep ==> instance.dep
    }
  }

  group("DI.wire by-name parameters (Scala 3 limitation)") {

    // By-name (`=> A`) parameters are wired on Scala 2 but not yet on Scala 3: the `ByNameType` wrapper is not surfaced
    // as a decomposable applied type through Hearth's typed API on Scala 3, so the underlying `A` cannot be recovered
    // to match a strict candidate. See docs/research/di-by-name-limitation.md.
    test("a by-name (=> A) parameter is NOT yet wired (documented limitation)") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class ByNameDep()
        class ByNameHolder(depProvider: => ByNameDep) { def dep: ByNameDep = depProvider }
        class ByNameModule {
          val dep: ByNameDep = new ByNameDep()
          lazy val holder: ByNameHolder = DI.wire[ByNameHolder]
        }
        """
      ).check(
        "Cannot find a value of type: [",
        "ByNameDep"
      )
    }
  }
}

object SelfTypeSpec {
  class Dep()
  class DepHolder(val dep: Dep)

  trait AProvider { def dep: Dep }

  trait Module { this: AProvider =>
    lazy val holder: DepHolder = DI.wire[DepHolder]
  }

  trait AProviderImpl extends AProvider {
    lazy val dep: Dep = new Dep()
  }
}
