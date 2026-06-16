package hearth.kindlings.di

import hearth.MacroSuite

/** Self-type providers (`this: AProvider =>`) and by-name (`=> A`) constructor parameters are both wired on Scala 2
  * AND Scala 3 as of Hearth 0.3.1-48 (self-type members are exposed on Scala 2; `Parameter.byNameUnderlying` recovers
  * the by-name underlying on Scala 3). Previously each worked on only one platform; because both now work everywhere,
  * this is a single shared spec (no platform split).
  */
final class SelfTypeSpec extends MacroSuite {

  group("DI.wire self-type providers") {

    test("a self-type provider's members are usable as wiring candidates") {
      val instance = new SelfTypeSpec.Module with SelfTypeSpec.AProviderImpl {}
      instance.holder.dep ==> instance.dep
    }
  }

  group("DI.wire by-name parameters") {

    test("wires a by-name (=> A) constructor parameter from scope") {
      val module = new SelfTypeSpec.ByNameModule
      module.holder.dep ==> module.dep
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

  // By-name (=> A) constructor parameter.
  class ByNameDep()
  class ByNameHolder(depProvider: => ByNameDep) {
    def dep: ByNameDep = depProvider
  }
  class ByNameModule {
    val dep: ByNameDep = new ByNameDep()
    lazy val holder: ByNameHolder = DI.wire[ByNameHolder]
  }
}
