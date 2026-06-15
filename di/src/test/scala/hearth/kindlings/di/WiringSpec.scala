package hearth.kindlings.di

import hearth.MacroSuite

final class WiringSpec extends MacroSuite {

  group("DI.wire") {

    test("wires a class from sibling members of the enclosing class") {
      val module = new WiringSpec.UserModule
      module.userFinder.databaseAccess ==> module.databaseAccess
      module.userFinder.securityFilter ==> module.securityFilter
    }

    test("wires a class from members of an enclosing object") {
      WiringSpec.ObjectModule.userFinder.databaseAccess ==> WiringSpec.ObjectModule.databaseAccess
      WiringSpec.ObjectModule.userFinder.securityFilter ==> WiringSpec.ObjectModule.securityFilter
    }

    test("bare `wire` (macwire-style import) works the same as DI.wire") {
      val module = new WiringSpec.BareModule
      module.userFinder.databaseAccess ==> module.databaseAccess
      module.userFinder.securityFilter ==> module.securityFilter
    }

    test("resolves implicit parameters via implicit search, not from scope") {
      val module = new WiringSpec.ImplicitModule
      module.service.databaseAccess ==> module.databaseAccess
      module.service.clock ==> WiringSpec.theClock
    }

    test("falls back to a single companion apply when the primary constructor is inaccessible") {
      val module = new WiringSpec.CompanionModule
      module.widget.databaseAccess ==> module.databaseAccess
    }
  }

  group("DI.wire composition & scoping") {

    test("wires from members inherited from a mixed-in module trait") {
      val module = new WiringSpec.ComposedModule
      module.userFinder.databaseAccess ==> module.databaseAccess
      module.userFinder.securityFilter ==> module.securityFilter
    }

    test("prefers a candidate from the nearest (innermost) scope") {
      val inner = new WiringSpec.NestedScopes.Inner
      // databaseAccess is found in BOTH the inner class and the outer object; the inner one must win.
      inner.userFinder.databaseAccess ==> inner.databaseAccess
      assert(inner.userFinder.databaseAccess ne WiringSpec.NestedScopes.databaseAccess)
      // securityFilter is only in the outer object scope, so it is pulled from there.
      inner.userFinder.securityFilter ==> WiringSpec.NestedScopes.securityFilter
    }

    test("pulls candidates from a @Module-annotated dependency") {
      val module = new WiringSpec.ModuleConsumer(new WiringSpec.DataModule)
      module.userFinder.databaseAccess ==> module.dataModule.databaseAccess
      module.userFinder.securityFilter ==> module.dataModule.securityFilter
    }
  }

  group("DI.wireRec") {

    test("recursively constructs dependencies not present in scope") {
      val module = new WiringSpec.RecursiveModule
      module.app.service.databaseAccess ==> module.databaseAccess
      module.app.handler.databaseAccess ==> module.databaseAccess
    }
  }

  group("DI.wireWith") {

    test("wires a factory method's parameters from the enclosing scope") {
      val module = new WiringSpec.FactoryMethodModule
      module.userFinder.databaseAccess ==> module.databaseAccess
      module.userFinder.securityFilter ==> module.securityFilter
    }

    test("wires a factory lambda's parameters from the enclosing scope") {
      val module = new WiringSpec.FactoryLambdaModule
      module.userFinder.databaseAccess ==> module.databaseAccess
      module.userFinder.securityFilter ==> module.securityFilter
    }

    test("supports a zero-argument factory") {
      val module = new WiringSpec.NullaryFactoryModule
      module.config ==> WiringSpec.sharedConfig
    }
  }

  group("DI.wire with tagging") {

    test("disambiguates two values of the same base type by their tags") {
      val module = new WiringSpec.TaggedModule
      module.connector.primary ==> module.primaryDb
      module.connector.replica ==> module.replicaDb
    }
  }

  group("DI.autowire") {

    test("builds an object graph from provided instances") {
      val db = new WiringSpec.DatabaseAccess()
      val sf = new WiringSpec.SecurityFilter()
      val finder = DI.autowire[WiringSpec.UserFinder](db, sf)
      finder.databaseAccess ==> db
      finder.securityFilter ==> sf
    }

    test("uses a provided factory function to build a node") {
      val db = new WiringSpec.DatabaseAccess()
      val sf = new WiringSpec.SecurityFilter()
      val mk = (d: WiringSpec.DatabaseAccess, s: WiringSpec.SecurityFilter) => new WiringSpec.UserFinder(d, s)
      val finder = DI.autowire[WiringSpec.UserFinder](db, sf, mk)
      finder.databaseAccess ==> db
      finder.securityFilter ==> sf
    }

    test("constructs intermediate dependencies not provided explicitly") {
      val db = new WiringSpec.DatabaseAccess()
      // RecApp(service, handler) — neither provided; both built from the single provided db.
      val app = DI.autowire[WiringSpec.RecApp](db)
      app.service.databaseAccess ==> db
      app.handler.databaseAccess ==> db
    }

    test("shares a single instance of each type across the graph") {
      val db = new WiringSpec.DatabaseAccess()
      val app = DI.autowire[WiringSpec.RecApp](db)
      // both RecService and RecHandler must receive the very same db instance
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }
  }

  group("DI.wireSet / DI.wireList") {

    test("wireSet collects all values of a type from the enclosing scope") {
      val module = new WiringSpec.PluginModule
      module.plugins ==> Set(module.a, module.b, module.c)
    }

    test("wireList collects all values of a type preserving declaration order") {
      val module = new WiringSpec.PluginModule
      module.pluginList ==> List(module.a, module.b, module.c)
    }
  }

  group("DI.wire @Module priority & hygiene") {

    test("module members do not leak Object methods (hashCode/toString) as candidates") {
      // If membersAsValues did not filter Object's universal members, a String parameter would resolve to
      // `toString` and an Int parameter to `hashCode`; here they must come from the local `someString`/`someInt`.
      val module = new WiringSpec.ObjectMethodHygiene.Main(new WiringSpec.ObjectMethodHygiene.Base)
      module.stuff.a ==> module.base.theA
      module.stuff.i ==> module.someInt
      module.stuff.str ==> module.someString
    }

    test("a direct local member beats a @Module member of the same type") {
      val module = new WiringSpec.ModulePriority.Consumer(new WiringSpec.ModulePriority.DepModule)
      // `databaseAccess` exists both locally and inside the @Module dependency; the local one wins.
      module.userFinder.databaseAccess ==> module.localDb
      assert(module.userFinder.databaseAccess ne module.depModule.databaseAccess)
    }

    test("@Module annotation is honoured when inherited from a trait parent") {
      val module = new WiringSpec.InheritedModule.Consumer(new WiringSpec.InheritedModule.DepModule)
      module.userFinder.databaseAccess ==> module.depModule.databaseAccess
      module.userFinder.securityFilter ==> module.depModule.securityFilter
    }
  }

  group("DI.wire inheritance & visibility") {

    test("a private parent member is excluded; the public one is used") {
      WiringSpec.VisibilityModule.consumer.box.db ==> WiringSpec.VisibilityModule.publicDb
    }

    test("a protected parent member is usable as a candidate") {
      WiringSpec.ProtectedModule.consumer.box.db ==> WiringSpec.ProtectedModule.protectedDbValue
    }

    test("diamond inheritance shares a single instance of the common dependency") {
      WiringSpec.Diamond.service.dependency ==> WiringSpec.Diamond.dependency
      WiringSpec.Diamond.anotherService.dependency ==> WiringSpec.Diamond.dependency
      assert(WiringSpec.Diamond.service.dependency eq WiringSpec.Diamond.anotherService.dependency)
    }

    // Self-type providers (`this: AProvider =>`) are a PLATFORM DIFFERENCE: supported on Scala 3, not on Scala 2.
    // Hearth's `enclosingScope` exposes self-type members of an enclosing trait on Scala 3 but not on Scala 2 (its
    // member list comes from the class symbol's own type, which on Scala 2 omits self-type requirements). The positive
    // test lives in di/src/test/scala-3 and the negative (documented-limitation) test in di/src/test/scala-2. See
    // docs/research/di-self-type-limitation.md.
  }

  group("DI.wire candidate-source eligibility") {

    test("a parameterless def is usable as a candidate and is re-invoked on each wire") {
      val module = new WiringSpec.DefCandidateModule
      // each `wire[DbHolder]` calls the `a` def afresh, so the two holders get different DatabaseAccess values
      assert(module.b1.db ne module.b2.db)
      module.countA ==> 2
    }

    test("a def WITH parameters is ineligible as a candidate (the parameterless val is used)") {
      val module = new WiringSpec.DefWithParamsModule
      module.b.db ==> module.a
    }

    test("a generic class is disambiguated by its type argument") {
      val module = new WiringSpec.GenericModule
      module.boxConsumerInt.box ==> module.intBox
      module.boxConsumerString.box ==> module.stringBox
    }

    test("standard-library collection dependencies are not over-matched") {
      val module = new WiringSpec.CollectionDepModule
      module.consumer.strings ==> module.strings
      module.consumer.ints ==> module.ints
    }
  }

  // By-name (`=> A`) constructor parameters are a PLATFORM DIFFERENCE: wired on Scala 2, pending on Scala 3.
  // The by-name wrapper type is decomposable from the typed API on Scala 2 (`<byname>[A]` exposes `A` via type
  // arguments) but not on Scala 3 (the `ByNameType` is not surfaced as an applied type). Positive test in
  // di/src/test/scala-2, documented-limitation test in di/src/test/scala-3. See docs/research/di-by-name-limitation.md.

  group("DI.wireRec breadth") {

    test("rebuilds a shared intermediate (value-equal) across multiple recursive stages") {
      val module = new WiringSpec.MultiStageModule
      module.e.c.a ==> module.a
      // wireRec does not memoize (unlike autowire), so the intermediate C is structurally equal across stages.
      module.e.c ==> module.e.d.c
    }
  }

  group("DI.wireRec compile-time errors") {

    test("refuses to recursively construct a String/primitive leaf") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class Needy(val s: String)
        class RecModule {
          lazy val needy: Needy = DI.wireRec[Needy]
        }
        """
      ).check(
        "Cannot find a value of type: [",
        "String"
      )
    }
  }

  group("DI.wireWith breadth") {

    test("resolves an implicit parameter of the factory method from the enclosing scope") {
      val module = new WiringSpec.WireWithImplicitsModule
      module.a.n ==> 0
      module.a.s ==> "the-implicit"
    }
  }

  group("DI.wire tagging breadth") {

    test("supports a tagged primitive dependency") {
      val module = new WiringSpec.TaggedPrimitiveModule
      module.wallet.balance ==> module.usdAmount
    }

    test("supports multiple tags applied with andTaggedWith") {
      val module = new WiringSpec.MultiTagModule
      module.consumer.both ==> module.tagged
    }
  }

  group("DI.wire compile-time errors") {

    test("reports a missing value by type") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class DatabaseAccess()
        class SecurityFilter()
        class UserFinder(val databaseAccess: DatabaseAccess, val securityFilter: SecurityFilter)
        class Broken {
          lazy val databaseAccess = new DatabaseAccess()
          lazy val userFinder: UserFinder = DI.wire[UserFinder]
        }
        """
      ).check(
        "Cannot find a value of type: [",
        "SecurityFilter"
      )
    }

    test("reports an ambiguity with the conflicting member NAMES (not just the type)") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A()
        class B(val a: A)
        class Holder {
          lazy val theA1: A = new A()
          lazy val theA2: A = new A()
          lazy val b: B = DI.wire[B]
        }
        """
      ).check(
        "Found multiple values of type [",
        "A",
        "theA1",
        "theA2"
      )
    }

    test("reports a class with no public constructor and no usable companion apply") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        trait A
        class Holder {
          val a: A = new A {}
          class Target private (val a: A)
          lazy val t: Target = DI.wire[Target]
        }
        """
      ).check(
        "No public primary constructor found for",
        "Target"
      )
    }

    test("reports a companion object whose apply does not construct the target type (fake apply)") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        trait A
        class Holder {
          val a: A = new A {}
          class Target private (val a: A)
          object Target { def apply(a: A): String = "not a Target" }
          lazy val t: Target = DI.wire[Target]
        }
        """
      ).check(
        "Target",
        "has no apply methods constructing target type"
      )
    }

    test("reports a companion object with no apply methods at all") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        trait A
        class Holder {
          val a: A = new A {}
          class Target private (val a: A)
          object Target
          lazy val t: Target = DI.wire[Target]
        }
        """
      ).check(
        "Target",
        "has no apply methods constructing target type"
      )
    }

    test("reports an ambiguous companion apply (multiple matching applys, private primary ctor)") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class Holder {
          val i: Int = 1
          class Target private (val i: Int)
          object Target {
            def apply(i: Int): Target = new Target(i)
            def apply(i: Int, j: Int): Target = new Target(i + j)
          }
          lazy val t: Target = DI.wire[Target]
        }
        """
      ).check(
        "and multiple matching apply methods in its companion object were found",
        "Target"
      )
    }
  }

  group("DI.autowire compile-time errors") {

    test("detects a cyclic dependency and shows the wiring path") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A(val b: B)
        class B(val a: A)
        DI.autowire[A]()
        """
      ).check(
        "cyclic dependencies detected",
        "wiring path: ",
        "A",
        " -> ",
        "B",
        " -> ",
        "A"
      )
    }

    test("refuses a primitive/String leaf and shows the wiring path") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A(val s: String)
        DI.autowire[A]()
        """
      ).check(
        "cannot use a primitive type or String in autowiring",
        "wiring path: ",
        "A -> ",
        "String"
      )
    }

    test("reports a plain trait that cannot be constructed, with the wiring path") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        trait A
        class B(val a: A)
        DI.autowire[B]()
        """
      ).check(
        "cannot find a provided dependency, public constructor or public apply method for: ",
        "A",
        "wiring path: ",
        "B",
        " -> ",
        "A"
      )
    }

    test("reports a private constructor that cannot be wired, with the wiring path") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A private ()
        DI.autowire[A]()
        """
      ).check(
        "cannot find a provided dependency, public constructor or public apply method for: ",
        "A",
        "wiring path: ",
        "A"
      )
    }

    test("reports an unused provided dependency") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A()
        class B(val a: A)
        class C()
        DI.autowire[B](new A(), new C())
        """
      ).check(
        "unused dependencies: "
      )
    }

    test("reports a duplicate provided dependency type") {
      compileErrors(
        """
        import hearth.kindlings.di.DI
        class A()
        class B(val a: A)
        val a = new A()
        DI.autowire[B](a, a)
        """
      ).check(
        "duplicate type in dependencies list: ",
        "A"
      )
    }
  }
}

object WiringSpec {

  import com.softwaremill.tagging.@@

  class DatabaseAccess()
  class SecurityFilter()
  class UserFinder(val databaseAccess: DatabaseAccess, val securityFilter: SecurityFilter)

  class UserModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val userFinder: UserFinder = DI.wire[UserFinder]
  }

  object ObjectModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val userFinder: UserFinder = DI.wire[UserFinder]
  }

  class BareModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val userFinder: UserFinder = wire[UserFinder]
  }

  // Implicit-parameter resolution.
  final class Clock
  val theClock: Clock = new Clock
  class Service(val databaseAccess: DatabaseAccess)(implicit val clock: Clock)

  class ImplicitModule {
    implicit val clock: Clock = theClock
    lazy val databaseAccess = new DatabaseAccess()
    lazy val service: Service = DI.wire[Service]
  }

  // Companion-apply fallback (private primary constructor).
  class Widget private (val databaseAccess: DatabaseAccess)
  object Widget {
    def apply(databaseAccess: DatabaseAccess): Widget = new Widget(databaseAccess)
  }

  class CompanionModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val widget: Widget = DI.wire[Widget]
  }

  // Composition by trait inheritance.
  trait DatabaseModuleTrait {
    lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
    lazy val securityFilter: SecurityFilter = new SecurityFilter()
  }
  class ComposedModule extends DatabaseModuleTrait {
    lazy val userFinder: UserFinder = DI.wire[UserFinder]
  }

  // Scope precedence — the inner class's databaseAccess must win over the outer object's.
  object NestedScopes {
    val databaseAccess: DatabaseAccess = new DatabaseAccess()
    val securityFilter: SecurityFilter = new SecurityFilter()
    class Inner {
      val databaseAccess: DatabaseAccess = new DatabaseAccess()
      lazy val userFinder: UserFinder = DI.wire[UserFinder]
    }
  }

  // @Module expansion — pull a module dependency's own members as candidates.
  @Module
  class DataModule {
    lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
    lazy val securityFilter: SecurityFilter = new SecurityFilter()
  }
  class ModuleConsumer(val dataModule: DataModule) {
    lazy val userFinder: UserFinder = DI.wire[UserFinder]
  }

  // Recursive wiring — only `databaseAccess` is in scope; Service, Handler and App are built recursively.
  class RecService(val databaseAccess: DatabaseAccess)
  class RecHandler(val databaseAccess: DatabaseAccess)
  class RecApp(val service: RecService, val handler: RecHandler)
  class RecursiveModule {
    lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
    lazy val app: RecApp = DI.wireRec[RecApp]
  }

  // wireWith factory wiring.
  def makeUserFinder(databaseAccess: DatabaseAccess, securityFilter: SecurityFilter): UserFinder =
    new UserFinder(databaseAccess, securityFilter)

  class FactoryMethodModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val userFinder: UserFinder = DI.wireWith(WiringSpec.makeUserFinder _)
  }

  class FactoryLambdaModule {
    lazy val databaseAccess = new DatabaseAccess()
    lazy val securityFilter = new SecurityFilter()
    lazy val userFinder: UserFinder =
      DI.wireWith((db: DatabaseAccess, sf: SecurityFilter) => new UserFinder(db, sf))
  }

  class Config(val name: String)
  val sharedConfig: Config = new Config("shared")
  class NullaryFactoryModule {
    lazy val config: Config = DI.wireWith(() => WiringSpec.sharedConfig)
  }

  // Tagging — disambiguate same-typed dependencies by tag (macwire-compatible `com.softwaremill.tagging`).
  trait Primary
  trait Replica
  class Database(val url: String)
  class Connector(val primary: Database @@ Primary, val replica: Database @@ Replica)

  class TaggedModule {
    import com.softwaremill.tagging.*
    val primaryDb: Database @@ Primary = new Database("primary").taggedWith[Primary]
    val replicaDb: Database @@ Replica = new Database("replica").taggedWith[Replica]
    lazy val connector: Connector = DI.wire[Connector]
  }

  // Set/List wiring.
  class Plugin
  class PluginModule {
    val a: Plugin = new Plugin
    val b: Plugin = new Plugin
    val c: Plugin = new Plugin
    lazy val plugins: Set[Plugin] = DI.wireSet[Plugin]
    lazy val pluginList: List[Plugin] = DI.wireList[Plugin]
  }

  // ---- P1: @Module priority & hygiene ----

  // Object-method hygiene: when a @Module dependency is expanded, hashCode/toString must NOT leak as Int/String
  // candidates. The Int/String parameters must come from the local `someInt`/`someString`.
  object ObjectMethodHygiene {
    @Module
    class Base {
      lazy val theA: DatabaseAccess = new DatabaseAccess()
    }
    class Stuff(val a: DatabaseAccess, val i: Int, val str: String)
    class Main(val base: Base) {
      lazy val someInt: Int = 42
      lazy val someString: String = "toto"
      lazy val stuff: Stuff = DI.wire[Stuff]
    }
  }

  // Direct local member must beat a @Module member of the same type.
  object ModulePriority {
    @Module
    class DepModule {
      lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
    }
    class Consumer(val depModule: DepModule) {
      lazy val localDb: DatabaseAccess = new DatabaseAccess()
      lazy val userFinder: UserFinder = DI.wire[UserFinder]
      lazy val securityFilter: SecurityFilter = new SecurityFilter()
    }
  }

  // @Module inherited from a trait parent.
  object InheritedModule {
    @Module
    trait DepModuleTrait {
      lazy val databaseAccess: DatabaseAccess = new DatabaseAccess()
      lazy val securityFilter: SecurityFilter = new SecurityFilter()
    }
    class DepModule extends DepModuleTrait
    class Consumer(val depModule: DepModule) {
      lazy val userFinder: UserFinder = DI.wire[UserFinder]
    }
  }

  // ---- P2: inheritance / visibility / self-type ----

  // Private parent member excluded; public one used.
  object VisibilityModule {
    val publicDb: DatabaseAccess = new DatabaseAccess()
    trait Base {
      @scala.annotation.nowarn("msg=unused")
      private lazy val privateDb: DatabaseAccess = new DatabaseAccess()
      lazy val theDb: DatabaseAccess = publicDb
    }
    // `privateDb` is invisible at the call site, so `wire[DbHolder]` must pick `theDb` (== publicDb), not be ambiguous.
    val consumer: ConsumerHolder = new ConsumerHolder
    class ConsumerHolder extends Base {
      lazy val box: DbHolder = DI.wire[DbHolder]
    }
  }

  class DbHolder(val db: DatabaseAccess)

  // Protected parent member usable.
  object ProtectedModule {
    val protectedDbValue: DatabaseAccess = new DatabaseAccess()
    trait Base {
      protected lazy val protectedDb: DatabaseAccess = protectedDbValue
    }
    val consumer: ConsumerHolder = new ConsumerHolder
    class ConsumerHolder extends Base {
      lazy val box: DbHolder = DI.wire[DbHolder]
    }
  }

  // Diamond inheritance shares a single common dependency.
  object Diamond {
    class Dependency
    class Service(val dependency: Dependency)
    class AnotherService(val dependency: Dependency)
    trait A {
      val dependency: Dependency = DI.wire[Dependency]
    }
    trait B extends A {
      val service: Service = DI.wire[Service]
    }
    object Impl extends A with B {
      val anotherService: AnotherService = DI.wire[AnotherService]
    }
    def dependency: Dependency = Impl.dependency
    def service: Service = Impl.service
    def anotherService: AnotherService = Impl.anotherService
  }

  // ---- P3: candidate-source eligibility ----

  // A parameterless def is usable AND re-invoked on each wire.
  class DefCandidateModule {
    var countA: Int = 0
    def a: DatabaseAccess = { countA += 1; new DatabaseAccess() }
    lazy val b1: DbHolder = DI.wire[DbHolder]
    lazy val b2: DbHolder = DI.wire[DbHolder]
  }

  // A def WITH parameters is ineligible; the parameterless val is used.
  class DefWithParamsModule {
    lazy val a: DatabaseAccess = new DatabaseAccess()
    def buildSome(i: Int): DatabaseAccess = { val _ = i; new DatabaseAccess() }
    lazy val b: DbHolder = DI.wire[DbHolder]
  }

  // Generic-class disambiguation by type argument.
  class Box[X](val value: X)
  class BoxConsumer[X](val box: Box[X])
  class GenericModule {
    val intBox: Box[Int] = new Box(1)
    val stringBox: Box[String] = new Box("s")
    lazy val boxConsumerInt: BoxConsumer[Int] = DI.wire[BoxConsumer[Int]]
    lazy val boxConsumerString: BoxConsumer[String] = DI.wire[BoxConsumer[String]]
  }

  // Std-lib collection deps not over-matched.
  class CollectionConsumer(val strings: List[String], val ints: List[Int])
  class CollectionDepModule {
    val strings: List[String] = List("a", "b")
    val ints: List[Int] = List(1, 2)
    lazy val consumer: CollectionConsumer = DI.wire[CollectionConsumer]
  }

  // ---- P4: wireRec multi-stage / wireWith implicits / tagging ----

  // macwire's wireRecMultiStage uses case classes and structural (==) equality: wireRec rebuilds each dependency
  // independently (it does NOT memoize like autowire), so the shared intermediate is value-equal, not reference-equal.
  case class StageA()
  case class StageB()
  case class StageC(a: StageA, b: StageB)
  case class StageD(a: StageA, c: StageC)
  case class StageE(c: StageC, d: StageD)
  class MultiStageModule {
    lazy val a: StageA = new StageA()
    lazy val e: StageE = DI.wireRec[StageE]
  }

  class ImplicitFactoryResult(val n: Int, val s: String)
  object ImplicitFactoryResult {
    def make(n: Int)(implicit s: String): ImplicitFactoryResult = new ImplicitFactoryResult(n, s)
  }
  class WireWithImplicitsModule {
    lazy val n: Int = 0
    implicit lazy val s: String = "the-implicit"
    lazy val a: ImplicitFactoryResult = DI.wireWith(ImplicitFactoryResult.make _)
  }

  // Tagged primitive.
  trait Usd
  class Wallet(val balance: Int @@ Usd)
  class TaggedPrimitiveModule {
    import com.softwaremill.tagging.*
    val usdAmount: Int @@ Usd = 100.taggedWith[Usd]
    lazy val wallet: Wallet = DI.wire[Wallet]
  }

  // Multiple tags via andTaggedWith.
  trait T1
  trait T2
  class MultiTagConsumer(val both: Database @@ T1 with T2)
  class MultiTagModule {
    import com.softwaremill.tagging.*
    val tagged: Database @@ T1 with T2 = new Database("u").taggedWith[T1].andTaggedWith[T2]
    lazy val consumer: MultiTagConsumer = DI.wire[MultiTagConsumer]
  }
}
