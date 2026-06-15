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
    import com.softwaremill.tagging._
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
}
