package hearth.kindlings.di

import hearth.MacroSuite

/** Tests for `DI.plan[A]....build` — Kindlings' own opinionated wiring endpoint (always recursive, always caching, with
  * a customizable storage strategy and per-type construction overrides). Reuses the `RecApp` diamond fixtures from
  * [[WiringSpec]] (`RecApp` needs a `RecService` and a `RecHandler`, both of which need a `DatabaseAccess`).
  */
final class DIPlanSpec extends MacroSuite {

  group("DI.plan storage strategies") {

    test("default (val) storage builds the whole graph and shares each instance exactly once") {
      val app = DI.plan[WiringSpec.RecApp].build
      app.service.databaseAccess ==> app.handler.databaseAccess
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }

    test("asVals shares each instance (same as default)") {
      val app = DI.plan[WiringSpec.RecApp].asVals.build
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }

    test("asLazyVals shares each instance (created once, lazily)") {
      val app = DI.plan[WiringSpec.RecApp].asLazyVals.build
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }

    test("asDefs re-creates a dependency on each use (not shared)") {
      val app = DI.plan[WiringSpec.RecApp].asDefs.build
      // Every node is a `def`, so the two references to DatabaseAccess (one per consumer) each build a fresh instance.
      assert(app.service.databaseAccess ne app.handler.databaseAccess)
    }

    test("storeAsDef overrides storage for a single type while the rest stay vals") {
      val app = DI.plan[WiringSpec.RecApp].storeAsDef[WiringSpec.DatabaseAccess].build
      // DatabaseAccess is a `def` (re-created per use), so the two consumers get distinct instances...
      assert(app.service.databaseAccess ne app.handler.databaseAccess)
      // ...while RecService/RecHandler themselves are still vals (the default), built once inside the single RecApp.
      app.service ==> app.service
    }
  }

  group("DI.plan construction overrides") {

    test("provide supplies a factory used to construct a type instead of its constructor") {
      val myDb = new WiringSpec.DatabaseAccess()
      val app = DI.plan[WiringSpec.RecApp].provide[WiringSpec.DatabaseAccess](myDb).build
      // Both consumers receive the provided instance (stored once as a val by default).
      assert(app.service.databaseAccess eq myDb)
      assert(app.handler.databaseAccess eq myDb)
    }

    test("provide combined with a storage override still wires correctly") {
      val app = DI
        .plan[WiringSpec.RecApp]
        .asLazyVals
        .provide[WiringSpec.DatabaseAccess](new WiringSpec.DatabaseAccess())
        .build
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }
  }

  group("DI.plan debug") {

    test("debugTree compiles and produces a wired instance") {
      val app = DI.plan[WiringSpec.RecApp].debugTree.build
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }

    test("debugMermaid compiles and produces a wired instance") {
      val app = DI.plan[WiringSpec.RecApp].debugMermaid.build
      assert(app.service.databaseAccess eq app.handler.databaseAccess)
    }
  }
}
