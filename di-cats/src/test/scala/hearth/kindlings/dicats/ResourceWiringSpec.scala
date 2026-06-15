package hearth.kindlings.dicats

import hearth.MacroSuite
import cats.effect.{Resource, SyncIO}
import scala.collection.mutable.ListBuffer

final class ResourceWiringSpec extends MacroSuite {

  import ResourceWiringSpec.*

  group("DICats.wireResource — instances only") {

    test("all plain instances produce a Resource.pure (no acquire/release)") {
      val config = new Config("db-url")
      val logger = new Logger
      val res: Resource[SyncIO, App] = DICats.wireResource[SyncIO, App](config, logger)
      val (app, release) = res.allocated.unsafeRunSync()
      app.config ==> config
      app.logger ==> logger
      release.unsafeRunSync()
    }

    test("a zero-dependency no-arg constructor root is a plain Resource.pure") {
      val res: Resource[SyncIO, NoDeps] = DICats.wireResource[SyncIO, NoDeps]()
      val out = res.use(n => SyncIO(n)).unsafeRunSync()
      (out ne null) ==> true
    }

    test("a plain instance is spliced by reference (same identity, not rebuilt)") {
      val config = new Config("db-url")
      val res: Resource[SyncIO, OnlyConfig] = DICats.wireResource[SyncIO, OnlyConfig](config)
      val out = res.use(o => SyncIO(o.config)).unsafeRunSync()
      (out eq config) ==> true
    }

    test("a subtype instance resolves a supertype constructor parameter (same identity)") {
      val impl = new AAImpl(1)
      val res: Resource[SyncIO, NeedsA] = DICats.wireResource[SyncIO, NeedsA](impl)
      val out = res.use(n => SyncIO(n.a)).unsafeRunSync()
      (out eq impl) ==> true
    }

    test("bare `wireResource` (macwire-style import) works the same as DICats.wireResource") {
      // `wireResource` is already in scope from the enclosing `hearth.kindlings.dicats` package object.
      val config = new Config("db-url")
      val logger = new Logger
      val res: Resource[SyncIO, App] = wireResource[SyncIO, App](config, logger)
      res.use(app => SyncIO(app.config)).unsafeRunSync() ==> config
    }
  }

  group("DICats.wireResource — Resource[F, X] dependency") {

    test("a single Resource[F, X] dep is flatMapped, preserving acquire/release ordering") {
      val log = ListBuffer.empty[String]
      val config = new Config("db-url")
      val dbResource: Resource[SyncIO, Db] =
        Resource.make(SyncIO { log += "acquire-db"; new Db(config) })(_ => SyncIO { log += "release-db"; () })

      val res: Resource[SyncIO, AppWithDb] = DICats.wireResource[SyncIO, AppWithDb](config, dbResource)
      val out = res
        .use { app =>
          SyncIO { log += "use"; app }
        }
        .unsafeRunSync()

      out.config ==> config
      // acquire happens before use, release after use.
      log.toList ==> List("acquire-db", "use", "release-db")
    }

    test("a subtype Resource[F, AA] resolves a supertype parameter A") {
      val impl = new AAImpl(7)
      val aaResource: Resource[SyncIO, AAImpl] = Resource.pure(impl)
      val res: Resource[SyncIO, NeedsA] = DICats.wireResource[SyncIO, NeedsA](aaResource)
      val out = res.use(n => SyncIO(n.a)).unsafeRunSync()
      (out eq impl) ==> true
    }
  }

  group("DICats.wireResource — F[X] effect dependency") {

    test("a single F[X] effect dep is wrapped in Resource.eval then flatMapped") {
      val log = ListBuffer.empty[String]
      val config = new Config("db-url")
      val dbEffect: SyncIO[Db] = SyncIO { log += "eval-db"; new Db(config) }

      val res: Resource[SyncIO, AppWithDb] = DICats.wireResource[SyncIO, AppWithDb](config, dbEffect)
      val out = res.use(app => SyncIO { log += "use"; app }).unsafeRunSync()

      out.config ==> config
      out.db.config ==> config
      // eval runs during acquisition, before use; no release for an eval'd effect.
      log.toList ==> List("eval-db", "use")
    }

    test("two F[X] effects are evaluated in input order") {
      val log = ListBuffer.empty[String]
      val aEffect: SyncIO[ServiceA] = SyncIO { log += "eval-a"; new ServiceA }
      val bEffect: SyncIO[ServiceB] = SyncIO { log += "eval-b"; new ServiceB }

      val res: Resource[SyncIO, TwoServices] = DICats.wireResource[SyncIO, TwoServices](aEffect, bEffect)
      val out = res.use(app => SyncIO { log += "use"; app }).unsafeRunSync()

      (out.a ne null) ==> true
      (out.b ne null) ==> true
      log.toList ==> List("eval-a", "eval-b", "use")
    }
  }

  group("DICats.wireResource — mixed") {

    test("two resources + one instance compose in input order, innermost is the construction") {
      val log = ListBuffer.empty[String]
      val config = new Config("db-url")
      val dbResource: Resource[SyncIO, Db] =
        Resource.make(SyncIO { log += "acquire-db"; new Db(config) })(_ => SyncIO { log += "release-db"; () })
      val cacheResource: Resource[SyncIO, Cache] =
        Resource.make(SyncIO { log += "acquire-cache"; new Cache })(_ => SyncIO { log += "release-cache"; () })

      val res: Resource[SyncIO, FullApp] =
        DICats.wireResource[SyncIO, FullApp](config, dbResource, cacheResource)
      val out = res.use(app => SyncIO { log += "use"; app }).unsafeRunSync()

      out.config ==> config
      // db acquired first (input order), then cache; released in reverse (cache then db).
      log.toList ==> List("acquire-db", "acquire-cache", "use", "release-cache", "release-db")
    }

    test("falls back to a companion apply when the primary constructor is not used") {
      val config = new Config("db-url")
      val res: Resource[SyncIO, ViaApply] = DICats.wireResource[SyncIO, ViaApply](config)
      res.use(v => SyncIO(v.config)).unsafeRunSync() ==> config
    }

    test("one effect + one resource + one instance compose with full acquire/eval/use/release ordering") {
      val log = ListBuffer.empty[String]
      val config = new Config("db-url")
      val dbEffect: SyncIO[Db] = SyncIO { log += "eval-db"; new Db(config) }
      val cacheResource: Resource[SyncIO, Cache] =
        Resource.make(SyncIO { log += "acquire-cache"; new Cache })(_ => SyncIO { log += "release-cache"; () })

      val res: Resource[SyncIO, FullApp] =
        DICats.wireResource[SyncIO, FullApp](config, dbEffect, cacheResource)
      val out = res.use(app => SyncIO { log += "use"; app }).unsafeRunSync()

      out.config ==> config
      // db effect evaluated first (input order), then cache acquired; cache released after use (no release for an eval).
      log.toList ==> List("eval-db", "acquire-cache", "use", "release-cache")
    }
  }

  group("DICats.wireResource — factory methods (FunctionN deps)") {

    test("a no-parameter factory method produces a dependency for the graph") {
      val created = ListBuffer.empty[String]
      val makeDb: () => Db = () => { created += "db"; new Db(new Config("from-factory")) }
      val res: Resource[SyncIO, AppWithDb] =
        DICats.wireResource[SyncIO, AppWithDb](new Config("c"), makeDb)
      val out = res.use(a => SyncIO(a)).unsafeRunSync()
      (out.db ne null) ==> true
      created.toList ==> List("db")
    }

    test("a factory method's own parameter is resolved from another provider") {
      val created = ListBuffer.empty[String]
      val config = new Config("c")
      val makeDb: Config => Db = (cfg: Config) => { created += s"db($cfg)"; new Db(cfg) }
      val res: Resource[SyncIO, AppWithDb] = DICats.wireResource[SyncIO, AppWithDb](config, makeDb)
      val out = res.use(a => SyncIO(a)).unsafeRunSync()
      (out.db.config eq config) ==> true
      created.size ==> 1
    }

    test("a factory returning F[X] is Resource.eval-wrapped; its parameter comes from a Resource provider") {
      val factoryParams = ListBuffer.empty[Config]
      val makeDb: Config => SyncIO[Db] = (cfg: Config) => SyncIO { factoryParams += cfg; new Db(cfg) }
      val configResource: Resource[SyncIO, Config] = Resource.pure(new Config("rc"))
      val res: Resource[SyncIO, AppWithDb] = DICats.wireResource[SyncIO, AppWithDb](makeDb, configResource)
      val out = res.use(a => SyncIO(a)).unsafeRunSync()
      (out.db ne null) ==> true
      factoryParams.size ==> 1
      (factoryParams.head eq out.db.config) ==> true
    }

    test("a factory returning Resource[F, X] is flatMapped; its parameter comes from a Resource provider") {
      val factoryParams = ListBuffer.empty[Config]
      val makeDb: Config => Resource[SyncIO, Db] =
        (cfg: Config) => Resource.pure { factoryParams += cfg; new Db(cfg) }
      val configResource: Resource[SyncIO, Config] = Resource.pure(new Config("rc"))
      val res: Resource[SyncIO, AppWithDb] = DICats.wireResource[SyncIO, AppWithDb](makeDb, configResource)
      val out = res.use(a => SyncIO(a)).unsafeRunSync()
      (out.db ne null) ==> true
      factoryParams.size ==> 1
      (factoryParams.head eq out.db.config) ==> true
    }

    test("mixed effect + resource + factory(effect, resource) compose in dependency order") {
      val created = ListBuffer.empty[String]
      val ioA: SyncIO[ServiceA] = SyncIO { created += "a"; new ServiceA }
      val resourceB: Resource[SyncIO, ServiceB] = Resource.eval(SyncIO { created += "b"; new ServiceB })
      val makeC: (ServiceA, ServiceB) => Resource[SyncIO, ServiceC] =
        (a: ServiceA, b: ServiceB) => Resource.eval(SyncIO { created += "c"; new ServiceC(a, b) })

      val res: Resource[SyncIO, NeedsC] = DICats.wireResource[SyncIO, NeedsC](ioA, resourceB, makeC)
      val out = res.use(d => SyncIO(d)).unsafeRunSync()

      (out.c ne null) ==> true
      created.toList ==> List("a", "b", "c")
    }

    test("chained factory methods are topologically ordered (a before b)") {
      val created = ListBuffer.empty[String]
      val theA: () => Resource[SyncIO, ServiceA] =
        () => Resource.eval(SyncIO { created += "a"; new ServiceA })
      val theB: ServiceA => Resource[SyncIO, ServiceB] =
        (_: ServiceA) => Resource.eval(SyncIO { created += "b"; new ServiceB })

      val res: Resource[SyncIO, TwoServices] = DICats.wireResource[SyncIO, TwoServices](theA, theB)
      val out = res.use(c => SyncIO(c)).unsafeRunSync()

      (out.a ne null) ==> true
      (out.b ne null) ==> true
      created.toList ==> List("a", "b")
    }

    test("factory methods are sorted by their inter-dependencies regardless of input order (b before c)") {
      val created = ListBuffer.empty[String]
      // makeC depends on B; makeB depends on A. Passed in the 'wrong' order (makeC first).
      val makeB: ServiceA => Resource[SyncIO, ServiceB] =
        (_: ServiceA) => Resource.eval(SyncIO { created += "b"; new ServiceB })
      val makeC: ServiceB => Resource[SyncIO, ServiceC2] =
        (b: ServiceB) => Resource.eval(SyncIO { created += "c"; new ServiceC2(b) })

      val res: Resource[SyncIO, NeedsCD] = DICats.wireResource[SyncIO, NeedsCD](makeC, makeB)
      val out = res.use(e => SyncIO(e)).unsafeRunSync()

      (out.c ne null) ==> true
      (out.d ne null) ==> true
      created.toList ==> List("b", "c")
    }
  }

  group("DICats.wireResource — recursive intermediate construction (sharing)") {

    test("an intermediate type with no provider is auto-constructed from the graph") {
      val config = new Config("c")
      val res: Resource[SyncIO, NeedsAppWithDb] = DICats.wireResource[SyncIO, NeedsAppWithDb](config)
      val out = res.use(n => SyncIO(n)).unsafeRunSync()
      (out.app.db ne null) ==> true
      (out.app.config eq config) ==> true
    }

    test("a shared auto-built instance is created exactly once and reused (eq) across params") {
      val created = ListBuffer.empty[String]
      val res: Resource[SyncIO, SharesDb] =
        DICats.wireResource[SyncIO, SharesDb](new Config("c"), CountingDb.factory(created))
      val out = res.use(s => SyncIO(s)).unsafeRunSync()
      // Db feeds both `directDb` and `app.db`; it must be the same instance, built once.
      (out.directDb eq out.app.db) ==> true
      created.toList ==> List("db")
    }

    test("an auto-built Resource intermediate is allocated exactly once") {
      val allocated = ListBuffer.empty[String]
      val dbResource: Resource[SyncIO, Db] =
        Resource.make(SyncIO { allocated += "db"; new Db(new Config("c")) })(_ => SyncIO(()))
      // NeedsTwoDbs needs two Db params; the single Resource provider must be shared, allocated once.
      val res: Resource[SyncIO, NeedsTwoDbs] = DICats.wireResource[SyncIO, NeedsTwoDbs](dbResource)
      val out = res.use(n => SyncIO(n)).unsafeRunSync()
      (out.first eq out.second) ==> true
      allocated.length ==> 1
    }
  }

  group("DICats.wireResource — deliberate divergence from macwire") {

    // macwire's `constructInputProvider` fixture is a FAILURE: macwire never reuses a directly-passed root instance,
    // it always rebuilds the root via a creator, so `autowire[B](new B(new A("s")))` fails on the missing String
    // (macwire even flags this with `// TODO we should add a warning in this case.`). di-cats DELIBERATELY DIVERGES:
    // a provided instance whose type matches the root IS reused as the root (wrapped in `Resource.pure`). This is the
    // least-surprising behavior and is consistent with how di-cats resolves every other parameter by provided type.
    test("a directly-passed root instance is reused as the root (NOT rebuilt — diverges from macwire)") {
      val b = new NeedsString("s")
      val res: Resource[SyncIO, NeedsString] = DICats.wireResource[SyncIO, NeedsString](b)
      val out = res.use(x => SyncIO(x)).unsafeRunSync()
      (out eq b) ==> true
    }
  }

  group("DICats.wireResource — compile errors (macwire parity)") {

    test("an unused provider is reported") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, OnlyConfig](new Config("c"), new Logger)
        """
      ).check("Not used providers for the following types", "Logger")
    }

    test("missing dependency reports a macwire-style header + single-level path") {
      // NeedsString's constructor needs a String (a non-wireable scala.* type): it cannot be auto-built.
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, NeedsString]()
        """
      ).check(
        "Failed to create an instance of [hearth.kindlings.dicats.ResourceWiringSpec.NeedsString].",
        "Missing dependency of type [java.lang.String]. Path",
        "[constructor hearth.kindlings.dicats.ResourceWiringSpec.NeedsString].s"
      )
    }

    test("multi-level missing dependency reports the full construction path") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, NeedsNeedsString]()
        """
      ).check(
        "Failed to create an instance of [hearth.kindlings.dicats.ResourceWiringSpec.NeedsNeedsString].",
        "[constructor hearth.kindlings.dicats.ResourceWiringSpec.NeedsNeedsString].n",
        "[constructor hearth.kindlings.dicats.ResourceWiringSpec.NeedsString].s"
      )
    }

    test("ambiguous dependency lists the conflicting types") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, AppWithDb](new Config("a"), new Config("b"), new Db(new Config("c")))
        """
      ).check("Ambiguous instances of types", "Config")
    }

    test("ambiguous trait implementations list all matching types") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, NeedsT](new TImplA, new TImplB)
        """
      ).check("Ambiguous instances of types", "TImplA", "TImplB")
    }
  }
}

object ResourceWiringSpec {

  final class Config(val url: String)
  final class Logger
  final class Cache
  final class Db(val config: Config)

  final class App(val config: Config, val logger: Logger)
  final class AppWithDb(val db: Db, val config: Config)
  final class FullApp(val db: Db, val cache: Cache, val config: Config)

  // Zero-dependency, no-arg constructor root.
  final class NoDeps

  // Single-instance root (identity assertion).
  final class OnlyConfig(val config: Config)

  // Subtype resolution: AAImpl <: A, resolved to an `A` parameter.
  trait A
  final class AAImpl(val i: Int) extends A
  final class NeedsA(val a: A)

  // Two effects in input order.
  final class ServiceA
  final class ServiceB
  final class TwoServices(val a: ServiceA, val b: ServiceB)

  // Ambiguous trait implementations.
  trait T
  final class TImplA extends T
  final class TImplB extends T
  final class NeedsT(val t: T)

  // Genuinely-missing dependency (String is non-wireable): drives the macwire-style missing-path error.
  final class NeedsString(val s: String)
  final class NeedsNeedsString(val n: NeedsString)

  // Factory-method / chaining fixtures.
  final class ServiceC(val a: ServiceA, val b: ServiceB)
  final class NeedsC(val c: ServiceC)
  final class ServiceC2(val b: ServiceB)
  final class NeedsCD(val c: ServiceC2, val d: ServiceB)

  // Recursive intermediate construction + sharing fixtures.
  final class NeedsAppWithDb(val app: AppWithDb)
  final class NeedsTwoDbs(val first: Db, val second: Db)

  // A Db built by a counting factory (asserts single construction when shared).
  final class SharesDb(val directDb: Db, val app: AppWithDb)
  object CountingDb {
    def factory(log: ListBuffer[String]): Config => Db = { (cfg: Config) =>
      log += "db"; new Db(cfg)
    }
  }

  // Private primary constructor forces the companion-apply fallback.
  final class ViaApply private (val config: Config)
  object ViaApply {
    def apply(config: Config): ViaApply = new ViaApply(config)
  }
}
