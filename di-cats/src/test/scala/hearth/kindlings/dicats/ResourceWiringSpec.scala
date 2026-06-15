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
  }

  group("DICats.wireResource — compile errors (macwire parity)") {

    test("missing dependency reports a clear error") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, App](new Config("x"))
        """
      ).arePresent()
    }

    test("ambiguous dependency reports a clear error") {
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceWiringSpec.*
        DICats.wireResource[SyncIO, AppWithDb](new Config("a"), new Config("b"), new Db(new Config("c")))
        """
      ).check("Ambiguous")
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

  // Private primary constructor forces the companion-apply fallback.
  final class ViaApply private (val config: Config)
  object ViaApply {
    def apply(config: Config): ViaApply = new ViaApply(config)
  }
}
