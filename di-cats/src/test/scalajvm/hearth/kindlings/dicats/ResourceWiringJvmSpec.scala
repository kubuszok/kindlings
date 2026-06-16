package hearth.kindlings.dicats

import hearth.MacroSuite
import cats.effect.{IO, Resource}
import scala.collection.mutable.ListBuffer

/** F-agnosticism tests that use `cats.effect.IO`. They live in a JVM-only source set because `IO.unsafeRunSync()`
  * blocks, which is unsupported on Scala.js. The cross-platform F-abstraction coverage uses `SyncIO` in
  * `ResourceWiringSpec`; these add a second, distinct `F` (`IO`) on the JVM. Fixtures come from `ResourceWiringSpec`.
  */
final class ResourceWiringJvmSpec extends MacroSuite {

  import ResourceWiringSpec.*

  group("DICats.wireResource — F-agnostic via IO (JVM only)") {

    test("the same derivation produces a Resource[IO, T] (proves F is abstract)") {
      import cats.effect.unsafe.implicits.global
      val config = new Config("db-url")
      val logger = new Logger
      val res: Resource[IO, App] = DICats.wireResource[IO, App](config, logger)
      val app = res.use(a => IO.pure(a)).unsafeRunSync()
      app.config ==> config
      app.logger ==> logger
    }

    test("an IO effect dependency is wrapped in Resource.eval and evaluated during acquisition") {
      import cats.effect.unsafe.implicits.global
      val log = ListBuffer.empty[String]
      val config = new Config("c")
      val dbEffect: IO[Db] = IO { log += "eval-db"; new Db(config) }
      val res: Resource[IO, AppWithDb] = DICats.wireResource[IO, AppWithDb](config, dbEffect)
      val out = res.use(a => IO { log += "use"; a }).unsafeRunSync()
      out.db.config ==> config
      log.toList ==> List("eval-db", "use")
    }
  }
}
