package hearth.kindlings.dicats

import hearth.MacroSuite
import cats.effect.{Resource, SyncIO}

/** Smoke test for di-cats' opt-in generation logging: importing `hearth.kindlings.dicats.debug.*` puts the
  * `LogGeneration` marker in scope, so `wireResource` emits (as a compiler-info message) the resolution logic, the
  * embedded ZIO-Magic-style wiring graph, and the generated code. Exercising it here proves the logging +
  * graph-building paths compile and run, and that the generated `Resource` is still correct.
  */
final class GenerationLoggingSpec extends MacroSuite {

  import hearth.kindlings.dicats.debug.* // enables the generation log (incl. the wiring graph) for macros in this scope

  group("wireResource with generation logging enabled") {

    test("still generates a correct Resource for a mixed graph (instance + Resource dependency)") {
      val config = new GenerationLoggingSpec.Config
      val dbResource: Resource[SyncIO, GenerationLoggingSpec.Db] =
        Resource.pure(new GenerationLoggingSpec.Db(config))
      val res: Resource[SyncIO, GenerationLoggingSpec.App] =
        DICats.wireResource[SyncIO, GenerationLoggingSpec.App](config, dbResource)
      val app = res.use(a => SyncIO.pure(a)).unsafeRunSync()
      (app.db.config eq config) ==> true
      (app.config eq config) ==> true
    }
  }
}

object GenerationLoggingSpec {
  final class Config
  final class Db(val config: Config)
  final class App(val db: Db, val config: Config)
}
