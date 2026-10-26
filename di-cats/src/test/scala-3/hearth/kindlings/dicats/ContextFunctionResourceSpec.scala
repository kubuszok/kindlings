package hearth.kindlings.dicats

import hearth.MacroSuite
import cats.effect.{Resource, SyncIO}

/** Scala 3 only: a context function (`A ?=> X`) passed to `wireResource` is a factory, its `using` parameters
  * resolved from the graph and its result wrapped per its return shape, exactly like a plain function factory.
  */
final class ContextFunctionResourceSpec extends MacroSuite {

  import ResourceWiringSpec.*

  group("DICats.wireResource — context function factories") {

    test("a context function returning a plain value is a factory") {
      val config = new Config("ctx")
      val res: Resource[SyncIO, AppWithDb] =
        DICats.wireResource[SyncIO, AppWithDb](config, (cfg: Config) ?=> new Db(cfg))
      (res.use(a => SyncIO(a)).unsafeRunSync().db.config eq config) ==> true
    }

    test("a context function returning Resource[F, X] is flatMapped") {
      val config = new Config("ctx-resource")
      val res: Resource[SyncIO, AppWithDb] =
        DICats.wireResource[SyncIO, AppWithDb](
          config,
          (cfg: Config) ?=> Resource.pure[SyncIO, Db](new Db(cfg))
        )
      (res.use(a => SyncIO(a)).unsafeRunSync().db.config eq config) ==> true
    }
  }
}
