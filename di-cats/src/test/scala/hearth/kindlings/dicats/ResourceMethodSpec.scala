package hearth.kindlings.dicats

import hearth.MacroSuite
import cats.effect.{Resource, Sync, SyncIO}

/** Tests for the Kindlings extension: auto-discovering a companion `object T { def resource[F[_]: <constraints>]:
  * Resource[F, T] }` factory and using it to build `T` (applying our `F`, summoning its context-bound implicits).
  */
final class ResourceMethodSpec extends MacroSuite {

  import ResourceMethodSpec.*

  group("DICats.wireResource — companion resource[F] factory") {

    test("uses a constraint-free companion resource[F] to build the root") {
      val res: Resource[SyncIO, Widget] = DICats.wireResource[SyncIO, Widget]()
      res.use(w => SyncIO.pure(w)).unsafeRunSync().id ==> 1
    }

    test("uses a context-bound companion resource[F: Sync] (summoning Sync[F])") {
      val res: Resource[SyncIO, Gadget] = DICats.wireResource[SyncIO, Gadget]()
      res.use(g => SyncIO.pure(g)).unsafeRunSync().n ==> 2
    }

    test("uses a companion resource[F] for a transitive dependency of the root") {
      // App needs a Widget; Widget has no provider but its companion resource[F] supplies it.
      val res: Resource[SyncIO, App] = DICats.wireResource[SyncIO, App]()
      res.use(a => SyncIO.pure(a)).unsafeRunSync().widget.id ==> 1
    }

    test("resolves an explicit value parameter of resource[F: Sync](config) from provided deps") {
      val config = new Config("jdbc://root")
      val res: Resource[SyncIO, Service] = DICats.wireResource[SyncIO, Service](config)
      val s = res.use(SyncIO.pure).unsafeRunSync()
      s.config.url ==> "jdbc://root"
      s.n ==> 3
    }

    test("resolves an explicit resource[F] value parameter transitively") {
      // Server needs a Service; Service has no provider but its companion resource[F](config) supplies it, with the
      // explicit `config` resolved from the provided Config.
      val config = new Config("jdbc://transitive")
      val res: Resource[SyncIO, Server] = DICats.wireResource[SyncIO, Server](config)
      res.use(SyncIO.pure).unsafeRunSync().service.config.url ==> "jdbc://transitive"
    }

    test("reports the explicit resource param via the graph path when it cannot be resolved") {
      // Service.resource's explicit `config` param is resolved from the graph: with no Config provided it is recursively
      // constructed, which fails on its non-wireable `String` — the path proves the arg is threaded through the graph.
      compileErrors(
        """
        import hearth.kindlings.dicats.DICats
        import cats.effect.{Resource, SyncIO}
        import hearth.kindlings.dicats.ResourceMethodSpec.*
        DICats.wireResource[SyncIO, Service]()
        """
      ).check(
        "Missing dependency of type [java.lang.String].",
        "[method resource].arg0",
        "[constructor hearth.kindlings.dicats.ResourceMethodSpec.Config].url"
      )
    }
  }
}

object ResourceMethodSpec {

  final class Widget(val id: Int)
  object Widget {
    def resource[F[_]]: Resource[F, Widget] = Resource.pure(new Widget(1))
  }

  final class Gadget(val n: Int)
  object Gadget {
    def resource[F[_]: Sync]: Resource[F, Gadget] = Resource.eval(Sync[F].delay(new Gadget(2)))
  }

  final class App(val widget: Widget)

  final class Config(val url: String)

  // Companion `resource[F: Sync]` with an EXPLICIT value parameter resolved from the graph.
  final class Service(val config: Config, val n: Int)
  object Service {
    def resource[F[_]: Sync](config: Config): Resource[F, Service] =
      Resource.eval(Sync[F].delay(new Service(config, 3)))
  }

  final class Server(val service: Service)
}
