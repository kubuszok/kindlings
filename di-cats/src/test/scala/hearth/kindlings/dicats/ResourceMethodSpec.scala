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
}
