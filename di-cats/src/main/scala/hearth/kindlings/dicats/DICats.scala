package hearth.kindlings.dicats

/** Compile-time, F-agnostic `Resource[F, _]` dependency injection — an F-abstracted re-imagining of
  * [[https://github.com/softwaremill/macwire macwire]]'s `autocats.autowire` (which hardcodes
  * `Resource[cats.effect.IO, _]`). Built on Hearth's macro-agnostic API so the very same implementation works on Scala
  * 2.13 and Scala 3, across the JVM, Scala.js and Scala Native.
  *
  * `wireResource[F, T](deps*)` constructs a `T` from the supplied dependencies and wraps the whole construction in a
  * `Resource[F, T]`. Each dependency is classified by its type:
  *   - a `Resource[F, X]` is acquired (and released) around the construction — composed via `flatMap`;
  *   - an `F[X]` effect is run via `Resource.eval` then composed via `flatMap`;
  *   - anything else is a plain instance, spliced directly into the construction.
  *
  * {{{
  * import hearth.kindlings.dicats.DICats
  * import cats.effect.{Resource, SyncIO}
  *
  * class Config()
  * class Db(config: Config)
  * class App(db: Db, config: Config)
  *
  * val config = new Config()
  * val dbResource: Resource[SyncIO, Db] = Resource.pure(new Db(config))
  * // builds: dbResource.flatMap(db => Resource.pure[SyncIO, App](new App(db, config)))
  * val app: Resource[SyncIO, App] = DICats.wireResource[SyncIO, App](config, dbResource)
  * }}}
  *
  * No `Sync`/`Async`/`Applicative` constraint on `F` is required: in cats-effect 3 `Resource.pure`, `Resource.eval` and
  * `Resource#flatMap` are all constraint-free.
  */
object DICats extends DICatsCompanionCompat
