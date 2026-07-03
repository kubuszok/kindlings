package hearth.kindlings.di

/** A fluent builder for [[DI.plan]] — Kindlings' own opinionated wiring endpoint.
  *
  * Unlike [[DI.wire]] / [[DI.autowire]] (which follow macwire's conventions), `DI.plan[A]` is always recursive and
  * always caches: it builds the whole object graph reachable from `A`, instantiating each distinct type exactly once
  * and sharing it. The builder lets you customise, without touching your domain classes:
  *
  *   - the '''storage strategy''' for the whole graph — [[asVals]] (default), [[asLazyVals]], [[asDefs]];
  *   - a '''per-type storage override''' — [[storeAsVal]], [[storeAsLazyVal]], [[storeAsDef]];
  *   - a '''per-type construction override''' — [[provide]] (`for type T, initialise it with this factory`);
  *   - an inline '''wiring-graph dump''' (ZIO-Magic style) — [[debugTree]] / [[debugMermaid]].
  *
  * The chain is consumed at compile time by the `build` macro (see the platform-specific `DIPlan` companion); the
  * methods here are never actually executed at runtime — each simply returns `this` so the chain type-checks.
  *
  * {{{
  * val app: App =
  *   DI.plan[App]
  *     .asLazyVals                       // whole-graph default
  *     .storeAsDef[RequestScoped]        // per-type override
  *     .provide[Db](Db.connect())        // construction override
  *     .debugTree                        // print the wiring graph
  *     .build
  * }}}
  */
final class DIPlan[A] private[di] () {

  /** Store every constructed dependency as a `val` (the default — each is created once, eagerly). */
  def asVals: DIPlan[A] = this

  /** Store every constructed dependency as a `lazy val` (created once, on first use). */
  def asLazyVals: DIPlan[A] = this

  /** Store every constructed dependency as a `def` (re-created on every use). */
  def asDefs: DIPlan[A] = this

  /** Override the storage of `T` to a `val`, regardless of the whole-graph default. */
  def storeAsVal[T]: DIPlan[A] = this

  /** Override the storage of `T` to a `lazy val`. */
  def storeAsLazyVal[T]: DIPlan[A] = this

  /** Override the storage of `T` to a `def`. */
  def storeAsDef[T]: DIPlan[A] = this

  /** Construct `T` with `factory` instead of its constructor — "when you need a `T`, initialise it with this". The
    * factory is by-name and evaluated according to `T`'s storage (once for `val`/`lazy val`, per-use for `def`).
    */
  def provide[T](factory: => T): DIPlan[A] = this

  /** Print the wiring graph as an ASCII dependency tree (ZIO `ZLayer.Debug.tree` analog) at the wiring site. */
  def debugTree: DIPlan[A] = this

  /** Print the wiring graph as a Mermaid diagram + link (ZIO `ZLayer.Debug.mermaid` analog) at the wiring site. */
  def debugMermaid: DIPlan[A] = this
}
