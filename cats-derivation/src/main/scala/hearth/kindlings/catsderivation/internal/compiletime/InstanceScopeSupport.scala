package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

/** Derives the method bodies of a monomorphic type class instance ONCE, at the instance scope.
  *
  * Each helper builds a cached `def` (in the shared [[ValDefsCache]]) whose body is the derived expression, and returns
  * a function producing a call to that `def`. The entry point then wraps the whole instance in
  * `cache.toValDefs.use { _ => Expr.quote { factory(...) } }` and only calls the `def`s from inside the per-call
  * lambdas.
  *
  * Without it, `cache.toValDefs.use` sat inside the per-call lambda, so every `show`/`eqv`/`compare`/`combine` call
  * re-created the cached `lazy val`s of the summoned field instances (a `LazyRef` and a synchronized initialisation
  * each). Same pattern as [[HashMacrosImpl]] (and the `hearth-def-caching` skill).
  */
trait InstanceScopeSupport { this: MacroCommons =>

  /** `def <name>(a: A): R = <body(a)>` at the instance scope; returns the call builder. */
  protected def instanceScopedDef1[A: Type, R: Type](cache: MLocal[ValDefsCache], name: String)(
      body: Expr[A] => MIO[Expr[R]]
  ): MIO[Expr[A] => Expr[R]] = {
    val key = s"instance-scope:$name"
    MIO.scoped { rs =>
      rs(cache.buildCachedWith(key, ValDefBuilder.ofDef1[A, R](name)) { case (_, a) => rs(body(a)) })
      rs(cache.get1Ary[A, R](key)).get
    }
  }

  /** `def <name>(x: A, y: B): R = <body(x, y)>` at the instance scope; returns the call builder. */
  protected def instanceScopedDef2[A: Type, B: Type, R: Type](cache: MLocal[ValDefsCache], name: String)(
      body: (Expr[A], Expr[B]) => MIO[Expr[R]]
  ): MIO[(Expr[A], Expr[B]) => Expr[R]] = {
    val key = s"instance-scope:$name"
    MIO.scoped { rs =>
      rs(cache.buildCachedWith(key, ValDefBuilder.ofDef2[A, B, R](name)) { case (_, (x, y)) => rs(body(x, y)) })
      rs(cache.get2Ary[A, B, R](key)).get
    }
  }
}
