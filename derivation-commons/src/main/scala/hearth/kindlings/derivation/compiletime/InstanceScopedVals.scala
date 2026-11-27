package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Binds expressions resolved during a derivation - summoned implicits (`Functor[List]`, `Semigroup[String]`, ...),
  * composed instances (`composeFunctor(Functor[List], Functor[Option])` for a `List[Option[A]]` field), kernel
  * instances built from `*K` ones (`SemigroupK[G].algebra`) - to `lazy val`s around the derived instance, so they are
  * evaluated once per instance instead of on every call of its methods.
  *
  * Without it, the resolved expression is spliced into the per-call method body, so an `implicit def` instance (e.g.
  * cats' `catsKernelStdMonoidForList[A]`, or a user's `implicit def`) and every `compose` is re-evaluated on each call.
  *
  * For derivations that do not thread an `MLocal[ValDefsCache]` through the code resolving those instances (e.g. the
  * polymorphic cats derivations, which resolve them in plain functions): the vals are recorded in a per-expansion
  * [[ValDefsCache]] (a macro bundle is instantiated once per expansion) and [[withInstanceScopedVals]] emits them
  * around the final instance. Every entry point that can reach [[instanceScoped]] MUST wrap its result with
  * [[withInstanceScopedVals]], otherwise the expansion refers to undefined vals.
  *
  * The vals are `lazy` so that an instance referring back to the instance being defined (e.g. an implicit `lazy val`
  * holding it) is not forced while the instance is created. Only expressions whose free references are visible at the
  * expansion's top level may be passed (no method parameters or type parameters of the generated methods).
  */
trait InstanceScopedVals { this: MacroCommons =>

  private var instanceScopedValsCache: Option[ValDefsCache] = None
  private var instanceScopedValsCount: Int = 0

  /** Returns a reference to a `lazy val` (around the instance being derived) holding `instance`. Identical expressions
    * (same type and code) share one val.
    */
  protected def instanceScoped[T: Type](instance: Expr[T]): Expr[T] = {
    val cache = instanceScopedValsCache.getOrElse(ValDefsCache.empty)
    val key = s"instance-scoped-val:${Type[T].plainPrint}:${instance.plainPrint}"
    cache.get0Ary[T](key) match {
      case Some(ref) => ref
      case None      =>
        instanceScopedValsCount += 1
        val updated =
          ValDefBuilder.ofLazy[T](s"instance$instanceScopedValsCount").buildCachedWith(cache, key)(_ => instance)
        instanceScopedValsCache = Some(updated)
        updated.get0Ary[T](key).get
    }
  }

  /** [[instanceScoped]] for erased (`Expr[Any]`) instances. */
  protected def instanceScopedAny(instance: Expr[Any]): Expr[Any] = instanceScoped(instance)(InstanceScopedAnyType)

  private lazy val InstanceScopedAnyType: Type[Any] = Type.of[Any]

  /** Emits the vals recorded by [[instanceScoped]] around `instance` (no-op when there are none). */
  protected def withInstanceScopedVals[T](instance: Expr[T]): Expr[T] =
    instanceScopedValsCache.fold(instance)(_.toValDefs.use(_ => instance))

  /** The vals recorded so far, to be handed to [[restoreInstanceScopedVals]] when a derivation attempt is abandoned (so
    * that the vals it recorded are not emitted around the result of the attempt that replaces it).
    */
  protected def instanceScopedValsSnapshot(): Any = (instanceScopedValsCache, instanceScopedValsCount)

  /** Forgets the vals recorded since `snapshot` (a value returned by [[instanceScopedValsSnapshot]]). */
  protected def restoreInstanceScopedVals(snapshot: Any): Unit = snapshot match {
    case (cache: Option[?], count: Int) =>
      instanceScopedValsCache = cache.asInstanceOf[Option[ValDefsCache]]
      instanceScopedValsCount = count
    case _ => ()
  }
}
