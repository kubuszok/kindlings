package hearth.kindlings.optics

/** The result of `obj.modify(_.a.b.c)`: a focused view on a single (possibly deeply nested) field `A` of a source value
  * `S`, carrying the original `obj` together with the compiler-generated copy-with-modification function `doModify`.
  *
  * The terminal operations (`using`/`setTo`/...) supply the `A => A` transformation; `doModify` threads it through the
  * nested `.copy` chain the macro generated and returns the rebuilt `S`. Mirrors SoftwareMill quicklens' `PathModify`.
  */
final case class PathModify[S, A](private val obj: S, private val doModify: (S, A => A) => S) {

  /** Apply `mod` to the focused field and rebuild `S`. */
  def using(mod: A => A): S = doModify(obj, mod)

  /** Alias of [[using]] — `obj.modify(_.a)(f)`. */
  def apply(mod: A => A): S = using(mod)

  /** Replace the focused field with `v`. */
  def setTo(v: A): S = using(_ => v)

  /** Replace the focused field with the contents of `v` when it is defined, otherwise leave `S` unchanged. */
  def setToIfDefined(v: Option[A]): S = v.fold(obj)(setTo)

  /** Replace the focused field with `v` only when `cond` holds, otherwise leave `S` unchanged. `v` is by-name so it is
    * evaluated only when used.
    */
  def setToIf(cond: Boolean)(v: => A): S = if (cond) setTo(v) else obj

  /** Apply `mod` to the focused field only when `cond` holds, otherwise leave `S` unchanged. */
  def usingIf(cond: Boolean)(mod: A => A): S = if (cond) using(mod) else obj
}

/** A *reusable* lens, the result of `modifyLens[T](_.a.b)` / `modifyAllLens[T](_.a, _.b)`: a focused view on a
  * (possibly deeply nested) field `U` of `T` that is NOT bound to a specific object. Carries only the
  * compiler-generated copy-with-modification function `doModify`; its terminal operations (`using`/`setTo`/...) return
  * a reusable `T => T` function instead of a rebuilt value.
  *
  * Mirrors SoftwareMill quicklens' `PathLazyModify`. Compose two lenses with [[andThenModify]].
  */
final case class PathLazyModify[T, U](doModify: (T, U => U) => T) {

  /** A reusable `T => T` that applies `mod` to the focused field. */
  def using(mod: U => U): T => T = t => doModify(t, mod)

  /** Alias of [[using]]. */
  def apply(mod: U => U): T => T = using(mod)

  /** A reusable `T => T` that replaces the focused field with `v`. */
  def setTo(v: U): T => T = using(_ => v)

  /** A reusable `T => T` that replaces the focused field with the contents of `v` when defined, else leaves `T` as-is.
    */
  def setToIfDefined(v: Option[U]): T => T = v.fold((t: T) => t)(setTo)

  /** A reusable `T => T` that replaces the focused field with `v` only when `cond` holds, else leaves `T` as-is. */
  def setToIf(cond: Boolean)(v: => U): T => T = if (cond) setTo(v) else (t: T) => t

  /** A reusable `T => T` that applies `mod` to the focused field only when `cond` holds, else leaves `T` as-is. */
  def usingIf(cond: Boolean)(mod: U => U): T => T = if (cond) using(mod) else (t: T) => t

  /** Compose this lens `T -> U` with a lens `U -> V`, yielding a lens `T -> V`: focusing through `this`, then through
    * `other`. `modifyLens[T](_.a) andThenModify modifyLens[U](_.b)` focuses `_.a.b` on a `T`.
    */
  def andThenModify[V](other: PathLazyModify[U, V]): PathLazyModify[T, V] =
    PathLazyModify[T, V]((t, modV) => doModify(t, u => other.doModify(u, modV)))
}
