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
