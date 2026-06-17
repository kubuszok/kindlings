package hearth.kindlings.optics

import scala.language.experimental.macros

/** Compile-time evidence that `_.xs.each` type-checks, with the type member `Elem` set to the traversed element type.
  *
  * It is **materialized by a whitebox macro** that consults Hearth's `IsCollection`/`IsMap`/`IsOption` SPI: `.each`
  * type-checks ONLY when the container actually has a provider (the built-ins, or anything on the classpath such as
  * cats `NonEmpty*` via `kindlings-cats-integration`), and `Elem` is taken from that provider. There are no
  * per-container instances — a new provider jar lights `.each` up automatically; a non-container fails to compile right
  * at the `.each`.
  */
sealed trait IsElementOf[C] { type Elem }

object IsElementOf {
  type Aux[C, A] = IsElementOf[C] { type Elem = A }

  private val instance: IsElementOf[Any] = new IsElementOf[Any] { type Elem = Any }

  /** The (phantom) evidence value the materializer emits. `.each` is `@compileTimeOnly` and the `modify` macro discards
    * the evidence, so this is never evaluated at runtime; it exists only to carry the refined `Elem` type.
    */
  def witness[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]

  implicit def derived[C]: IsElementOf[C] = macro internal.compiletime.ModifyMacros.isElementOfImpl[C]
}
