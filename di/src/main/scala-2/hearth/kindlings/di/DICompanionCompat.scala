package hearth.kindlings.di

import scala.language.experimental.macros

private[di] trait DICompanionCompat {

  /** Construct an `A` by wiring its primary-constructor parameters from values found in the enclosing lexical scope. */
  def wire[A]: A = macro internal.compiletime.WiringMacros.wireImpl[A]

  /** Like [[wire]], but recursively constructs any dependency not found in scope (refusing `java.*`/`scala.*` types).
    */
  def wireRec[A]: A = macro internal.compiletime.WiringMacros.wireRecImpl[A]

  /** Build a complete object graph for `A` from an explicit list of dependencies (instances and factory functions),
    * constructing everything else and sharing each instance through a generated local `val`.
    */
  def autowire[A](dependencies: Any*): A = macro internal.compiletime.WiringMacros.autowireImpl[A]

  /** Collect all values of a type conforming to `A` from the enclosing scope into a `Set[A]`. */
  def wireSet[A]: Set[A] = macro internal.compiletime.WiringMacros.wireSetImpl[A]

  /** Collect all values of a type conforming to `A` from the enclosing scope into a `List[A]`, preserving order. */
  def wireList[A]: List[A] = macro internal.compiletime.WiringMacros.wireListImpl[A]

  // wireWith — resolve each parameter of a factory function from the enclosing scope, then apply it.
  def wireWith[RES](factory: () => RES): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, RES](factory: (A) => RES): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, RES](factory: (A, B) => RES): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, RES](factory: (A, B, C) => RES): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, RES](factory: (A, B, C, D) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, RES](factory: (A, B, C, D, E) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, RES](factory: (A, B, C, D, E, F) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, RES](factory: (A, B, C, D, E, F, G) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, RES](factory: (A, B, C, D, E, F, G, H) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, RES](factory: (A, B, C, D, E, F, G, H, I) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, RES](factory: (A, B, C, D, E, F, G, H, I, J) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, RES](factory: (A, B, C, D, E, F, G, H, I, J, K) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, RES](factory: (A, B, C, D, E, F, G, H, I, J, K, L) => RES): RES =
    macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
  def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RES](
      factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RES
  ): RES = macro internal.compiletime.WiringMacros.wireWithImpl[RES]
}
