package hearth.kindlings.di

private[di] trait DICompanionCompat {

  /** Construct an `A` by wiring its primary-constructor parameters from values found in the enclosing lexical scope. */
  inline def wire[A]: A = ${ internal.compiletime.WiringMacros.wireImpl[A] }

  /** Like [[wire]], but recursively constructs any dependency not found in scope (refusing `java.*`/`scala.*` types). */
  inline def wireRec[A]: A = ${ internal.compiletime.WiringMacros.wireRecImpl[A] }

  /** Build a complete object graph for `A` from an explicit list of dependencies (instances and factory functions),
    * constructing everything else and sharing each instance through a generated local `val`.
    */
  inline def autowire[A](inline dependencies: Any*): A = ${ internal.compiletime.WiringMacros.autowireImpl[A]('dependencies) }

  /** Collect all values of a type conforming to `A` from the enclosing scope into a `Set[A]`. */
  inline def wireSet[A]: Set[A] = ${ internal.compiletime.WiringMacros.wireSetImpl[A] }

  /** Collect all values of a type conforming to `A` from the enclosing scope into a `List[A]`, preserving order. */
  inline def wireList[A]: List[A] = ${ internal.compiletime.WiringMacros.wireListImpl[A] }

  // wireWith — resolve each parameter of a factory function from the enclosing scope, then apply it.
  inline def wireWith[RES](inline factory: () => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, RES](inline factory: (A) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, RES](inline factory: (A, B) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, RES](inline factory: (A, B, C) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, RES](inline factory: (A, B, C, D) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, RES](inline factory: (A, B, C, D, E) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, RES](inline factory: (A, B, C, D, E, F) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, RES](inline factory: (A, B, C, D, E, F, G) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, RES](inline factory: (A, B, C, D, E, F, G, H) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, RES](inline factory: (A, B, C, D, E, F, G, H, I) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, RES](inline factory: (A, B, C, D, E, F, G, H, I, J) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
  inline def wireWith[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, RES](inline factory: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RES): RES = ${ internal.compiletime.WiringMacros.wireWithImpl[RES]('factory) }
}
