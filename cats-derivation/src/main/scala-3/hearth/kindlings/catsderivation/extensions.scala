package hearth.kindlings.catsderivation

object extensions {

  extension (companion: cats.Show.type) {
    inline def derived[A]: cats.Show[A] = ${ internal.compiletime.ShowMacros.deriveShowImpl[A] }
  }

  extension (companion: cats.kernel.Eq.type) {
    inline def derived[A]: cats.kernel.Eq[A] = ${ internal.compiletime.EqMacros.deriveEqImpl[A] }
  }

  extension (companion: cats.kernel.Order.type) {
    inline def derived[A]: cats.kernel.Order[A] = ${ internal.compiletime.OrderMacros.deriveOrderImpl[A] }
  }

  extension (companion: cats.kernel.PartialOrder.type) {
    inline def derived[A]: cats.kernel.PartialOrder[A] = ${
      internal.compiletime.PartialOrderMacros.derivePartialOrderImpl[A]
    }
  }

  extension (companion: cats.kernel.Hash.type) {
    inline def derived[A]: cats.kernel.Hash[A] = ${ internal.compiletime.HashMacros.deriveHashImpl[A] }
  }

  extension (companion: cats.kernel.Semigroup.type) {
    inline def derived[A]: cats.kernel.Semigroup[A] = ${ internal.compiletime.SemigroupMacros.deriveSemigroupImpl[A] }
  }

  extension (companion: cats.kernel.Monoid.type) {
    inline def derived[A]: cats.kernel.Monoid[A] = ${ internal.compiletime.MonoidMacros.deriveMonoidImpl[A] }
  }

  extension (companion: cats.kernel.CommutativeSemigroup.type) {
    inline def derived[A]: cats.kernel.CommutativeSemigroup[A] = ${
      internal.compiletime.CommutativeSemigroupMacros.deriveCommutativeSemigroupImpl[A]
    }
  }

  extension (companion: cats.kernel.CommutativeMonoid.type) {
    inline def derived[A]: cats.kernel.CommutativeMonoid[A] = ${
      internal.compiletime.CommutativeMonoidMacros.deriveCommutativeMonoidImpl[A]
    }
  }

  extension (companion: alleycats.Empty.type) {
    inline def derived[A]: alleycats.Empty[A] = ${ internal.compiletime.EmptyMacros.deriveEmptyImpl[A] }
  }

  extension (companion: cats.Functor.type) {
    inline def derived[F[_]]: cats.Functor[F] = ${ internal.compiletime.FunctorMacros.deriveFunctorImpl[F] }
  }

  extension (companion: cats.Contravariant.type) {
    inline def derived[F[_]]: cats.Contravariant[F] = ${
      internal.compiletime.ContravariantMacros.deriveContravariantImpl[F]
    }
  }

  extension (companion: cats.Invariant.type) {
    inline def derived[F[_]]: cats.Invariant[F] = ${
      internal.compiletime.InvariantMacros.deriveInvariantImpl[F]
    }
  }

  extension (companion: alleycats.Pure.type) {
    inline def derived[F[_]]: alleycats.Pure[F] = ${ internal.compiletime.PureMacros.derivePureImpl[F] }
  }

  extension (companion: alleycats.EmptyK.type) {
    inline def derived[F[_]]: alleycats.EmptyK[F] = ${ internal.compiletime.EmptyKMacros.deriveEmptyKImpl[F] }
  }

  extension (companion: cats.SemigroupK.type) {
    inline def derived[F[_]]: cats.SemigroupK[F] = ${ internal.compiletime.SemigroupKMacros.deriveSemigroupKImpl[F] }
  }

  extension (companion: cats.MonoidK.type) {
    inline def derived[F[_]]: cats.MonoidK[F] = ${ internal.compiletime.MonoidKMacros.deriveMonoidKImpl[F] }
  }

  extension (companion: cats.Apply.type) {
    inline def derived[F[_]]: cats.Apply[F] = ${ internal.compiletime.ApplyMacros.deriveApplyImpl[F] }
  }

  extension (companion: cats.Applicative.type) {
    inline def derived[F[_]]: cats.Applicative[F] = ${
      internal.compiletime.ApplicativeMacros.deriveApplicativeImpl[F]
    }
  }

  extension (companion: cats.Foldable.type) {
    inline def derived[F[_]]: cats.Foldable[F] = ${ internal.compiletime.FoldableMacros.deriveFoldableImpl[F] }
  }

  extension (companion: cats.Traverse.type) {
    inline def derived[F[_]]: cats.Traverse[F] = ${ internal.compiletime.TraverseMacros.deriveTraverseImpl[F] }
  }

  extension (companion: cats.Reducible.type) {
    inline def derived[F[_]]: cats.Reducible[F] = ${ internal.compiletime.ReducibleMacros.deriveReducibleImpl[F] }
  }

  extension (companion: cats.NonEmptyTraverse.type) {
    inline def derived[F[_]]: cats.NonEmptyTraverse[F] = ${
      internal.compiletime.NonEmptyTraverseMacros.deriveNonEmptyTraverseImpl[F]
    }
  }

  extension (companion: cats.NonEmptyAlternative.type) {
    inline def derived[F[_]]: cats.NonEmptyAlternative[F] = ${
      internal.compiletime.NonEmptyAlternativeMacros.deriveNonEmptyAlternativeImpl[F]
    }
  }

  extension (companion: cats.Alternative.type) {
    inline def derived[F[_]]: cats.Alternative[F] = ${
      internal.compiletime.AlternativeMacros.deriveAlternativeImpl[F]
    }
  }

  extension (companion: alleycats.ConsK.type) {
    inline def derived[F[_]]: alleycats.ConsK[F] = ${
      internal.compiletime.ConsKMacros.deriveConsKImpl[F]
    }
  }
}
