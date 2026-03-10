package hearth.kindlings.catsderivation

import scala.language.experimental.macros

trait CatsDerivationScala2Extensions {

  implicit class ShowDerived(private val companion: cats.Show.type) {
    def derived[A]: cats.Show[A] = macro internal.compiletime.ShowMacros.deriveShowImpl[A]
  }

  implicit class EqDerived(private val companion: cats.kernel.Eq.type) {
    def derived[A]: cats.kernel.Eq[A] = macro internal.compiletime.EqMacros.deriveEqImpl[A]
  }

  implicit class OrderDerived(private val companion: cats.kernel.Order.type) {
    def derived[A]: cats.kernel.Order[A] = macro internal.compiletime.OrderMacros.deriveOrderImpl[A]
  }

  implicit class PartialOrderDerived(private val companion: cats.kernel.PartialOrder.type) {
    def derived[A]: cats.kernel.PartialOrder[A] =
      macro internal.compiletime.PartialOrderMacros.derivePartialOrderImpl[A]
  }

  implicit class HashDerived(private val companion: cats.kernel.Hash.type) {
    def derived[A]: cats.kernel.Hash[A] = macro internal.compiletime.HashMacros.deriveHashImpl[A]
  }

  implicit class SemigroupDerived(private val companion: cats.kernel.Semigroup.type) {
    def derived[A]: cats.kernel.Semigroup[A] = macro internal.compiletime.SemigroupMacros.deriveSemigroupImpl[A]
  }

  implicit class MonoidDerived(private val companion: cats.kernel.Monoid.type) {
    def derived[A]: cats.kernel.Monoid[A] = macro internal.compiletime.MonoidMacros.deriveMonoidImpl[A]
  }

  implicit class CommutativeSemigroupDerived(private val companion: cats.kernel.CommutativeSemigroup.type) {
    def derived[A]: cats.kernel.CommutativeSemigroup[A] =
      macro internal.compiletime.CommutativeSemigroupMacros.deriveCommutativeSemigroupImpl[A]
  }

  implicit class CommutativeMonoidDerived(private val companion: cats.kernel.CommutativeMonoid.type) {
    def derived[A]: cats.kernel.CommutativeMonoid[A] =
      macro internal.compiletime.CommutativeMonoidMacros.deriveCommutativeMonoidImpl[A]
  }

  implicit class EmptyDerived(private val companion: alleycats.Empty.type) {
    def derived[A]: alleycats.Empty[A] = macro internal.compiletime.EmptyMacros.deriveEmptyImpl[A]
  }

  implicit class FunctorDerived(private val companion: cats.Functor.type) {
    def derived[F[_]]: cats.Functor[F] = macro internal.compiletime.FunctorMacros.deriveFunctorImpl[F]
  }

  implicit class ContravariantDerived(private val companion: cats.Contravariant.type) {
    def derived[F[_]]: cats.Contravariant[F] =
      macro internal.compiletime.ContravariantMacros.deriveContravariantImpl[F]
  }

  implicit class InvariantDerived(private val companion: cats.Invariant.type) {
    def derived[F[_]]: cats.Invariant[F] =
      macro internal.compiletime.InvariantMacros.deriveInvariantImpl[F]
  }

  implicit class PureDerived(private val companion: alleycats.Pure.type) {
    def derived[F[_]]: alleycats.Pure[F] = macro internal.compiletime.PureMacros.derivePureImpl[F]
  }

  implicit class EmptyKDerived(private val companion: alleycats.EmptyK.type) {
    def derived[F[_]]: alleycats.EmptyK[F] = macro internal.compiletime.EmptyKMacros.deriveEmptyKImpl[F]
  }

  implicit class SemigroupKDerived(private val companion: cats.SemigroupK.type) {
    def derived[F[_]]: cats.SemigroupK[F] = macro internal.compiletime.SemigroupKMacros.deriveSemigroupKImpl[F]
  }

  implicit class MonoidKDerived(private val companion: cats.MonoidK.type) {
    def derived[F[_]]: cats.MonoidK[F] = macro internal.compiletime.MonoidKMacros.deriveMonoidKImpl[F]
  }

  implicit class ApplyDerived(private val companion: cats.Apply.type) {
    def derived[F[_]]: cats.Apply[F] = macro internal.compiletime.ApplyMacros.deriveApplyImpl[F]
  }

  implicit class ApplicativeDerived(private val companion: cats.Applicative.type) {
    def derived[F[_]]: cats.Applicative[F] =
      macro internal.compiletime.ApplicativeMacros.deriveApplicativeImpl[F]
  }

  implicit class FoldableDerived(private val companion: cats.Foldable.type) {
    def derived[F[_]]: cats.Foldable[F] = macro internal.compiletime.FoldableMacros.deriveFoldableImpl[F]
  }

  implicit class TraverseDerived(private val companion: cats.Traverse.type) {
    def derived[F[_]]: cats.Traverse[F] = macro internal.compiletime.TraverseMacros.deriveTraverseImpl[F]
  }

  implicit class ReducibleDerived(private val companion: cats.Reducible.type) {
    def derived[F[_]]: cats.Reducible[F] = macro internal.compiletime.ReducibleMacros.deriveReducibleImpl[F]
  }

  implicit class NonEmptyTraverseDerived(private val companion: cats.NonEmptyTraverse.type) {
    def derived[F[_]]: cats.NonEmptyTraverse[F] =
      macro internal.compiletime.NonEmptyTraverseMacros.deriveNonEmptyTraverseImpl[F]
  }

  implicit class NonEmptyAlternativeDerived(private val companion: cats.NonEmptyAlternative.type) {
    def derived[F[_]]: cats.NonEmptyAlternative[F] =
      macro internal.compiletime.NonEmptyAlternativeMacros.deriveNonEmptyAlternativeImpl[F]
  }

  implicit class AlternativeDerived(private val companion: cats.Alternative.type) {
    def derived[F[_]]: cats.Alternative[F] =
      macro internal.compiletime.AlternativeMacros.deriveAlternativeImpl[F]
  }

  implicit class ConsKDerived(private val companion: alleycats.ConsK.type) {
    def derived[F[_]]: alleycats.ConsK[F] = macro internal.compiletime.ConsKMacros.deriveConsKImpl[F]
  }
}

object extensions extends CatsDerivationScala2Extensions
