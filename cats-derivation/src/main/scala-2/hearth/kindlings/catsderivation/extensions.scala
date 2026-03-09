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
}

object extensions extends CatsDerivationScala2Extensions
