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
}
