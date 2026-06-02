package hearth.kindlings.catsderivation

import scala.language.experimental.macros

object auto {

  object show {
    implicit def deriveShow[A]: cats.Show[A] = macro internal.compiletime.ShowMacros.deriveShowImpl[A]
  }

  object eq {
    implicit def deriveEq[A]: cats.kernel.Eq[A] = macro internal.compiletime.EqMacros.deriveEqImpl[A]
  }

  object order {
    implicit def deriveOrder[A]: cats.kernel.Order[A] = macro internal.compiletime.OrderMacros.deriveOrderImpl[A]
  }

  object partialOrder {
    implicit def derivePartialOrder[A]: cats.kernel.PartialOrder[A] =
      macro internal.compiletime.PartialOrderMacros.derivePartialOrderImpl[A]
  }

  object hash {
    implicit def deriveHash[A]: cats.kernel.Hash[A] = macro internal.compiletime.HashMacros.deriveHashImpl[A]
  }

  object semigroup {
    implicit def deriveSemigroup[A]: cats.kernel.Semigroup[A] =
      macro internal.compiletime.SemigroupMacros.deriveSemigroupImpl[A]
  }

  object monoid {
    implicit def deriveMonoid[A]: cats.kernel.Monoid[A] = macro internal.compiletime.MonoidMacros.deriveMonoidImpl[A]
  }

  object empty {
    implicit def deriveEmpty[A]: alleycats.Empty[A] = macro internal.compiletime.EmptyMacros.deriveEmptyImpl[A]
  }
}
