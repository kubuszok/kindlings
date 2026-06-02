package hearth.kindlings.catsderivation

import hearth.kindlings.catsderivation.extensions.*

object auto {

  object show {
    inline given [A]: cats.Show[A] = cats.Show.derived[A]
  }

  object eq {
    inline given [A]: cats.kernel.Eq[A] = cats.kernel.Eq.derived[A]
  }

  object order {
    inline given [A]: cats.kernel.Order[A] = cats.kernel.Order.derived[A]
  }

  object partialOrder {
    inline given [A]: cats.kernel.PartialOrder[A] = cats.kernel.PartialOrder.derived[A]
  }

  object hash {
    inline given [A]: cats.kernel.Hash[A] = cats.kernel.Hash.derived[A]
  }

  object semigroup {
    inline given [A]: cats.kernel.Semigroup[A] = cats.kernel.Semigroup.derived[A]
  }

  object monoid {
    inline given [A]: cats.kernel.Monoid[A] = cats.kernel.Monoid.derived[A]
  }

  object empty {
    inline given [A]: alleycats.Empty[A] = alleycats.Empty.derived[A]
  }

  object functor {
    inline given [F[_]]: cats.Functor[F] = cats.Functor.derived[F]
  }

  object contravariant {
    inline given [F[_]]: cats.Contravariant[F] = cats.Contravariant.derived[F]
  }

  object foldable {
    inline given [F[_]]: cats.Foldable[F] = cats.Foldable.derived[F]
  }

  object traverse {
    inline given [F[_]]: cats.Traverse[F] = cats.Traverse.derived[F]
  }
}
