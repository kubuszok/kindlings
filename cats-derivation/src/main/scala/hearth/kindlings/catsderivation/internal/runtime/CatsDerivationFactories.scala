package hearth.kindlings.catsderivation.internal.runtime

import cats.Eval

object CatsDerivationFactories {

  sealed trait W1
  sealed trait W2

  /** Kind-correct alias for erasing higher-kinded type parameters (e.g., G[_] in Traverse). */
  type AnyF[A] = Any

  // --- Monomorphic (kind *) ---

  def showInstance[A](showFn: A => String): cats.Show[A] = new cats.Show[A] {
    def show(a: A): String = showFn(a)
  }

  def eqInstance[A](eqvFn: (A, A) => Boolean): cats.kernel.Eq[A] = new cats.kernel.Eq[A] {
    def eqv(x: A, y: A): Boolean = eqvFn(x, y)
  }

  def partialOrderInstance[A](partialCompareFn: (A, A) => Double): cats.kernel.PartialOrder[A] =
    new cats.kernel.PartialOrder[A] {
      def partialCompare(x: A, y: A): Double = partialCompareFn(x, y)
    }

  def orderInstance[A](compareFn: (A, A) => Int): cats.kernel.Order[A] = new cats.kernel.Order[A] {
    def compare(x: A, y: A): Int = compareFn(x, y)
  }

  def hashInstance[A](hashFn: A => Int, eqvFn: (A, A) => Boolean): cats.kernel.Hash[A] =
    new cats.kernel.Hash[A] {
      def hash(x: A): Int = hashFn(x)
      def eqv(x: A, y: A): Boolean = eqvFn(x, y)
    }

  def semigroupInstance[A](combineFn: (A, A) => A): cats.kernel.Semigroup[A] =
    new cats.kernel.Semigroup[A] {
      def combine(x: A, y: A): A = combineFn(x, y)
    }

  def commutativeSemigroupInstance[A](combineFn: (A, A) => A): cats.kernel.CommutativeSemigroup[A] =
    new cats.kernel.CommutativeSemigroup[A] {
      def combine(x: A, y: A): A = combineFn(x, y)
    }

  def monoidInstance[A](emptyVal: A, combineFn: (A, A) => A): cats.kernel.Monoid[A] =
    new cats.kernel.Monoid[A] {
      def empty: A = emptyVal
      def combine(x: A, y: A): A = combineFn(x, y)
    }

  def commutativeMonoidInstance[A](emptyVal: A, combineFn: (A, A) => A): cats.kernel.CommutativeMonoid[A] =
    new cats.kernel.CommutativeMonoid[A] {
      def empty: A = emptyVal
      def combine(x: A, y: A): A = combineFn(x, y)
    }

  def emptyInstance[A](emptyVal: A): alleycats.Empty[A] = new alleycats.Empty[A] {
    lazy val empty: A = emptyVal
  }

  // --- Polymorphic (kind * -> *), erasure-based ---
  //
  // IMPORTANT: W1/W2 must NEVER appear as direct lambda parameter types (only as type
  // arguments to other types like F[W1], W1 => W2, etc.). This is because on Scala 3,
  // lambda bridge methods include checkcast instructions for sealed trait parameter types,
  // which fail at runtime when the actual argument is e.g. Integer instead of W1.
  // Use Any for direct-value positions and cast inside the factory body.

  def functorInstance[F[_]](mapFn: (F[W1], W1 => W2) => F[W2]): cats.Functor[F] =
    new cats.Functor[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] =
        mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
    }

  def contravariantInstance[F[_]](contramapFn: (F[W1], W2 => W1) => F[W2]): cats.Contravariant[F] =
    new cats.Contravariant[F] {
      def contramap[A, B](fa: F[A])(f: B => A): F[B] =
        contramapFn.asInstanceOf[(F[A], B => A) => F[B]].apply(fa, f)
    }

  def invariantInstance[F[_]](imapFn: (F[W1], W1 => W2, W2 => W1) => F[W2]): cats.Invariant[F] =
    new cats.Invariant[F] {
      def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
        imapFn.asInstanceOf[(F[A], A => B, B => A) => F[B]].apply(fa, f, g)
    }

  def applyInstance[F[_]](
      mapFn: (F[W1], W1 => W2) => F[W2],
      apFn: (F[W1 => W2], F[W1]) => F[W2]
  ): cats.Apply[F] = new cats.Apply[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      apFn.asInstanceOf[(F[A => B], F[A]) => F[B]].apply(ff, fa)
  }

  def applicativeInstance[F[_]](
      pureFn: Any => F[W1],
      mapFn: (F[W1], W1 => W2) => F[W2],
      apFn: (F[W1 => W2], F[W1]) => F[W2]
  ): cats.Applicative[F] = new cats.Applicative[F] {
    def pure[A](a: A): F[A] = pureFn.asInstanceOf[Any => F[A]].apply(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      apFn.asInstanceOf[(F[A => B], F[A]) => F[B]].apply(ff, fa)
  }

  def foldableInstance[F[_]](
      foldLeftFn: (F[W1], Any, (Any, Any) => Any) => Any,
      foldRightFn: (F[W1], Eval[Any], (Any, Eval[Any]) => Eval[Any]) => Eval[Any]
  ): cats.Foldable[F] = new cats.Foldable[F] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      foldLeftFn.asInstanceOf[(F[A], B, (B, A) => B) => B].apply(fa, b, f)
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldRightFn.asInstanceOf[(F[A], Eval[B], (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, lb, f)
  }

  def traverseInstance[F[_]](
      traverseFn: (F[W1], Any, Any) => Any,
      foldLeftFn: (F[W1], Any, (Any, Any) => Any) => Any,
      foldRightFn: (F[W1], Eval[Any], (Any, Eval[Any]) => Eval[Any]) => Eval[Any]
  ): cats.Traverse[F] = new cats.Traverse[F] {
    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: cats.Applicative[G]): G[F[B]] =
      traverseFn.asInstanceOf[(F[A], A => G[B], cats.Applicative[G]) => G[F[B]]].apply(fa, f, G)
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      foldLeftFn.asInstanceOf[(F[A], B, (B, A) => B) => B].apply(fa, b, f)
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldRightFn.asInstanceOf[(F[A], Eval[B], (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, lb, f)
  }

  def reducibleInstance[F[_]](
      reduceLeftToFn: (F[W1], Any, Any) => Any,
      reduceRightToFn: (F[W1], Any, Any) => Any,
      foldLeftFn: (F[W1], Any, (Any, Any) => Any) => Any,
      foldRightFn: (F[W1], Eval[Any], (Any, Eval[Any]) => Eval[Any]) => Eval[Any]
  ): cats.Reducible[F] = new cats.Reducible[F] {
    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      reduceLeftToFn.asInstanceOf[(F[A], A => B, (B, A) => B) => B].apply(fa, f, g)
    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      reduceRightToFn.asInstanceOf[(F[A], A => B, (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, f, g)
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      foldLeftFn.asInstanceOf[(F[A], B, (B, A) => B) => B].apply(fa, b, f)
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldRightFn.asInstanceOf[(F[A], Eval[B], (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, lb, f)
  }

  def nonEmptyTraverseInstance[F[_]](
      nonEmptyTraverseFn: (F[W1], Any, Any) => Any,
      reduceLeftToFn: (F[W1], Any, Any) => Any,
      reduceRightToFn: (F[W1], Any, Any) => Any,
      foldLeftFn: (F[W1], Any, (Any, Any) => Any) => Any,
      foldRightFn: (F[W1], Eval[Any], (Any, Eval[Any]) => Eval[Any]) => Eval[Any]
  ): cats.NonEmptyTraverse[F] = new cats.NonEmptyTraverse[F] {
    def nonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: cats.Apply[G]): G[F[B]] =
      nonEmptyTraverseFn.asInstanceOf[(F[A], A => G[B], cats.Apply[G]) => G[F[B]]].apply(fa, f, G)
    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      reduceLeftToFn.asInstanceOf[(F[A], A => B, (B, A) => B) => B].apply(fa, f, g)
    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      reduceRightToFn.asInstanceOf[(F[A], A => B, (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, f, g)
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      foldLeftFn.asInstanceOf[(F[A], B, (B, A) => B) => B].apply(fa, b, f)
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldRightFn.asInstanceOf[(F[A], Eval[B], (A, Eval[B]) => Eval[B]) => Eval[B]].apply(fa, lb, f)
  }

  def semigroupKInstance[F[_]](combineKFn: (F[W1], F[W1]) => F[W1]): cats.SemigroupK[F] =
    new cats.SemigroupK[F] {
      def combineK[A](x: F[A], y: F[A]): F[A] =
        combineKFn.asInstanceOf[(F[A], F[A]) => F[A]].apply(x, y)
    }

  def monoidKInstance[F[_]](
      emptyKFn: () => F[W1],
      combineKFn: (F[W1], F[W1]) => F[W1]
  ): cats.MonoidK[F] = new cats.MonoidK[F] {
    def empty[A]: F[A] = emptyKFn.asInstanceOf[() => F[A]].apply()
    def combineK[A](x: F[A], y: F[A]): F[A] =
      combineKFn.asInstanceOf[(F[A], F[A]) => F[A]].apply(x, y)
  }

  def pureInstance[F[_]](pureFn: Any => F[W1]): alleycats.Pure[F] = new alleycats.Pure[F] {
    def pure[A](a: A): F[A] = pureFn.asInstanceOf[Any => F[A]].apply(a)
  }

  def emptyKInstance[F[_]](emptyKFn: () => F[W1]): alleycats.EmptyK[F] = new alleycats.EmptyK[F] {
    def empty[A]: F[A] = emptyKFn.asInstanceOf[() => F[A]].apply()
  }

  def nonEmptyAlternativeInstance[F[_]](
      pureFn: Any => F[W1],
      mapFn: (F[W1], W1 => W2) => F[W2],
      apFn: (F[W1 => W2], F[W1]) => F[W2],
      combineKFn: (F[W1], F[W1]) => F[W1]
  ): cats.NonEmptyAlternative[F] = new cats.NonEmptyAlternative[F] {
    def pure[A](a: A): F[A] = pureFn.asInstanceOf[Any => F[A]].apply(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      apFn.asInstanceOf[(F[A => B], F[A]) => F[B]].apply(ff, fa)
    def combineK[A](x: F[A], y: F[A]): F[A] =
      combineKFn.asInstanceOf[(F[A], F[A]) => F[A]].apply(x, y)
  }

  def alternativeInstance[F[_]](
      pureFn: Any => F[W1],
      mapFn: (F[W1], W1 => W2) => F[W2],
      apFn: (F[W1 => W2], F[W1]) => F[W2],
      emptyKFn: () => F[W1],
      combineKFn: (F[W1], F[W1]) => F[W1]
  ): cats.Alternative[F] = new cats.Alternative[F] {
    def pure[A](a: A): F[A] = pureFn.asInstanceOf[Any => F[A]].apply(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      mapFn.asInstanceOf[(F[A], A => B) => F[B]].apply(fa, f)
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      apFn.asInstanceOf[(F[A => B], F[A]) => F[B]].apply(ff, fa)
    def empty[A]: F[A] = emptyKFn.asInstanceOf[() => F[A]].apply()
    def combineK[A](x: F[A], y: F[A]): F[A] =
      combineKFn.asInstanceOf[(F[A], F[A]) => F[A]].apply(x, y)
  }

  def consKInstance[F[_]](consFn: (Any, F[W1]) => F[W1]): alleycats.ConsK[F] =
    new alleycats.ConsK[F] {
      def cons[A](hd: A, tl: F[A]): F[A] =
        consFn.asInstanceOf[(Any, F[A]) => F[A]].apply(hd, tl)
    }
}
