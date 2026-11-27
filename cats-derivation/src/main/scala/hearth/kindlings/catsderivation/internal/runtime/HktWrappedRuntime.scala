package hearth.kindlings.catsderivation.internal.runtime

import alleycats.{Empty, Pure}
import cats.{
  Alternative,
  Applicative,
  Apply,
  Eval,
  Foldable,
  NonEmptyAlternative,
  NonEmptyTraverse,
  Reducible,
  Traverse
}
import cats.kernel.{Monoid, Semigroup}

/** Instances for case classes `F[A]` whose fields wrap the type parameter (`tail: List[A]`, `children: List[Tree[A]]`,
  * `opt: Option[Vector[A]]`), built - like kittens - field by field from one instance per field:
  *   - a field of type `A` uses cats' `Id` instances,
  *   - a constant field behaves like `cats.data.Const` (combined with its `Semigroup`, `Monoid` or `Empty`),
  *   - a field `G[A]` uses `G`'s instance, a field `G[H[A]]` the composed instance, and a field mentioning `F` itself
  *     the instance being defined (passed to `fieldInstances` as `self`).
  *
  * The macro supplies the field accessors (`fields`) and the constructor (`construct`) of `F[Any]`, and the field
  * instances (erased to `AnyF`) through `fieldInstances`, evaluated once on first use.
  */
object HktWrappedRuntime {

  type AnyF[X] = Any

  // --- Field instances ---

  def idApplicative: Any = cats.catsInstancesForId
  def idPure: Any = Pure.applicativeIsPure[cats.Id](cats.catsInstancesForId)

  def constApply(semigroup: Any): Any = {
    val sg = semigroup.asInstanceOf[Semigroup[Any]]
    new Apply[AnyF] {
      def map[A, B](fa: Any)(f: A => B): Any = fa
      def ap[A, B](ff: Any)(fa: Any): Any = sg.combine(ff, fa)
    }
  }

  def constApplicative(monoid: Any): Any = {
    val m = monoid.asInstanceOf[Monoid[Any]]
    new Applicative[AnyF] {
      def pure[A](x: A): Any = m.empty
      def ap[A, B](ff: Any)(fa: Any): Any = m.combine(ff, fa)
      override def map[A, B](fa: Any)(f: A => B): Any = fa
    }
  }

  def constPure(empty: Any): Any = {
    val e = empty.asInstanceOf[Empty[Any]]
    new Pure[AnyF] { def pure[A](a: A): Any = e.empty }
  }

  def constNonEmptyAlternative(monoid: Any): Any = {
    val m = monoid.asInstanceOf[Monoid[Any]]
    new Alternative[AnyF] {
      def pure[A](x: A): Any = m.empty
      def ap[A, B](ff: Any)(fa: Any): Any = m.combine(ff, fa)
      override def map[A, B](fa: Any)(f: A => B): Any = fa
      def combineK[A](x: Any, y: Any): Any = m.combine(x, y)
      def empty[A]: Any = m.empty
    }
  }

  val constTraverse: Any = new Traverse[AnyF] {
    def traverse[G[_], A, B](fa: Any)(f: A => G[B])(implicit G: Applicative[G]): G[Any] = G.pure(fa)
    def foldLeft[A, B](fa: Any, b: B)(f: (B, A) => B): B = b
    def foldRight[A, B](fa: Any, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    override def map[A, B](fa: Any)(f: A => B): Any = fa
  }

  // --- Composition (`G[H[A]]`) ---

  def composeApply(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Apply[AnyF]].compose(inner.asInstanceOf[Apply[AnyF]])

  def composeApplicative(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Applicative[AnyF]].compose(inner.asInstanceOf[Applicative[AnyF]])

  def composePure(outer: Any, inner: Any): Any = {
    lazy val o = outer.asInstanceOf[Pure[AnyF]]
    lazy val i = inner.asInstanceOf[Pure[AnyF]]
    new Pure[AnyF] { def pure[A](a: A): Any = o.pure(i.pure(a)) }
  }

  def composeReducible(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Reducible[AnyF]].compose(inner.asInstanceOf[Reducible[AnyF]])

  def composeNonEmptyTraverse(outer: Any, inner: Any): Any =
    outer.asInstanceOf[NonEmptyTraverse[AnyF]].compose(inner.asInstanceOf[NonEmptyTraverse[AnyF]])

  /** `G[H[A]]` with `G: NonEmptyAlternative` and `H: Applicative`, like kittens. */
  def composeNonEmptyAlternative(outer: Any, inner: Any): Any =
    outer.asInstanceOf[NonEmptyAlternative[AnyF]].compose(inner.asInstanceOf[Applicative[AnyF]])

  /** `G[H[A]]` with `G: Alternative` and `H: Applicative`. */
  def composeAlternative(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Alternative[AnyF]].compose(inner.asInstanceOf[Applicative[AnyF]])

  // --- Derived instances ---

  /** Field accessors and constructor of `F[Any]`, plus the field instances (given the instance being defined). */
  final class Shape(
      val fields: Any => Array[Any],
      val construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any],
      self: Any
  ) extends Serializable { // the derived instances are Serializable, like cats' (kittens checks it)
    lazy val instances: Array[Any] = fieldInstances(self)
    def mapFields(fa: Any)(f: (Any, Int) => Any): Any = {
      val values = fields(fa)
      val result = new Array[Any](values.length)
      var i = 0
      while (i < values.length) { result(i) = f(values(i), i); i += 1 }
      construct(result)
    }
    def zipFields(x: Any, y: Any)(f: (Any, Any, Int) => Any): Any = {
      val xs = fields(x)
      val ys = fields(y)
      val result = new Array[Any](xs.length)
      var i = 0
      while (i < xs.length) { result(i) = f(xs(i), ys(i), i); i += 1 }
      construct(result)
    }
    def fill(f: Int => Any): Any = construct(Array.tabulate[Any](instances.length)(f))
  }

  private trait ApplyOps[F[_]] extends Apply[F] {
    protected def shape: Shape
    private def inst(i: Int): Apply[AnyF] = shape.instances(i).asInstanceOf[Apply[AnyF]]
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      shape.mapFields(fa)((v, i) => inst(i).map(v)(f)).asInstanceOf[F[B]]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      shape.zipFields(ff, fa)((f, a, i) => inst(i).ap[A, B](f)(a)).asInstanceOf[F[B]]
  }

  def applyInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): Apply[F] = new ApplyOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
  }

  def applicativeInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): Applicative[F] = new Applicative[F] with ApplyOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
    def pure[A](x: A): F[A] =
      shape.fill(i => shape.instances(i).asInstanceOf[Applicative[AnyF]].pure(x)).asInstanceOf[F[A]]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = super[ApplyOps].map(fa)(f)
  }

  def pureInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): Pure[F] = new Pure[F] { self =>
    private lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
    def pure[A](a: A): F[A] = shape.fill(i => shape.instances(i).asInstanceOf[Pure[AnyF]].pure(a)).asInstanceOf[F[A]]
  }

  private trait NonEmptyAlternativeOps[F[_]] extends NonEmptyAlternative[F] with ApplyOps[F] {
    private def inst(i: Int): NonEmptyAlternative[AnyF] = shape.instances(i).asInstanceOf[NonEmptyAlternative[AnyF]]
    def pure[A](x: A): F[A] = shape.fill(i => inst(i).pure(x)).asInstanceOf[F[A]]
    def combineK[A](x: F[A], y: F[A]): F[A] =
      shape.zipFields(x, y)((a, b, i) => inst(i).combineK[A](a, b)).asInstanceOf[F[A]]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = super[ApplyOps].map(fa)(f)
  }

  def nonEmptyAlternativeInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): NonEmptyAlternative[F] = new NonEmptyAlternativeOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
  }

  def alternativeInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): Alternative[F] = new Alternative[F] with NonEmptyAlternativeOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
    def empty[A]: F[A] = shape.fill(i => shape.instances(i).asInstanceOf[Alternative[AnyF]].empty[A]).asInstanceOf[F[A]]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = super[NonEmptyAlternativeOps].map(fa)(f)
    override def pure[A](x: A): F[A] = super[NonEmptyAlternativeOps].pure(x)
    override def combineK[A](x: F[A], y: F[A]): F[A] = super[NonEmptyAlternativeOps].combineK(x, y)
  }

  /** Foldable part, like kittens' product: `foldLeft` field by field, `foldRight` deferring every field. */
  private trait FoldableOps[F[_]] extends Foldable[F] {
    protected def shape: Shape
    private def inst(i: Int): Foldable[AnyF] = shape.instances(i).asInstanceOf[Foldable[AnyF]]
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
      val values = shape.fields(fa)
      var acc = b
      var i = 0
      while (i < values.length) { acc = inst(i).foldLeft(values(i), acc)(f); i += 1 }
      acc
    }
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val values = shape.fields(fa)
      def loop(i: Int): Eval[B] =
        if (i >= values.length) lb
        else Eval.defer(inst(i).foldRight(values(i), Eval.defer(loop(i + 1)))(f))
      loop(0)
    }
  }

  /** Reducible part, like kittens' product: the first field with an element starts the reduction. */
  private trait ReducibleOps[F[_]] extends Reducible[F] with FoldableOps[F] {
    private def inst(i: Int): Foldable[AnyF] = shape.instances(i).asInstanceOf[Foldable[AnyF]]
    def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
      val values = shape.fields(fa)
      var acc: Option[B] = None
      var i = 0
      while (i < values.length) {
        acc = acc match {
          case Some(b) => Some(inst(i).foldLeft(values(i), b)(g))
          case None    => inst(i).reduceLeftToOption[A, B](values(i))(f)(g)
        }
        i += 1
      }
      acc.get
    }
    def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val values = shape.fields(fa)
      def loop(i: Int): Eval[Option[B]] =
        if (i >= values.length) Eval.now(None)
        else
          loop(i + 1).flatMap {
            case Some(b) => inst(i).foldRight(values(i), Eval.now(b))(g).map(Some(_))
            case None    => inst(i).reduceRightToOption[A, B](values(i))(f)(g)
          }
      loop(0).map(_.get)
    }
  }

  def reducibleInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): Reducible[F] = new ReducibleOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
  }

  def nonEmptyTraverseInstance[F[_]](
      fields: Any => Array[Any],
      construct: Array[Any] => Any,
      fieldInstances: Any => Array[Any]
  ): NonEmptyTraverse[F] = new NonEmptyTraverse[F] with ReducibleOps[F] { self =>
    protected lazy val shape: Shape = new Shape(fields, construct, fieldInstances, self)
    private def inst(i: Int): Traverse[AnyF] = shape.instances(i).asInstanceOf[Traverse[AnyF]]

    // Like kittens' product: field by field, left to right, combined with `ap`.
    override def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = {
      val values = shape.fields(fa)
      var acc: G[List[Any]] = G.pure(Nil)
      var i = 0
      while (i < values.length) {
        val field: G[Any] = inst(i).traverse[G, A, B](values(i))(f).asInstanceOf[G[Any]]
        acc = G.ap(G.map(acc)(done => (value: Any) => value :: done))(field)
        i += 1
      }
      G.map(acc)(done => shape.construct(done.reverse.toArray).asInstanceOf[F[B]])
    }

    // Like kittens: `traverse` with an Applicative built from G's Apply (a field of type A always uses G).
    def nonEmptyTraverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G]): G[F[B]] = {
      type Alt[X] = Either[G[X], X]
      val altApplicative: Applicative[Alt] = new Applicative[Alt] {
        def pure[X](x: X): Alt[X] = Right(x)
        override def map[X, Y](fa: Alt[X])(f: X => Y): Alt[Y] = fa match {
          case Left(gx) => Left(G.map(gx)(f))
          case Right(x) => Right(f(x))
        }
        def ap[X, Y](ff: Alt[X => Y])(fa: Alt[X]): Alt[Y] = (ff, fa) match {
          case (Left(gf), Left(gx))  => Left(G.ap(gf)(gx))
          case (Left(gf), Right(x))  => Left(G.map(gf)(_(x)))
          case (Right(fn), Left(gx)) => Left(G.map(gx)(fn))
          case (Right(fn), Right(x)) => Right(fn(x))
        }
      }
      traverse[Alt, A, B](fa)(a => Left(f(a)))(altApplicative) match {
        case Left(value) => value
        case Right(_)    => throw new IllegalStateException("NonEmptyTraverse: no element was traversed")
      }
    }

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      shape.mapFields(fa)((v, i) => inst(i).map(v)(f)).asInstanceOf[F[B]]
  }
}
