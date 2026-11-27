package hearth.kindlings.catsderivation.internal.runtime

import alleycats.{Empty, EmptyK, Pure}
import cats.{
  Applicative,
  Apply,
  Contravariant,
  Eval,
  Foldable,
  Functor,
  Invariant,
  MonoidK,
  NonEmptyTraverse,
  Reducible,
  SemigroupK,
  Traverse
}
import cats.kernel.{Monoid, Semigroup}

/** Instances assembled - like kittens does - from one instance per part of a type `F[A]`, for the shapes the
  * specialised derivations do not generate code for:
  *   - a case class is combined field by field (`product*`),
  *   - a sealed trait / enum dispatches to the instance of the value's subtype (`coproduct*`),
  *   - a type nesting type constructors (`G[H[A]]`) composes their instances (`compose*`),
  *   - a part not mentioning `A` behaves like `cats.data.Const` (`const*`).
  *
  * Everything is erased to [[AnyF]]: the macro supplies the field accessors / constructor / subtype ordinal of `F[Any]`
  * and the part instances. Instances that refer to each other (recursive types) are looked up in a [[Registry]] when
  * the part instances are first needed, i.e. after all of them were created.
  */
object HktGenericRuntime {

  type AnyF[X] = Any

  /** Holds the derived instances of one expansion that may refer to each other (recursive data types). */
  final class Registry(size: Int) extends Serializable {
    private val slots = new Array[Any](size)
    def set(index: Int, instance: Any): Any = { slots(index) = instance; instance }
    def get(index: Int): Any = slots(index)
  }

  def recursive(size: Int, build: Registry => Any): Any = build(new Registry(size))

  /** Field accessors and constructor of a case class (erased), plus its field instances (evaluated on first use). */
  final class Fields(val values: Any => Array[Any], val construct: Array[Any] => Any, fieldInstances: Any => Array[Any])
      extends Serializable {
    lazy val instances: Array[Any] = fieldInstances(null)

    def map(fa: Any)(f: (Any, Int) => Any): Any = {
      val in = values(fa)
      val out = new Array[Any](in.length)
      var i = 0
      while (i < in.length) { out(i) = f(in(i), i); i += 1 }
      construct(out)
    }

    def zip(x: Any, y: Any)(f: (Any, Any, Int) => Any): Any = {
      val xs = values(x)
      val ys = values(y)
      val out = new Array[Any](xs.length)
      var i = 0
      while (i < xs.length) { out(i) = f(xs(i), ys(i), i); i += 1 }
      construct(out)
    }

    def fill(f: Int => Any): Any = {
      val out = new Array[Any](instances.length)
      var i = 0
      while (i < out.length) { out(i) = f(i); i += 1 }
      construct(out)
    }
  }

  /** The subtype ordinal of a sealed trait's value, plus the instances of its subtypes (evaluated on first use). */
  final class Variants(ordinal: Any => Int, variantInstances: Any => Array[Any]) extends Serializable {
    lazy val instances: Array[Any] = variantInstances(null)
    def of(fa: Any): Any = instances(ordinal(fa))
  }

  // --- Parts not mentioning the type parameter ---

  val constContravariant: Any = new Contravariant[AnyF] {
    def contramap[A, B](fa: Any)(f: B => A): Any = fa
  }

  def constSemigroupK(semigroup: Any): Any = {
    val sg = semigroup.asInstanceOf[Semigroup[Any]]
    new SemigroupK[AnyF] { def combineK[A](x: Any, y: Any): Any = sg.combine(x, y) }
  }

  def constMonoidK(monoid: Any): Any = {
    val m = monoid.asInstanceOf[Monoid[Any]]
    new MonoidK[AnyF] {
      def empty[A]: Any = m.empty
      def combineK[A](x: Any, y: Any): Any = m.combine(x, y)
    }
  }

  def constEmptyK(empty: Any): Any = {
    val e = empty.asInstanceOf[Empty[Any]]
    new EmptyK[AnyF] { def empty[A]: Any = e.empty }
  }

  /** `Pure` for a part whose type has a single value (a literal type, a case object). */
  def singletonPure(value: Any): Any = new Pure[AnyF] { def pure[A](a: A): Any = value }

  // --- cats' instances of partially applied binary type constructors ---

  def function1Contravariant: Any = cats.instances.function.catsStdContravariantForFunction1[Any]
  def function1Monad: Any = cats.instances.function.catsStdMonadForFunction1[Any]
  def eitherInstances: Any = cats.instances.either.catsStdInstancesForEither[Any]
  def eitherSemigroupK: Any = cats.instances.either.catsStdSemigroupKForEither[Any]
  def mapInstances: Any = cats.instances.map.catsStdInstancesForMap[Any]
  def mapMonoidK: Any = cats.instances.map.catsStdMonoidKForMap[Any]

  // --- Nested type constructors (`G[H[A]]`) ---

  /** `G[H[A]]` with contravariant `G` and `H` is a functor. */
  def composeContravariants(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Contravariant[AnyF]].compose(inner.asInstanceOf[Contravariant[AnyF]])

  def composeFunctorContravariant(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Functor[AnyF]].composeContravariant(inner.asInstanceOf[Contravariant[AnyF]])

  def composeInvariant(outer: Any, inner: Any): Any =
    outer.asInstanceOf[Invariant[AnyF]].compose(inner.asInstanceOf[Invariant[AnyF]])

  /** `G[H[A]]` with `G: Pure` and `H: EmptyK`. */
  def composePureEmptyK(outer: Any, inner: Any): Any = {
    val o = outer.asInstanceOf[Pure[AnyF]]
    val i = inner.asInstanceOf[EmptyK[AnyF]]
    new EmptyK[AnyF] { def empty[A]: Any = o.pure(i.empty[A]) }
  }

  /** `G[H[A]]` with `G: Apply` and `H: SemigroupK`. */
  def composeApplySemigroupK(outer: Any, inner: Any): Any = {
    val o = outer.asInstanceOf[Apply[AnyF]]
    val i = inner.asInstanceOf[SemigroupK[AnyF]]
    new SemigroupK[AnyF] {
      def combineK[A](x: Any, y: Any): Any = o.map2[Any, Any, Any](x, y)((a, b) => i.combineK[A](a, b))
    }
  }

  /** `G[H[A]]` with `G: Applicative` and `H: MonoidK`. */
  def composeApplicativeMonoidK(outer: Any, inner: Any): Any = {
    val o = outer.asInstanceOf[Applicative[AnyF]]
    val i = inner.asInstanceOf[MonoidK[AnyF]]
    new MonoidK[AnyF] {
      def empty[A]: Any = o.pure(i.empty[A])
      def combineK[A](x: Any, y: Any): Any = o.map2[Any, Any, Any](x, y)((a, b) => i.combineK[A](a, b))
    }
  }

  // --- Case classes ---

  def productFunctor(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new Functor[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def map[A, B](fa: Any)(f: A => B): Any =
        fields.map(fa)((v, i) => fields.instances(i).asInstanceOf[Functor[AnyF]].map[A, B](v)(f))
    }

  def productContravariant(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new Contravariant[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def contramap[A, B](fa: Any)(f: B => A): Any =
        fields.map(fa)((v, i) => fields.instances(i).asInstanceOf[Contravariant[AnyF]].contramap[A, B](v)(f))
    }

  def productInvariant(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new Invariant[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def imap[A, B](fa: Any)(f: A => B)(g: B => A): Any =
        fields.map(fa)((v, i) => fields.instances(i).asInstanceOf[Invariant[AnyF]].imap[A, B](v)(f)(g))
    }

  /** Like kittens' product: `foldLeft` field by field, `foldRight` deferring every field. */
  private trait ProductFoldable extends Foldable[AnyF] {
    protected def fields: Fields
    private def inst(i: Int): Foldable[AnyF] = fields.instances(i).asInstanceOf[Foldable[AnyF]]
    def foldLeft[A, B](fa: Any, b: B)(f: (B, A) => B): B = {
      val values = fields.values(fa)
      var acc = b
      var i = 0
      while (i < values.length) { acc = inst(i).foldLeft(values(i), acc)(f); i += 1 }
      acc
    }
    def foldRight[A, B](fa: Any, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val values = fields.values(fa)
      def loop(i: Int): Eval[B] =
        if (i >= values.length) lb
        else Eval.defer(inst(i).foldRight(values(i), Eval.defer(loop(i + 1)))(f))
      loop(0)
    }
  }

  def productFoldable(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new ProductFoldable {
      protected val fields: Fields = new Fields(values, construct, instances)
    }

  def productTraverse(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new Traverse[AnyF] with ProductFoldable {
      protected val fields: Fields = new Fields(values, construct, instances)
      private def inst(i: Int): Traverse[AnyF] = fields.instances(i).asInstanceOf[Traverse[AnyF]]

      // Like kittens' product: field by field, left to right, combined with `ap`.
      def traverse[G[_], A, B](fa: Any)(f: A => G[B])(implicit G: Applicative[G]): G[Any] = {
        val values = fields.values(fa)
        var acc: G[List[Any]] = G.pure(Nil)
        var i = 0
        while (i < values.length) {
          val field: G[Any] = inst(i).traverse[G, A, B](values(i))(f)
          acc = G.ap(G.map(acc)(done => (value: Any) => value :: done))(field)
          i += 1
        }
        G.map(acc)(done => fields.construct(done.reverse.toArray))
      }

      override def map[A, B](fa: Any)(f: A => B): Any =
        fields.map(fa)((v, i) => inst(i).map[A, B](v)(f))
    }

  def productSemigroupK(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new SemigroupK[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def combineK[A](x: Any, y: Any): Any =
        fields.zip(x, y)((a, b, i) => fields.instances(i).asInstanceOf[SemigroupK[AnyF]].combineK[A](a, b))
    }

  def productMonoidK(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new MonoidK[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def empty[A]: Any = fields.fill(i => fields.instances(i).asInstanceOf[MonoidK[AnyF]].empty[A])
      def combineK[A](x: Any, y: Any): Any =
        fields.zip(x, y)((a, b, i) => fields.instances(i).asInstanceOf[MonoidK[AnyF]].combineK[A](a, b))
    }

  def productEmptyK(values: Any => Array[Any], construct: Array[Any] => Any, instances: Any => Array[Any]): Any =
    new EmptyK[AnyF] {
      private val fields = new Fields(values, construct, instances)
      def empty[A]: Any = fields.fill(i => fields.instances(i).asInstanceOf[EmptyK[AnyF]].empty[A])
    }

  // --- Sealed traits and enums ---

  def coproductFunctor(ordinal: Any => Int, instances: Any => Array[Any]): Any = new Functor[AnyF] {
    private val variants = new Variants(ordinal, instances)
    def map[A, B](fa: Any)(f: A => B): Any = variants.of(fa).asInstanceOf[Functor[AnyF]].map[A, B](fa)(f)
  }

  def coproductContravariant(ordinal: Any => Int, instances: Any => Array[Any]): Any = new Contravariant[AnyF] {
    private val variants = new Variants(ordinal, instances)
    def contramap[A, B](fa: Any)(f: B => A): Any =
      variants.of(fa).asInstanceOf[Contravariant[AnyF]].contramap[A, B](fa)(f)
  }

  def coproductInvariant(ordinal: Any => Int, instances: Any => Array[Any]): Any = new Invariant[AnyF] {
    private val variants = new Variants(ordinal, instances)
    def imap[A, B](fa: Any)(f: A => B)(g: B => A): Any =
      variants.of(fa).asInstanceOf[Invariant[AnyF]].imap[A, B](fa)(f)(g)
  }

  /** Like kittens' coproduct: the subtype's instance, `foldRight` deferred. */
  private trait CoproductFoldable extends Foldable[AnyF] {
    protected def variants: Variants
    def foldLeft[A, B](fa: Any, b: B)(f: (B, A) => B): B =
      variants.of(fa).asInstanceOf[Foldable[AnyF]].foldLeft(fa, b)(f)
    def foldRight[A, B](fa: Any, lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Eval.defer(variants.of(fa).asInstanceOf[Foldable[AnyF]].foldRight(fa, lb)(f))
  }

  def coproductFoldable(ordinal: Any => Int, instances: Any => Array[Any]): Any = new CoproductFoldable {
    protected val variants: Variants = new Variants(ordinal, instances)
  }

  private trait CoproductTraverse extends Traverse[AnyF] with CoproductFoldable {
    def traverse[G[_], A, B](fa: Any)(f: A => G[B])(implicit G: Applicative[G]): G[Any] =
      variants.of(fa).asInstanceOf[Traverse[AnyF]].traverse[G, A, B](fa)(f)
    override def map[A, B](fa: Any)(f: A => B): Any =
      variants.of(fa).asInstanceOf[Functor[AnyF]].map[A, B](fa)(f)
  }

  def coproductTraverse(ordinal: Any => Int, instances: Any => Array[Any]): Any = new CoproductTraverse {
    protected val variants: Variants = new Variants(ordinal, instances)
  }

  private trait CoproductReducible extends Reducible[AnyF] with CoproductFoldable {
    def reduceLeftTo[A, B](fa: Any)(f: A => B)(g: (B, A) => B): B =
      variants.of(fa).asInstanceOf[Reducible[AnyF]].reduceLeftTo(fa)(f)(g)
    def reduceRightTo[A, B](fa: Any)(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      Eval.defer(variants.of(fa).asInstanceOf[Reducible[AnyF]].reduceRightTo(fa)(f)(g))
  }

  def coproductReducible(ordinal: Any => Int, instances: Any => Array[Any]): Any = new CoproductReducible {
    protected val variants: Variants = new Variants(ordinal, instances)
  }

  def coproductNonEmptyTraverse(ordinal: Any => Int, instances: Any => Array[Any]): Any =
    new NonEmptyTraverse[AnyF] with CoproductReducible with CoproductTraverse {
      protected val variants: Variants = new Variants(ordinal, instances)
      def nonEmptyTraverse[G[_], A, B](fa: Any)(f: A => G[B])(implicit G: Apply[G]): G[Any] =
        variants.of(fa).asInstanceOf[NonEmptyTraverse[AnyF]].nonEmptyTraverse[G, A, B](fa)(f)
      override def traverse[G[_], A, B](fa: Any)(f: A => G[B])(implicit G: Applicative[G]): G[Any] =
        super[CoproductTraverse].traverse[G, A, B](fa)(f)
      override def map[A, B](fa: Any)(f: A => B): Any = super[CoproductTraverse].map[A, B](fa)(f)
    }

  // --- alleycats.ConsK for list-like sealed traits ---

  def consK(consFn: (Any, Any) => Any): Any = new alleycats.ConsK[AnyF] {
    def cons[A](hd: A, tl: Any): Any = consFn(hd, tl)
  }
}
