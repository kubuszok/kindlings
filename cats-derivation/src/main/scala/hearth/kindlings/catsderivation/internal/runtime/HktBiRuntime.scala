package hearth.kindlings.catsderivation.internal.runtime

import cats.{Applicative, Bitraverse, Eval, Foldable, Functor, Traverse}

/** `Bifunctor` / `Bifoldable` / `Bitraverse` instances assembled - like kittens does - from one instance per part of a
  * type `F[A, B]`, for the shapes the specialised derivations do not generate code for (fields nesting the type
  * parameters in other type constructors, recursive types):
  *   - `A` / `B` themselves ([[left]] / [[right]]),
  *   - a part mentioning neither ([[const]]),
  *   - `G[X]` with an implicit `Functor` / `Foldable` / `Traverse` of `G` ([[map1]], `X` being any part),
  *   - `G[X, Y]` with an implicit `Bifunctor` / `Bifoldable` / `Bitraverse` of `G` ([[map2]]),
  *   - a case class, field by field ([[product]]), a sealed trait / enum, by subtype ([[coproduct]]).
  *
  * Every node implements all of `Bitraverse` (hence `Bifunctor` and `Bifoldable`), each method only using what the
  * derived type class needs: a derived `Bifunctor` only ever calls `bimap`, whose nodes hold `Functor`s / `Bifunctor`s.
  * Everything is erased to [[AnyF2]]; instances that refer to each other (recursive types) go through
  * [[HktGenericRuntime.Registry]] and are resolved on first use.
  */
object HktBiRuntime {

  type AnyF2[X, Y] = Any
  private type AnyF[X] = Any

  abstract class Node extends Bitraverse[AnyF2] with Serializable {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any]
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any

    final override def bimap[A, B, C, D](fab: Any)(f: A => C, g: B => D): Any =
      bimapAny(fab, f.asInstanceOf[Any => Any], g.asInstanceOf[Any => Any])
    final def bifoldLeft[A, B, C](fab: Any, c: C)(f: (C, A) => C, g: (C, B) => C): C =
      bifoldLeftAny(fab, c, f.asInstanceOf[(Any, Any) => Any], g.asInstanceOf[(Any, Any) => Any]).asInstanceOf[C]
    final def bifoldRight[A, B, C](
        fab: Any,
        c: Eval[C]
    )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      bifoldRightAny(
        fab,
        c.asInstanceOf[Eval[Any]],
        f.asInstanceOf[(Any, Eval[Any]) => Eval[Any]],
        g.asInstanceOf[(Any, Eval[Any]) => Eval[Any]]
      ).asInstanceOf[Eval[C]]
    final def bitraverse[G[_], A, B, C, D](fab: Any)(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Any] =
      bitraverseAny(fab, f.asInstanceOf[Any => Any], g.asInstanceOf[Any => Any], G.asInstanceOf[Applicative[AnyF]])
        .asInstanceOf[G[Any]]
  }

  private def node(instance: Any): Node = instance.asInstanceOf[Node]

  private val leftNode: Node = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = f(fab)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = f(c, fab)
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = f(fab, lc)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = f(fab)
  }

  private val rightNode: Node = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = g(fab)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = g(c, fab)
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = g(fab, lc)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = g(fab)
  }

  private val constNode: Node = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = fab
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = c
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = lc
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = G.pure(fab)
  }

  /** The part `A`. */
  val left: Any = leftNode

  /** The part `B`. */
  val right: Any = rightNode

  /** A part mentioning neither type parameter (like `cats.data.Const`). */
  val const: Any = constNode

  private def kindOf(n: Node): Int =
    if (n eq leftNode) KindLeft else if (n eq rightNode) KindRight else if (n eq constNode) KindConst else KindOther

  /** `G[X]`, where `outer` is the `Functor` / `Foldable` / `Traverse` of `G` and `inner` the instance of `X`. `G[A]` /
    * `G[B]` call `outer` with the function itself (no wrapping lambda).
    */
  def map1(outer: Any, inner: Any): Any = new Node {
    private lazy val in = node(inner)
    private lazy val k = kindOf(in)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = {
      val F = outer.asInstanceOf[Functor[AnyF]]
      (k: @scala.annotation.switch) match {
        case KindLeft  => F.map[Any, Any](fab)(f)
        case KindRight => F.map[Any, Any](fab)(g)
        case _         => F.map[Any, Any](fab)(x => in.bimapAny(x, f, g))
      }
    }
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = {
      val F = outer.asInstanceOf[Foldable[AnyF]]
      (k: @scala.annotation.switch) match {
        case KindLeft  => F.foldLeft[Any, Any](fab, c)(f)
        case KindRight => F.foldLeft[Any, Any](fab, c)(g)
        case _         => F.foldLeft[Any, Any](fab, c)((acc, x) => in.bifoldLeftAny(x, acc, f, g))
      }
    }
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = {
      val F = outer.asInstanceOf[Foldable[AnyF]]
      (k: @scala.annotation.switch) match {
        case KindLeft  => F.foldRight[Any, Any](fab, lc)(f)
        case KindRight => F.foldRight[Any, Any](fab, lc)(g)
        case _         => F.foldRight[Any, Any](fab, lc)((x, acc) => in.bifoldRightAny(x, acc, f, g))
      }
    }
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = {
      val F = outer.asInstanceOf[Traverse[AnyF]]
      (k: @scala.annotation.switch) match {
        case KindLeft  => F.traverse[AnyF, Any, Any](fab)(f)(G)
        case KindRight => F.traverse[AnyF, Any, Any](fab)(g)(G)
        case _         => F.traverse[AnyF, Any, Any](fab)(x => in.bitraverseAny(x, f, g, G))(G)
      }
    }
  }

  /** `G[X, Y]`, where `outer` is the `Bifunctor` / `Bifoldable` / `Bitraverse` of `G`. `G[A, B]` calls `outer` with the
    * functions themselves.
    */
  def map2(outer: Any, first: Any, second: Any): Any = new Node {
    private lazy val x = node(first)
    private lazy val y = node(second)
    private lazy val direct = (x eq leftNode) && (y eq rightNode)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = {
      val B = outer.asInstanceOf[cats.Bifunctor[AnyF2]]
      if (direct) B.bimap[Any, Any, Any, Any](fab)(f, g)
      else B.bimap[Any, Any, Any, Any](fab)(a => x.bimapAny(a, f, g), b => y.bimapAny(b, f, g))
    }
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = {
      val B = outer.asInstanceOf[cats.Bifoldable[AnyF2]]
      if (direct) B.bifoldLeft[Any, Any, Any](fab, c)(f, g)
      else
        B.bifoldLeft[Any, Any, Any](fab, c)(
          (acc, a) => x.bifoldLeftAny(a, acc, f, g),
          (acc, b) => y.bifoldLeftAny(b, acc, f, g)
        )
    }
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = {
      val B = outer.asInstanceOf[cats.Bifoldable[AnyF2]]
      if (direct) B.bifoldRight[Any, Any, Any](fab, lc)(f, g)
      else
        B.bifoldRight[Any, Any, Any](fab, lc)(
          (a, acc) => x.bifoldRightAny(a, acc, f, g),
          (b, acc) => y.bifoldRightAny(b, acc, f, g)
        )
    }
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = {
      val B = outer.asInstanceOf[Bitraverse[AnyF2]]
      if (direct) B.bitraverse[AnyF, Any, Any, Any, Any](fab)(f, g)(G)
      else
        B.bitraverse[AnyF, Any, Any, Any, Any](fab)(a => x.bitraverseAny(a, f, g, G), b => y.bitraverseAny(b, f, g, G))(
          G
        )
    }
  }

  final private val KindLeft = 0
  final private val KindRight = 1
  final private val KindConst = 2
  final private val KindOther = 3

  /** A case class: `values` / `construct` access its fields, `fieldInstances` gives their instances. Fields that are
    * `A`, `B` or constant are handled in place (no call through the megamorphic [[Node]] interface).
    */
  def product(values: Any => Array[Any], construct: Array[Any] => Any, fieldInstances: Any => Array[Any]): Any =
    new Node {
      private lazy val instances: Array[Node] = fieldInstances(null).map(node)
      private def elements(fab: Any): Product = HktGenericRuntime.elementsOf(fab, instances.length, values)
      private lazy val kinds: Array[Int] = instances.map(kindOf)
      def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = {
        val in = elements(fab)
        val ks = kinds
        val out = new Array[Any](ks.length)
        var i = 0
        while (i < out.length) {
          val v = in.productElement(i)
          out(i) = (ks(i): @scala.annotation.switch) match {
            case KindLeft  => f(v)
            case KindRight => g(v)
            case KindConst => v
            case _         => instances(i).bimapAny(v, f, g)
          }
          i += 1
        }
        construct(out)
      }
      def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = {
        val in = elements(fab)
        val ks = kinds
        var acc = c
        var i = 0
        while (i < ks.length) {
          val v = in.productElement(i)
          acc = (ks(i): @scala.annotation.switch) match {
            case KindLeft  => f(acc, v)
            case KindRight => g(acc, v)
            case KindConst => acc
            case _         => instances(i).bifoldLeftAny(v, acc, f, g)
          }
          i += 1
        }
        acc
      }
      def bifoldRightAny(
          fab: Any,
          lc: Eval[Any],
          f: (Any, Eval[Any]) => Eval[Any],
          g: (Any, Eval[Any]) => Eval[Any]
      ): Eval[Any] = {
        val in = elements(fab)
        val ks = kinds
        def loop(i: Int): Eval[Any] =
          if (i >= ks.length) lc
          else if (ks(i) == KindConst) loop(i + 1)
          else instances(i).bifoldRightAny(in.productElement(i), Eval.defer(loop(i + 1)), f, g)
        loop(0)
      }
      def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = {
        val in = values(fab)
        val ks = kinds
        // The effects of the non-constant fields, in order (collected in reverse); constant fields are copied as-is.
        var acc: Any = G.pure(Nil)
        var i = 0
        while (i < in.length) {
          val k = ks(i)
          if (k != KindConst) {
            val next = (k: @scala.annotation.switch) match {
              case KindLeft  => f(in(i))
              case KindRight => g(in(i))
              case _         => instances(i).bitraverseAny(in(i), f, g, G)
            }
            acc = G.map2(acc, next)((done: Any, value: Any) => value :: done.asInstanceOf[List[Any]])
          }
          i += 1
        }
        G.map(acc) { (done: Any) =>
          val result = in.clone()
          var rest = done.asInstanceOf[List[Any]]
          var j = in.length - 1
          while (j >= 0) {
            if (ks(j) != KindConst) { result(j) = rest.head; rest = rest.tail }
            j -= 1
          }
          construct(result)
        }
      }
    }

  /** A sealed trait / enum: `ordinal` tells the subtype of a value, `variantInstances` gives their instances. */
  def coproduct(ordinal: Any => Int, variantInstances: Any => Array[Any]): Any = new Node {
    private lazy val instances: Array[Node] = variantInstances(null).map(node)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = instances(ordinal(fab)).bimapAny(fab, f, g)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any =
      instances(ordinal(fab)).bifoldLeftAny(fab, c, f, g)
    def bifoldRightAny(
        fab: Any,
        lc: Eval[Any],
        f: (Any, Eval[Any]) => Eval[Any],
        g: (Any, Eval[Any]) => Eval[Any]
    ): Eval[Any] = instances(ordinal(fab)).bifoldRightAny(fab, lc, f, g)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any =
      instances(ordinal(fab)).bitraverseAny(fab, f, g, G)
  }
}
