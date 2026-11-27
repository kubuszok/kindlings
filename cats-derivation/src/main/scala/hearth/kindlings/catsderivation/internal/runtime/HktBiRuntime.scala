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
    final def bifoldRight[A, B, C](fab: Any, c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C])
        : Eval[C] =
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

  /** The part `A`. */
  val left: Any = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = f(fab)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = f(c, fab)
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] = f(fab, lc)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = f(fab)
  }

  /** The part `B`. */
  val right: Any = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = g(fab)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = g(c, fab)
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] = g(fab, lc)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = g(fab)
  }

  /** A part mentioning neither type parameter (like `cats.data.Const`). */
  val const: Any = new Node {
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = fab
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = c
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] = lc
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = G.pure(fab)
  }

  /** `G[X]`, where `outer` is the `Functor` / `Foldable` / `Traverse` of `G` and `inner` the instance of `X`. */
  def map1(outer: Any, inner: Any): Any = new Node {
    private lazy val in = node(inner)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any =
      outer.asInstanceOf[Functor[AnyF]].map[Any, Any](fab)(x => in.bimapAny(x, f, g))
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any =
      outer.asInstanceOf[Foldable[AnyF]].foldLeft[Any, Any](fab, c)((acc, x) => in.bifoldLeftAny(x, acc, f, g))
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] =
      outer.asInstanceOf[Foldable[AnyF]].foldRight[Any, Any](fab, lc)((x, acc) => in.bifoldRightAny(x, acc, f, g))
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any =
      outer.asInstanceOf[Traverse[AnyF]].traverse[AnyF, Any, Any](fab)(x => in.bitraverseAny(x, f, g, G))(G)
  }

  /** `G[X, Y]`, where `outer` is the `Bifunctor` / `Bifoldable` / `Bitraverse` of `G`. */
  def map2(outer: Any, first: Any, second: Any): Any = new Node {
    private lazy val x = node(first)
    private lazy val y = node(second)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any =
      outer.asInstanceOf[cats.Bifunctor[AnyF2]].bimap[Any, Any, Any, Any](fab)(a => x.bimapAny(a, f, g), b => y.bimapAny(b, f, g))
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any =
      outer
        .asInstanceOf[cats.Bifoldable[AnyF2]]
        .bifoldLeft[Any, Any, Any](fab, c)((acc, a) => x.bifoldLeftAny(a, acc, f, g), (acc, b) => y.bifoldLeftAny(b, acc, f, g))
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] =
      outer
        .asInstanceOf[cats.Bifoldable[AnyF2]]
        .bifoldRight[Any, Any, Any](fab, lc)((a, acc) => x.bifoldRightAny(a, acc, f, g), (b, acc) => y.bifoldRightAny(b, acc, f, g))
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any =
      outer
        .asInstanceOf[Bitraverse[AnyF2]]
        .bitraverse[AnyF, Any, Any, Any, Any](fab)(a => x.bitraverseAny(a, f, g, G), b => y.bitraverseAny(b, f, g, G))(
          G
        )
  }

  /** A case class: `values` / `construct` access its fields, `fieldInstances` gives their instances. */
  def product(values: Any => Array[Any], construct: Array[Any] => Any, fieldInstances: Any => Array[Any]): Any =
    new Node {
      private lazy val instances: Array[Node] = fieldInstances(null).map(node)
      def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = {
        val in = values(fab)
        val out = new Array[Any](in.length)
        var i = 0
        while (i < in.length) { out(i) = instances(i).bimapAny(in(i), f, g); i += 1 }
        construct(out)
      }
      def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any = {
        val in = values(fab)
        var acc = c
        var i = 0
        while (i < in.length) { acc = instances(i).bifoldLeftAny(in(i), acc, f, g); i += 1 }
        acc
      }
      def bifoldRightAny(
          fab: Any,
          lc: Eval[Any],
          f: (Any, Eval[Any]) => Eval[Any],
          g: (Any, Eval[Any]) => Eval[Any]
      ): Eval[Any] = {
        val in = values(fab)
        def loop(i: Int): Eval[Any] =
          if (i >= in.length) lc
          else instances(i).bifoldRightAny(in(i), Eval.defer(loop(i + 1)), f, g)
        loop(0)
      }
      def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any = {
        val in = values(fab)
        var acc: Any = G.pure(List.empty[Any])
        var i = 0
        while (i < in.length) {
          val next = instances(i).bitraverseAny(in(i), f, g, G)
          acc = G.map2(acc, next)((done: Any, value: Any) => value :: done.asInstanceOf[List[Any]])
          i += 1
        }
        G.map(acc)((done: Any) => construct(done.asInstanceOf[List[Any]].reverse.toArray))
      }
    }

  /** A sealed trait / enum: `ordinal` tells the subtype of a value, `variantInstances` gives their instances. */
  def coproduct(ordinal: Any => Int, variantInstances: Any => Array[Any]): Any = new Node {
    private lazy val instances: Array[Node] = variantInstances(null).map(node)
    def bimapAny(fab: Any, f: Any => Any, g: Any => Any): Any = instances(ordinal(fab)).bimapAny(fab, f, g)
    def bifoldLeftAny(fab: Any, c: Any, f: (Any, Any) => Any, g: (Any, Any) => Any): Any =
      instances(ordinal(fab)).bifoldLeftAny(fab, c, f, g)
    def bifoldRightAny(fab: Any, lc: Eval[Any], f: (Any, Eval[Any]) => Eval[Any], g: (Any, Eval[Any]) => Eval[Any])
        : Eval[Any] = instances(ordinal(fab)).bifoldRightAny(fab, lc, f, g)
    def bitraverseAny(fab: Any, f: Any => Any, g: Any => Any, G: Applicative[AnyF]): Any =
      instances(ordinal(fab)).bitraverseAny(fab, f, g, G)
  }
}
