package hearth.kindlings.catsderivation.internal.runtime

object HktNestedRuntime {

  private type AnyF[X] = Any

  def mapNested(functor: Any, fa: Any, f: Any): Any =
    functor.asInstanceOf[cats.Functor[AnyF]].map(fa)(f.asInstanceOf[Any => Any])

  def foldLeftNested(foldable: Any, fa: Any, b: Any, f: Any): Any =
    foldable.asInstanceOf[cats.Foldable[AnyF]].foldLeft(fa, b)(f.asInstanceOf[(Any, Any) => Any])

  def foldRightNested(foldable: Any, fa: Any, lb: cats.Eval[Any], f: Any): cats.Eval[Any] =
    foldable
      .asInstanceOf[cats.Foldable[AnyF]]
      .foldRight(fa, lb)(f.asInstanceOf[(Any, cats.Eval[Any]) => cats.Eval[Any]])

  def traverseNested(traverse: Any, fa: Any, f: Any, G: Any): Any =
    traverse
      .asInstanceOf[cats.Traverse[AnyF]]
      .traverse[AnyF, Any, Any](fa)(f.asInstanceOf[Any => Any])(G.asInstanceOf[cats.Applicative[AnyF]])

  /** Map over a nested self-recursive field like `Option[Search[A]]`.
    *
    * Uses the outer functor (e.g., `Functor[Option]`) to map over the container, and the self functor (the
    * `Functor[Search]` being derived, passed as `var self$macro`) to recursively map each inner element.
    */
  @scala.annotation.nowarn("msg=dead code")
  def mapNestedSelfRecursive(outerFunctor: Any, selfFunctor: Any, fa: Any, f: Any): Any =
    outerFunctor
      .asInstanceOf[cats.Functor[AnyF]]
      .map(fa)((inner: Any) => selfFunctor.asInstanceOf[cats.Functor[AnyF]].map(inner)(f.asInstanceOf[Any => Any]))
}
