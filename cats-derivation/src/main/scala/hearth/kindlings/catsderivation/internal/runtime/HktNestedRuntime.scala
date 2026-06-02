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
}
