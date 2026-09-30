package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Shared by the Arbitrary/Shrink/Cogen collection and map rules (issue kubuszok/kindlings#218).
  *
  * A container recognized by `IsCollection` is not necessarily a Scala `Iterable`, and its `factory` builds only the
  * intermediate `CtorResult` (e.g. `List[E]` for `cats.data.NonEmptyList[E]`). The rules must therefore read through
  * `asIterable` and construct through `build`, never by casting.
  */
trait ScalacheckCollectionSupport { this: MacroCommons & StdExtensions =>

  /** Emits `(items: List[Item]) => Either[Any, A]`: fills the provider's `factory` builder with `items` and runs the
    * provider's smart constructor (`build`). Every predefined `CtorLikeOf` variant is normalized to an `Either` whose
    * `Left` only means "rejected" (e.g. an empty list for a non-empty container), so callers can drop or regenerate
    * such candidates instead of throwing.
    *
    * `None` for a custom `CtorLikeOf` variant whose result shape is unknown.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def collectionBuildFn[A: Type, Item: Type](
      isCollection: IsCollectionOf[A, Item]
  ): Option[Expr[List[Item] => Either[Any, A]]] = {
    import isCollection.CtorResult
    implicit val ListItem: Type[List[Item]] = Type.of[List[Item]]
    implicit val EitherAnyA: Type[Either[Any, A]] = Type.of[Either[Any, A]]

    val factoryExpr = isCollection.factory
    val buildStep = isCollection.build

    val toEither: Option[Expr[Any] => Expr[Either[Any, A]]] = buildStep match {
      case _: CtorLikeOf.PlainValue[?, ?] =>
        Some { result =>
          val value = result.asInstanceOf[Expr[A]]
          Expr.quote((Right(Expr.splice(value)): Either[Any, A]))
        }
      case _: CtorLikeOf.EitherStringOrValue[?, ?] | _: CtorLikeOf.EitherIterableStringOrValue[?, ?] |
          _: CtorLikeOf.EitherThrowableOrValue[?, ?] | _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
        // Either is covariant in its Left, so each of these is already an Either[Any, A].
        Some(result => result.asInstanceOf[Expr[Either[Any, A]]])
      case _ =>
        None
    }

    toEither.map { normalize =>
      Expr.quote { (items: List[Item]) =>
        Expr.splice {
          val builderExpr = Expr.quote {
            val builder = Expr.splice(factoryExpr).newBuilder
            builder ++= items
            builder
          }
          normalize(buildStep.ctor(builderExpr).asInstanceOf[Expr[Any]])
        }
      }
    }
  }

  /** Emits `(value: A) => Iterable[Item]` using the provider's `asIterable`. */
  @scala.annotation.nowarn("msg=is never used")
  protected def collectionToIterableFn[A: Type, Item: Type](
      isCollection: IsCollectionOf[A, Item]
  ): Expr[A => Iterable[Item]] = {
    implicit val IterableItem: Type[Iterable[Item]] = Type.of[Iterable[Item]]
    Expr.quote { (value: A) =>
      Expr.splice(isCollection.asIterable(Expr.quote(value)))
    }
  }

  /** Reason used when `collectionBuildFn` does not recognize the provider's smart constructor. */
  protected def unsupportedCollectionBuild[A: Type](isCollection: IsCollectionOf[A, ?]): String =
    s"The type ${Type[A].prettyPrint} is built with an unsupported smart constructor (${isCollection.build})"
}
