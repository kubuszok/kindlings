package hearth.kindlings.scalacheckderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
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

  /** Whether iterating two equal values of `A` may visit elements in different orders - Scala and Java sets and maps
    * (e.g. `Set(1, 2)` and `Set(2, 1)` are equal, but small immutable sets iterate in insertion order). A `Cogen` for
    * such a type must combine its elements order-independently, or equal values would perturb differently.
    */
  protected def isUnorderedCollection[A: Type]: Boolean = {
    val unordered = List(
      Type.of[scala.collection.Set[Any]].asUntyped,
      Type.of[scala.collection.Map[Any, Any]].asUntyped,
      Type.of[java.util.Set[Any]].asUntyped,
      Type.of[java.util.Map[Any, Any]].asUntyped
    )
    Type[A].asUntyped.baseClasses.exists(base => unordered.exists(base.sameTypeConstructorAs(_)))
  }

  /** Emits `(value: A) => Int` returning the index (in `directChildren` order) of the case `value` belongs to, using a
    * real pattern match (`Enum.matchOn`) - so type classes derived per case can be dispatched to exactly the matching
    * case, instead of trying each case's instance until one does not throw (which silently picked the wrong case, e.g.
    * a singleton case's identity instance accepts every value).
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def enumOrdinalFn[A: Type](enumData: Enum[A]): MIO[Expr[A => Int]] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    implicit val OrdinalFnT: Type[A => Int] = Type.of[A => Int]
    val cases: List[??] = enumData.directChildren.toList.map { case (_, child) =>
      import child.Underlying as Child
      Type[Child].as_??
    }
    MIO.scoped { runSafe =>
      // Keep the pattern-match construction out of the splice itself: on Scala 2 cross-quotes re-emit the splice's
      // source, which must not contain macro-level lambdas typed with this trait's path-dependent types.
      def ordinal(value: Expr[A]): Expr[Int] = runSafe(enumOrdinalBody[A](enumData, cases, value))
      Expr.quote { (value: A) =>
        Expr.splice(ordinal(Expr.quote(value)))
      }
    }
  }

  private def enumOrdinalBody[A: Type](enumData: Enum[A], cases: List[??], value: Expr[A]): MIO[Expr[Int]] = {
    implicit val IntT: Type[Int] = Type.of[Int]
    enumData
      .matchOn[MIO, Int](value) { matched =>
        import matched.Underlying as Case
        MIO.pure(Expr(cases.indexWhere(tpe => tpe.Underlying =:= Type[Case])))
      }
      .map(_.getOrElse(Expr(-1)))
  }
}
