package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** For SemigroupK / MonoidK / EmptyK derivation: a field `G[A]` (probed as `G[Any]`) is combined with `SemigroupK[G]`
  * (`MonoidK[G]`, `EmptyK[G]`) when there is one, like kittens - e.g. an `Option[A]` field with `orElse`, not with
  * `Semigroup[Option[A]]` (which would need a `Semigroup[A]` and combine the values). Other fields use the kernel type
  * class of the field type (e.g. `Semigroup[String]` for a constant field).
  */
trait KernelFromKSupport { this: MacroCommons & StdExtensions =>

  private type AnyKK[X] = Any

  protected lazy val CatsSemigroupKCtor: Type.CtorK1[cats.SemigroupK] = Type.CtorK1.of[cats.SemigroupK]
  protected lazy val CatsMonoidKCtor: Type.CtorK1[cats.MonoidK] = Type.CtorK1.of[cats.MonoidK]
  protected lazy val AlleycatsEmptyKCtor: Type.CtorK1[alleycats.EmptyK] = Type.CtorK1.of[alleycats.EmptyK]
  private lazy val KernelAnyType: Type[Any] = Type.of[Any]
  private lazy val KernelSemigroupCtor: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
  private lazy val KernelMonoidCtor: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
  private lazy val AlleycatsEmptyCtor: Type.Ctor1[alleycats.Empty] = Type.Ctor1.of[alleycats.Empty]

  private def isAny(arg: ??): Boolean = arg.Underlying =:= KernelAnyType

  private def summonSemigroupK(fieldType: Type[Any]): Option[Expr[Any]] =
    Type.decompose1(using fieldType).flatMap { case (gCtor, arg) =>
      if (!isAny(arg)) None
      else {
        implicit val KOfG: Type[cats.SemigroupK[AnyKK]] =
          CatsSemigroupKCtor.apply(using gCtor).asInstanceOf[Type[cats.SemigroupK[AnyKK]]]
        Expr.summonImplicit[cats.SemigroupK[AnyKK]].toOption.map(_.asInstanceOf[Expr[Any]])
      }
    }

  private def summonMonoidK(fieldType: Type[Any]): Option[Expr[Any]] =
    Type.decompose1(using fieldType).flatMap { case (gCtor, arg) =>
      if (!isAny(arg)) None
      else {
        implicit val KOfG: Type[cats.MonoidK[AnyKK]] =
          CatsMonoidKCtor.apply(using gCtor).asInstanceOf[Type[cats.MonoidK[AnyKK]]]
        Expr.summonImplicit[cats.MonoidK[AnyKK]].toOption.map(_.asInstanceOf[Expr[Any]])
      }
    }

  private def summonEmptyK(fieldType: Type[Any]): Option[Expr[Any]] =
    Type.decompose1(using fieldType).flatMap { case (gCtor, arg) =>
      if (!isAny(arg)) None
      else {
        implicit val KOfG: Type[alleycats.EmptyK[AnyKK]] =
          AlleycatsEmptyKCtor.apply(using gCtor).asInstanceOf[Type[alleycats.EmptyK[AnyKK]]]
        Expr.summonImplicit[alleycats.EmptyK[AnyKK]].toOption.map(_.asInstanceOf[Expr[Any]])
      }
    }

  protected def semigroupFromSemigroupK[Field: Type]: Option[Expr[cats.kernel.Semigroup[Field]]] =
    summonSemigroupK(Type[Field].asInstanceOf[Type[Any]]).map { k =>
      implicit val SemigroupField: Type[cats.kernel.Semigroup[Field]] = KernelSemigroupCtor.apply[Field]
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
          .semigroupKAlgebra(Expr.splice(k))
          .asInstanceOf[cats.kernel.Semigroup[Field]]
      }
    }

  protected def monoidFromMonoidK[Field: Type]: Option[Expr[cats.kernel.Monoid[Field]]] =
    summonMonoidK(Type[Field].asInstanceOf[Type[Any]]).map { k =>
      implicit val MonoidField: Type[cats.kernel.Monoid[Field]] = KernelMonoidCtor.apply[Field]
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
          .monoidKAlgebra(Expr.splice(k))
          .asInstanceOf[cats.kernel.Monoid[Field]]
      }
    }

  protected def emptyFromEmptyK[Field: Type]: Option[Expr[alleycats.Empty[Field]]] =
    summonEmptyK(Type[Field].asInstanceOf[Type[Any]]).map { k =>
      implicit val EmptyField: Type[alleycats.Empty[Field]] = AlleycatsEmptyCtor.apply[Field]
      Expr.quote {
        hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
          .emptyKSynthesize(Expr.splice(k))
          .asInstanceOf[alleycats.Empty[Field]]
      }
    }
}
