package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** The general derivation of the polymorphic (kind `* -> *`) type classes, following kittens' rules for every shape of
  * `F[A]` - the specialised derivations (which generate code per field) handle the common shapes and fall back to this
  * one for the rest:
  *
  *   - `A` itself: cats' `Id` instances,
  *   - a type not mentioning `A`: a `Const`-like instance (needing its `Semigroup` / `Monoid` / `Empty` where kittens
  *     needs one),
  *   - a type constructor applied to `A` (`G[A]`, `Either[E, A]`, `A => R`): the implicit instance when there is one,
  *     otherwise the one derived for `G` (so a field whose type is another data type needs no instance in scope),
  *   - nested type constructors (`G[H[A]]`): the composition of the instances kittens composes (e.g. two contravariant
  *     functors form a functor, `Apply` and `SemigroupK` form a `SemigroupK`),
  *   - a case class: its fields' instances, combined field by field,
  *   - a sealed trait / enum: its subtypes' instances.
  *
  * A type is represented by its application to two probes (`Int` and `String`) and to `Any`: positions holding the
  * probes are the type parameter, positions equal in both are constant, and the `Any` application is what the generated
  * (erased) code works with. The instances are assembled at runtime by `HktGenericRuntime` / `HktWrappedRuntime`;
  * instances of recursive types refer to each other through `HktGenericRuntime.Registry`.
  */
trait ErasedHktSupport
    extends CatsDerivationTimeout
    with CatsDerivationErrorSupport
    with hearth.kindlings.derivation.compiletime.InstanceScopedVals {
  this: MacroCommons & StdExtensions =>

  sealed abstract class ErasedTC(val name: String)
  object ErasedTC {
    case object Functor extends ErasedTC("Functor")
    case object Contravariant extends ErasedTC("Contravariant")
    case object Invariant extends ErasedTC("Invariant")
    case object Foldable extends ErasedTC("Foldable")
    case object Traverse extends ErasedTC("Traverse")
    case object Reducible extends ErasedTC("Reducible")
    case object NonEmptyTraverse extends ErasedTC("NonEmptyTraverse")
    case object Apply extends ErasedTC("Apply")
    case object Applicative extends ErasedTC("Applicative")
    case object Pure extends ErasedTC("Pure")
    case object EmptyK extends ErasedTC("EmptyK")
    case object SemigroupK extends ErasedTC("SemigroupK")
    case object MonoidK extends ErasedTC("MonoidK")
    case object NonEmptyAlternative extends ErasedTC("NonEmptyAlternative")
    case object Alternative extends ErasedTC("Alternative")
  }

  private type ErasedAnyK[X] = Any

  private lazy val ErasedIntType: Type[Int] = Type.of[Int]
  private lazy val ErasedStringType: Type[String] = Type.of[String]
  private lazy val ErasedAnyType: Type[Any] = Type.of[Any]
  private lazy val ErasedArrayAnyType: Type[Array[Any]] = Type.of[Array[Any]]
  private lazy val ErasedRegistryType
      : Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
    Type.of[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry]

  private lazy val ErasedFunctorK: Type.CtorK1[cats.Functor] = Type.CtorK1.of[cats.Functor]
  private lazy val ErasedContravariantK: Type.CtorK1[cats.Contravariant] = Type.CtorK1.of[cats.Contravariant]
  private lazy val ErasedInvariantK: Type.CtorK1[cats.Invariant] = Type.CtorK1.of[cats.Invariant]
  private lazy val ErasedFoldableK: Type.CtorK1[cats.Foldable] = Type.CtorK1.of[cats.Foldable]
  private lazy val ErasedTraverseK: Type.CtorK1[cats.Traverse] = Type.CtorK1.of[cats.Traverse]
  private lazy val ErasedReducibleK: Type.CtorK1[cats.Reducible] = Type.CtorK1.of[cats.Reducible]
  private lazy val ErasedNonEmptyTraverseK: Type.CtorK1[cats.NonEmptyTraverse] = Type.CtorK1.of[cats.NonEmptyTraverse]
  private lazy val ErasedApplyK: Type.CtorK1[cats.Apply] = Type.CtorK1.of[cats.Apply]
  private lazy val ErasedApplicativeK: Type.CtorK1[cats.Applicative] = Type.CtorK1.of[cats.Applicative]
  private lazy val ErasedPureK: Type.CtorK1[alleycats.Pure] = Type.CtorK1.of[alleycats.Pure]
  private lazy val ErasedEmptyKK: Type.CtorK1[alleycats.EmptyK] = Type.CtorK1.of[alleycats.EmptyK]
  private lazy val ErasedSemigroupKK: Type.CtorK1[cats.SemigroupK] = Type.CtorK1.of[cats.SemigroupK]
  private lazy val ErasedMonoidKK: Type.CtorK1[cats.MonoidK] = Type.CtorK1.of[cats.MonoidK]
  private lazy val ErasedNonEmptyAlternativeK: Type.CtorK1[cats.NonEmptyAlternative] =
    Type.CtorK1.of[cats.NonEmptyAlternative]
  private lazy val ErasedAlternativeK: Type.CtorK1[cats.Alternative] = Type.CtorK1.of[cats.Alternative]
  private lazy val ErasedSemigroupCtor: Type.Ctor1[cats.kernel.Semigroup] = Type.Ctor1.of[cats.kernel.Semigroup]
  private lazy val ErasedMonoidCtor: Type.Ctor1[cats.kernel.Monoid] = Type.Ctor1.of[cats.kernel.Monoid]
  private lazy val ErasedEmptyCtor: Type.Ctor1[alleycats.Empty] = Type.Ctor1.of[alleycats.Empty]

  private lazy val ErasedFunction1Ctor: Type.Ctor2[Function1] = Type.Ctor2.of[Function1]
  private lazy val ErasedEitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]
  private lazy val ErasedMapCtor: Type.Ctor2[Map] = Type.Ctor2.of[Map]

  /** cats' instance of a binary type constructor applied to one constant argument (`* => R`, `X => *`, `Either[E, *]`,
    * `Map[K, *]`), for when implicit search cannot be asked for it: on Scala 2 the partially applied type constructor
    * cannot be built (docs/research/hearth-gap-partially-applied-ctor-scala2.md).
    */
  private def knownInstanceFor(tc: ErasedTC, lam: Lam): Option[Expr[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    val untyped = UntypedType.fromTyped(using lam.int)
    val holeIsFirst = typeArgumentsOf(lam.int).zip(typeArgumentsOf(lam.string)).headOption.exists { case (i, s) =>
      !(i =:= s)
    }
    if (ErasedFunction1Ctor.sameTypeConstructorAs(untyped)) {
      if (holeIsFirst) tc match {
        case Contravariant | Invariant =>
          Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.function1Contravariant))
        case _ => None
      }
      else
        tc match {
          case Functor | Invariant | Apply | Applicative =>
            Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.function1Monad))
          case _ => None
        }
    } else if (ErasedEitherCtor.sameTypeConstructorAs(untyped) && !holeIsFirst) tc match {
      case Functor | Invariant | Foldable | Traverse | Apply | Applicative =>
        Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.eitherInstances))
      case SemigroupK =>
        Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.eitherSemigroupK))
      case _ => None
    }
    else if (ErasedMapCtor.sameTypeConstructorAs(untyped) && !holeIsFirst) tc match {
      case Functor | Invariant | Apply =>
        Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.mapInstances))
      case SemigroupK | MonoidK =>
        Some(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.mapMonoidK))
      case _ => None
    }
    else None
  }

  /** A type `T[A]` seen through its applications to the probes (`Int`, `String`) and to `Any`. */
  final private case class Lam(int: Type[Any], string: Type[Any], any: Type[Any]) {
    def isId: Boolean = int =:= ErasedIntType && string =:= ErasedStringType
    def isConst: Boolean = int =:= string
    def show: String = if (isId) "A" else any.prettyPrint
    def sameAs(other: Lam): Boolean = int =:= other.int && string =:= other.string
  }

  private def lamOf[F[_]](ctor: Type.Ctor1[F]): Lam = Lam(
    ctor.apply[Int](using ErasedIntType).asInstanceOf[Type[Any]],
    ctor.apply[String](using ErasedStringType).asInstanceOf[Type[Any]],
    ctor.apply[Any](using ErasedAnyType).asInstanceOf[Type[Any]]
  )

  private def typeArgumentsOf(tpe: Type[Any]): List[Type[Any]] =
    UntypedType
      .typeArguments(UntypedType.dealias(UntypedType.fromTyped(using tpe)))
      .map(_.as_??.Underlying.asInstanceOf[Type[Any]])

  /** How the instance of a type is obtained; turned into code by [[emitPlan]]. */
  sealed private trait Plan
  private object Plan {

    /** An expression not referring to instances being derived (implicit, `Id`, constant). */
    final case class Ready(instance: Expr[Any]) extends Plan

    /** The instance (being) derived for an enclosing type, looked up in the registry. */
    final case class Self(id: Int) extends Plan

    final case class Composed(compose: (Expr[Any], Expr[Any]) => Expr[Any], outer: Plan, inner: Plan) extends Plan

    /** Registers the underlying instance under `id` as well. */
    final case class Alias(id: Int, underlying: Plan) extends Plan

    final case class Product(
        id: Int,
        tc: ErasedTC,
        values: Expr[Any => Array[Any]],
        construct: Expr[Array[Any] => Any],
        fields: List[Plan]
    ) extends Plan

    final case class Coproduct(id: Int, tc: ErasedTC, ordinal: Expr[Any => Int], variants: List[Plan]) extends Plan
  }

  /** Whether an instance of `have` (the type class being derived for an enclosing type) can be used as `want`. */
  private def provides(have: ErasedTC, want: ErasedTC): Boolean = {
    import ErasedTC.*
    val functors: Set[ErasedTC] =
      Set(Functor, Traverse, NonEmptyTraverse, Apply, Applicative, NonEmptyAlternative, Alternative)
    have == want || (want match {
      case Functor             => functors(have)
      case Invariant           => functors(have) || have == Contravariant
      case Foldable            => Set[ErasedTC](Traverse, Reducible, NonEmptyTraverse)(have)
      case Traverse            => have == NonEmptyTraverse
      case Reducible           => have == NonEmptyTraverse
      case Apply               => Set[ErasedTC](Applicative, NonEmptyAlternative, Alternative)(have)
      case Applicative         => Set[ErasedTC](NonEmptyAlternative, Alternative)(have)
      case SemigroupK          => Set[ErasedTC](MonoidK, NonEmptyAlternative, Alternative)(have)
      case MonoidK             => have == Alternative
      case NonEmptyAlternative => have == Alternative
      case _                   => false
    })
  }

  private def summonFor(tc: ErasedTC, ctor: Type.Ctor1[ErasedAnyK]): Option[Expr[Any]] = {
    def summon[TC[_[_]]](ctorK: Type.CtorK1[TC]): Option[Expr[Any]] = {
      implicit val InstanceType: Type[TC[ErasedAnyK]] = ctorK.apply[ErasedAnyK](using ctor)
      Expr.summonImplicit[TC[ErasedAnyK]].toOption.map(_.asInstanceOf[Expr[Any]])
    }
    tc match {
      case ErasedTC.Functor             => summon(ErasedFunctorK)
      case ErasedTC.Contravariant       => summon(ErasedContravariantK)
      case ErasedTC.Invariant           => summon(ErasedInvariantK)
      case ErasedTC.Foldable            => summon(ErasedFoldableK)
      case ErasedTC.Traverse            => summon(ErasedTraverseK)
      case ErasedTC.Reducible           => summon(ErasedReducibleK)
      case ErasedTC.NonEmptyTraverse    => summon(ErasedNonEmptyTraverseK)
      case ErasedTC.Apply               => summon(ErasedApplyK)
      case ErasedTC.Applicative         => summon(ErasedApplicativeK)
      case ErasedTC.Pure                => summon(ErasedPureK)
      case ErasedTC.EmptyK              => summon(ErasedEmptyKK)
      case ErasedTC.SemigroupK          => summon(ErasedSemigroupKK)
      case ErasedTC.MonoidK             => summon(ErasedMonoidKK)
      case ErasedTC.NonEmptyAlternative => summon(ErasedNonEmptyAlternativeK)
      case ErasedTC.Alternative         => summon(ErasedAlternativeK)
    }
  }

  private def summonKernel[K[_]](ctor: Type.Ctor1[K], tpe: Type[Any]): Option[Expr[Any]] = {
    val kernelType: Type[Any] = ctor.apply[Any](using tpe).asInstanceOf[Type[Any]]
    Expr.summonImplicit[Any](using kernelType).toOption.map(_.asInstanceOf[Expr[Any]])
  }

  /** The value of a type with exactly one value: a case object / enum value or a literal type. */
  private def singleValueOf(tpe: Type[Any]): Option[Expr[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    val singleton: ?? = tpe.as_??
    import singleton.Underlying as S
    SingletonValue.unapply(Type[S]).map(_.singletonExpr.upcast[Any]).orElse(literalOf(tpe))
  }

  private def literalOf(tpe: Type[Any]): Option[Expr[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    def decode[U](codec: TypeCodec[U])(implicit U: Type[U], exprCodec: ExprCodec[U]): Option[Expr[Any]] =
      codec.fromType(tpe).map(found => Expr[U](found.value: U).upcast[Any])
    decode(Type.StringCodec)(ErasedStringType, implicitly[ExprCodec[String]])
      .orElse(decode(Type.IntCodec)(ErasedIntType, implicitly[ExprCodec[Int]]))
      .orElse(decode(Type.LongCodec)(Type.of[Long], implicitly[ExprCodec[Long]]))
      .orElse(decode(Type.DoubleCodec)(Type.of[Double], implicitly[ExprCodec[Double]]))
      .orElse(decode(Type.FloatCodec)(Type.of[Float], implicitly[ExprCodec[Float]]))
      .orElse(decode(Type.BooleanCodec)(Type.of[Boolean], implicitly[ExprCodec[Boolean]]))
      .orElse(decode(Type.CharCodec)(Type.of[Char], implicitly[ExprCodec[Char]]))
      .orElse(decode(Type.ShortCodec)(Type.of[Short], implicitly[ExprCodec[Short]]))
      .orElse(decode(Type.ByteCodec)(Type.of[Byte], implicitly[ExprCodec[Byte]]))
  }

  private def idInstance(tc: ErasedTC): Either[String, Expr[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    tc match {
      case Functor | Invariant | Foldable | Traverse | Reducible | NonEmptyTraverse | Apply | Applicative =>
        Right(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.idApplicative))
      case Pure =>
        Right(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.idPure))
      case _ =>
        Left(s"the type parameter itself has no ${tc.name}")
    }
  }

  /** The `Const`-like instance of a type not mentioning the type parameter, when the type class has one. */
  private def constInstance(tc: ErasedTC, tpe: Type[Any]): Either[String, Expr[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    def kernel[K[_]](ctor: Type.Ctor1[K], label: String)(build: Expr[Any] => Expr[Any]): Either[String, Expr[Any]] =
      summonKernel(ctor, tpe)
        .map(instance => build(instanceScopedAny(instance)))
        .toRight(s"${tpe.prettyPrint} does not mention the type parameter and has no $label")
    tc match {
      case Functor | Invariant | Foldable | Traverse =>
        Right(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.constTraverse))
      case Contravariant =>
        Right(Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.constContravariant))
      case Reducible | NonEmptyTraverse =>
        Left(s"${tpe.prettyPrint} does not mention the type parameter, so it cannot guarantee an element")
      case Apply =>
        kernel(ErasedSemigroupCtor, "Semigroup") { sg =>
          Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.constApply(Expr.splice(sg)))
        }
      case Applicative =>
        kernel(ErasedMonoidCtor, "Monoid") { m =>
          Expr.quote(
            hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.constApplicative(Expr.splice(m))
          )
        }
      case Pure =>
        kernel(ErasedEmptyCtor, "Empty") { e =>
          Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.constPure(Expr.splice(e)))
        }.orElse {
          singleValueOf(tpe)
            .map { value =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.singletonPure(Expr.splice(value))
              )
            }
            .toRight(s"${tpe.prettyPrint} does not mention the type parameter and has no Empty")
        }
      case EmptyK =>
        kernel(ErasedEmptyCtor, "Empty") { e =>
          Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.constEmptyK(Expr.splice(e)))
        }
      case SemigroupK =>
        kernel(ErasedSemigroupCtor, "Semigroup") { sg =>
          Expr.quote(
            hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.constSemigroupK(Expr.splice(sg))
          )
        }
      case MonoidK =>
        kernel(ErasedMonoidCtor, "Monoid") { m =>
          Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.constMonoidK(Expr.splice(m)))
        }
      case NonEmptyAlternative | Alternative =>
        kernel(ErasedMonoidCtor, "Monoid") { m =>
          Expr.quote(
            hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
              .constNonEmptyAlternative(Expr.splice(m))
          )
        }
    }
  }

  /** The ways kittens builds the instance of `G[H[A]]`: the type classes needed for `G` and (if any) for `H`. */
  private def nestedRules(
      tc: ErasedTC
  ): List[(ErasedTC, Option[ErasedTC], (Expr[Any], Expr[Any]) => Expr[Any])] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    val outerOnly: (Expr[Any], Expr[Any]) => Expr[Any] = (outer, _) => outer
    tc match {
      case Functor =>
        List(
          (
            Functor,
            Some(Functor),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                  .composeFunctor(Expr.splice(o), Expr.splice(i))
              )
          ),
          (
            Contravariant,
            Some(Contravariant),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composeContravariants(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Contravariant =>
        List(
          (
            Functor,
            Some(Contravariant),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composeFunctorContravariant(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Invariant =>
        List(
          (
            Invariant,
            Some(Invariant),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composeInvariant(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Foldable =>
        List(
          (
            Foldable,
            Some(Foldable),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                  .composeFoldable(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Traverse =>
        List(
          (
            Traverse,
            Some(Traverse),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktNestedRuntime
                  .composeTraverse(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Reducible =>
        List(
          (
            Reducible,
            Some(Reducible),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeReducible(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case NonEmptyTraverse =>
        List(
          (
            NonEmptyTraverse,
            Some(NonEmptyTraverse),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeNonEmptyTraverse(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Apply =>
        List(
          (
            Apply,
            Some(Apply),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeApply(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Applicative =>
        List(
          (
            Applicative,
            Some(Applicative),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeApplicative(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Pure =>
        List(
          (
            Pure,
            Some(Pure),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composePure(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case EmptyK =>
        List(
          (EmptyK, None, outerOnly),
          (
            Pure,
            Some(EmptyK),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composePureEmptyK(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case SemigroupK =>
        List(
          (SemigroupK, None, outerOnly),
          (
            Apply,
            Some(SemigroupK),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composeApplySemigroupK(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case MonoidK =>
        List(
          (MonoidK, None, outerOnly),
          (
            Applicative,
            Some(MonoidK),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
                  .composeApplicativeMonoidK(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case NonEmptyAlternative =>
        List(
          (
            NonEmptyAlternative,
            Some(Applicative),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeNonEmptyAlternative(Expr.splice(o), Expr.splice(i))
              )
          )
        )
      case Alternative =>
        List(
          (
            Alternative,
            Some(Applicative),
            (o, i) =>
              Expr.quote(
                hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
                  .composeAlternative(Expr.splice(o), Expr.splice(i))
              )
          )
        )
    }
  }

  /** `G` and the inner type of a type applying a type constructor (of one or two parameters) to exactly one argument
    * mentioning the type parameter: `List[Option[A]]` gives `List` and `Option[A]`, `Either[E, A]` gives `Either[E, *]`
    * and `A`, `A => R` gives `* => R` and `A`.
    */
  private def outerAndInner(lam: Lam): Option[(Type.Ctor1[ErasedAnyK], Lam)] = {
    val args =
      typeArgumentsOf(lam.int).zip(typeArgumentsOf(lam.string)).zip(typeArgumentsOf(lam.any)).map { case ((i, s), a) =>
        Lam(i, s, a)
      }
    args.filterNot(_.isConst) match {
      case inner :: Nil =>
        args match {
          case _ :: Nil =>
            Type.decompose1(using lam.int).map { case (ctor, _) => (ctor.asInstanceOf[Type.Ctor1[ErasedAnyK]], inner) }
          case first :: second :: Nil =>
            Type.decompose2(using lam.int).map { case (ctor, (a, b)) =>
              val partiallyApplied =
                if (first.isConst) {
                  import a.Underlying as Fixed
                  ctor.setA[Fixed].asInstanceOf[Type.Ctor1[ErasedAnyK]]
                } else {
                  import b.Underlying as Fixed
                  ctor.setB[Fixed].asInstanceOf[Type.Ctor1[ErasedAnyK]]
                }
              val _ = second
              (partiallyApplied, inner)
            }
          case _ => None
        }
      case _ => None
    }
  }

  /** Builds the [[Plan]]s of one expansion. */
  final private class Planner(runSafe: hearth.fp.DirectStyle.RunSafe[MIO]) {

    private var nextId: Int = 0
    private var inProgress: List[(ErasedTC, Lam, Int)] = Nil

    def registrySize: Int = nextId

    /** Runs a planning attempt, forgetting the instance-scoped vals it recorded when it fails (an alternative is tried
      * next): otherwise they would be emitted unused (an "is never used" warning at the expansion site).
      */
    private def attempt[A](body: => Either[List[String], A]): Either[List[String], A] = {
      val snapshot = instanceScopedValsSnapshot()
      val result = body
      if (result.isLeft) restoreInstanceScopedVals(snapshot)
      result
    }

    def plan(tc: ErasedTC, lam: Lam, isTop: Boolean): Either[List[String], Plan] =
      if (lam.isId) idInstance(tc).map(Plan.Ready(_)).left.map(List(_))
      else if (lam.isConst)
        constInstance(tc, lam.any) match {
          case Right(instance) => Right(Plan.Ready(instance))
          // A case object or a case class of constant fields (e.g. an enum's `case Empty`): field by field.
          case Left(reason) => generic(tc, lam).left.map(_ => List(reason))
        }
      else
        inProgress.collectFirst {
          case (have, enclosing, id) if provides(have, tc) && enclosing.sameAs(lam) => id
        } match {
          case Some(id) => Right(Plan.Self(id))
          case None     =>
            val applied: Either[List[String], Plan] = attempt(outerAndInner(lam) match {
              case Some((outer, inner)) if inner.isId =>
                if (isTop) Left(Nil)
                else
                  summonFor(tc, outer)
                    .orElse(knownInstanceFor(tc, lam))
                    .map(instance => Plan.Ready(instanceScopedAny(instance)))
                    .toRight(List(s"no implicit ${tc.name} for ${lam.show}"))
              case Some((outer, inner)) => nested(tc, outer, inner, lam)
              case None                 => Left(Nil)
            })
            applied match {
              case Right(found) => Right(found)
              case Left(errors) => generic(tc, lam).left.map(errors ++ _)
            }
        }

    private def nested(
        tc: ErasedTC,
        outer: Type.Ctor1[ErasedAnyK],
        inner: Lam,
        lam: Lam
    ): Either[List[String], Plan] = {
      val outerLam = lamOf(outer)
      val attempts = nestedRules(tc).iterator.map { case (outerTc, innerTc, compose) =>
        attempt(for {
          outerPlan <- plan(outerTc, outerLam, isTop = false)
          result <- innerTc match {
            case None       => Right(outerPlan)
            case Some(inTc) => plan(inTc, inner, isTop = false).map(Plan.Composed(compose, outerPlan, _))
          }
        } yield result)
      }
      val errors = List.newBuilder[String]
      var found: Option[Plan] = None
      while (found.isEmpty && attempts.hasNext)
        attempts.next() match {
          case Right(p)      => found = Some(p)
          case Left(reasons) => errors ++= reasons
        }
      found.toRight(s"${lam.show} is not a composition ${tc.name} can be derived for" :: errors.result().map("  " + _))
    }

    /** The instance of a case class or of a sealed trait, from the instances of its fields / subtypes. */
    private def generic(tc: ErasedTC, lam: Lam): Either[List[String], Plan] = {
      val id = nextId
      nextId += 1
      inProgress = (tc, lam, id) :: inProgress
      try
        productParts(lam) match {
          case Some(Right((values, construct, fields))) => product(tc, lam, id, values, construct, fields)
          case Some(Left(reason))                       => Left(List(reason))
          case None                                     =>
            coproductParts(lam) match {
              case Some(Right((ordinal, variants))) => coproduct(tc, lam, id, ordinal, variants)
              case Some(Left(reason))               => Left(List(reason))
              case None                             =>
                Left(List(s"${lam.show} is neither a case class nor a sealed trait, and has no ${tc.name}"))
            }
        }
      finally inProgress = inProgress.tail
    }

    private def product(
        tc: ErasedTC,
        lam: Lam,
        id: Int,
        values: Expr[Any => Array[Any]],
        construct: Expr[Array[Any] => Any],
        fields: List[(String, Lam)]
    ): Either[List[String], Plan] = {
      // Reducible / NonEmptyTraverse: Foldable / Traverse fields, at least one of them being non-empty.
      val (fieldTc, nonEmptyTc): (ErasedTC, Option[ErasedTC]) = tc match {
        case ErasedTC.Reducible        => (ErasedTC.Foldable, Some(ErasedTC.Reducible))
        case ErasedTC.NonEmptyTraverse => (ErasedTC.Traverse, Some(ErasedTC.NonEmptyTraverse))
        case other                     => (other, None)
      }
      val results: List[Either[List[String], (Plan, Boolean)]] = fields.map { case (name, fieldLam) =>
        val nonEmpty = nonEmptyTc.flatMap(nonEmpty => attempt(plan(nonEmpty, fieldLam, isTop = false)).toOption)
        nonEmpty match {
          case Some(p) => Right((p, true))
          case None    =>
            plan(fieldTc, fieldLam, isTop = false)
              .map((_, false))
              .left
              .map(reasons => s"field $name of ${lam.show}:" :: reasons.map("  " + _))
        }
      }
      val errors = results.collect { case Left(reasons) => reasons }.flatten
      if (errors.nonEmpty) Left(errors)
      else {
        val plans = results.collect { case Right(found) => found }
        if (nonEmptyTc.isDefined && !plans.exists(_._2))
          Left(
            List(
              s"no field of ${lam.show} guarantees an element: like kittens, at least one field must be of the type " +
                s"parameter or wrap it in a ${tc.name} type"
            )
          )
        else Right(Plan.Product(id, tc, values, construct, plans.map(_._1)))
      }
    }

    private def coproduct(
        tc: ErasedTC,
        lam: Lam,
        id: Int,
        ordinal: Expr[Any => Int],
        variants: List[(String, Lam)]
    ): Either[List[String], Plan] = {
      import ErasedTC.*
      tc match {
        case Functor | Contravariant | Invariant | Foldable | Traverse | Reducible | NonEmptyTraverse =>
          val results = variants.map { case (name, variantLam) =>
            plan(tc, variantLam, isTop = false).left.map(reasons =>
              s"subtype $name of ${lam.show}:" :: reasons.map("  " + _)
            )
          }
          val errors = results.collect { case Left(reasons) => reasons }.flatten
          if (errors.nonEmpty) Left(errors)
          else Right(Plan.Coproduct(id, tc, ordinal, results.collect { case Right(found) => found }))
        case EmptyK =>
          // Like kittens: exactly one subtype forms an EmptyK.
          variants.flatMap { case (name, variantLam) =>
            attempt(plan(tc, variantLam, isTop = false)).toOption.map(name -> _)
          } match {
            case (_, only) :: Nil => Right(Plan.Alias(id, only))
            case Nil              => Left(List(s"no subtype of ${lam.show} forms an EmptyK (exactly one has to)"))
            case several          =>
              Left(
                List(
                  s"several subtypes of ${lam.show} form an EmptyK (${several.map(_._1).mkString(", ")}), " +
                    "exactly one has to"
                )
              )
          }
        case _ =>
          Left(List(s"${tc.name} cannot be derived for a sealed trait (${lam.show}), only for case classes"))
      }
    }

    /** Field accessors, constructor and field types of a case class (or of a case object: no fields). */
    @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
    private def productParts(
        lam: Lam
    ): Option[Either[String, (Expr[Any => Array[Any]], Expr[Array[Any] => Any], List[(String, Lam)])]] = {
      implicit val AnyType: Type[Any] = ErasedAnyType
      implicit val ArrayAnyType: Type[Array[Any]] = ErasedArrayAnyType
      implicit val IntType: Type[Int] = ErasedIntType
      val whole: ?? = lam.any.as_??
      import whole.Underlying as T
      singleValueOf(lam.any) match {
        case Some(value) =>
          val valuesFn: Expr[Any => Array[Any]] = Expr.quote { (fa: Any) =>
            val _ = fa
            new Array[Any](0)
          }
          val constructFn: Expr[Array[Any] => Any] = Expr.quote { (values: Array[Any]) =>
            val _ = values
            Expr.splice(value)
          }
          Some(Right((valuesFn, constructFn, Nil)))
        case None =>
          (
            CaseClass.parse[T].toOption,
            CaseClass.parse(using lam.int).toOption,
            CaseClass.parse(using lam.string).toOption
          ) match {
            case (Some(ccAny), Some(ccInt), Some(ccString)) =>
              val paramsAny = ccAny.primaryConstructor.totalParameters.flatten.toList
              val paramsInt = ccInt.primaryConstructor.totalParameters.flatten.toList
              val paramsString = ccString.primaryConstructor.totalParameters.flatten.toList
              val accessible = ccAny.caseFieldValuesAt(Expr.quote(null.asInstanceOf[T])).size
              if (
                accessible != paramsAny.size || paramsInt.size != paramsAny.size || paramsString.size != paramsAny.size
              )
                Some(Left(s"${lam.show} has constructor parameters that are not accessible as fields"))
              else {
                val valuesBody: Expr[Any] => Expr[Array[Any]] = { fa =>
                  val value: Expr[T] = Expr.quote(Expr.splice(fa).asInstanceOf[T])
                  erasedArrayOf(ccAny.caseFieldValuesAt(value).toList.map { case (_, fieldValue) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                  })
                }
                val constructBody: Expr[Array[Any]] => Expr[Any] = { values =>
                  val args: Map[String, Expr_??] = paramsAny.zipWithIndex.map { case ((name, param), index) =>
                    name -> erasedCastTo(Expr.quote(Expr.splice(values)(Expr.splice(Expr(index)))), param.tpe)
                  }.toMap
                  runSafe(
                    constructInstanceFree(ccAny.primaryConstructor, "Constructor", Type[T].prettyPrint)(args)
                  ).value
                    .asInstanceOf[Expr[Any]]
                }
                val valuesFn: Expr[Any => Array[Any]] =
                  Expr.quote((fa: Any) => Expr.splice(valuesBody(Expr.quote(fa))))
                val constructFn: Expr[Array[Any] => Any] =
                  Expr.quote((values: Array[Any]) => Expr.splice(constructBody(Expr.quote(values))))
                val fields =
                  paramsInt.zip(paramsString).zip(paramsAny).map { case (((name, pInt), (_, pString)), (_, pAny)) =>
                    name -> Lam(
                      pInt.tpe.Underlying.asInstanceOf[Type[Any]],
                      pString.tpe.Underlying.asInstanceOf[Type[Any]],
                      pAny.tpe.Underlying.asInstanceOf[Type[Any]]
                    )
                  }
                Some(Right((valuesFn, constructFn, fields)))
              }
            case _ => None
          }
      }
    }

    /** The subtype ordinal function and the subtypes of a sealed trait / enum. */
    @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
    private def coproductParts(lam: Lam): Option[Either[String, (Expr[Any => Int], List[(String, Lam)])]] = {
      implicit val AnyType: Type[Any] = ErasedAnyType
      implicit val IntType: Type[Int] = ErasedIntType
      val whole: ?? = lam.any.as_??
      import whole.Underlying as T
      (Enum.parse[T].toOption, Enum.parse(using lam.int).toOption, Enum.parse(using lam.string).toOption) match {
        case (Some(enumAny), Some(enumInt), Some(enumString)) =>
          val childrenAny = enumAny.directChildren.toList
          val childrenInt = enumInt.directChildren.toList
          val childrenString = enumString.directChildren.toList
          if (childrenAny.isEmpty || childrenInt.size != childrenAny.size || childrenString.size != childrenAny.size)
            Some(Left(s"${lam.show} has no subtypes to derive for"))
          else {
            val ordinalBody: Expr[Any] => Expr[Int] = { fa =>
              val value: Expr[T] = Expr.quote(Expr.splice(fa).asInstanceOf[T])
              runSafe {
                enumAny
                  .matchOn[MIO, Int](value) { matched =>
                    import matched.Underlying as Child
                    val index = childrenAny.indexWhere { case (_, child) => child.Underlying =:= Type[Child] }
                    MIO.pure(Expr(index))
                  }
                  .flatMap {
                    case Some(found) => MIO.pure(found)
                    case None        => failDerivation[Expr[Int]](CatsDerivationError.EnumHasNoChildren(lam.show))
                  }
              }
            }
            val ordinalFn: Expr[Any => Int] = Expr.quote((fa: Any) => Expr.splice(ordinalBody(Expr.quote(fa))))
            val variants =
              childrenInt.zip(childrenString).zip(childrenAny).map { case (((name, cInt), (_, cString)), (_, cAny)) =>
                name -> Lam(
                  cInt.Underlying.asInstanceOf[Type[Any]],
                  cString.Underlying.asInstanceOf[Type[Any]],
                  cAny.Underlying.asInstanceOf[Type[Any]]
                )
              }
            Some(Right((ordinalFn, variants)))
          }
        case _ => None
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def erasedCastTo(value: Expr[Any], targetType: ??): Expr_?? = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import targetType.Underlying as T
    Expr.quote(Expr.splice(value).asInstanceOf[T]).as_??
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def erasedArrayOf(values: List[Expr[Any]]): Expr[Array[Any]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    implicit val ArrayAnyType: Type[Array[Any]] = ErasedArrayAnyType
    implicit val IntType: Type[Int] = ErasedIntType
    val empty: Expr[Array[Any]] = Expr.quote(new Array[Any](Expr.splice(Expr(values.size))))
    values.zipWithIndex.foldLeft(empty) { case (array, (value, index)) =>
      Expr.quote(
        hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
          .arraySet(Expr.splice(array), Expr.splice(Expr(index)), Expr.splice(value))
      )
    }
  }

  /** The code creating the instance a [[Plan]] describes; `registry` is where recursive instances are looked up. */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private def emitPlan(
      plan: Plan,
      registry: Expr[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry]
  ): Expr[Any] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    implicit val IntType: Type[Int] = ErasedIntType
    implicit val ArrayAnyType: Type[Array[Any]] = ErasedArrayAnyType
    implicit val RegistryType: Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
      ErasedRegistryType
    plan match {
      case Plan.Ready(instance)                 => instance
      case Plan.Self(id)                        => Expr.quote(Expr.splice(registry).get(Expr.splice(Expr(id))))
      case Plan.Composed(compose, outer, inner) => compose(emitPlan(outer, registry), emitPlan(inner, registry))
      case Plan.Alias(id, underlying)           =>
        val instance = emitPlan(underlying, registry)
        Expr.quote(Expr.splice(registry).set(Expr.splice(Expr(id)), Expr.splice(instance)))
      case Plan.Product(id, tc, values, construct, fields) =>
        val instancesBody: () => Expr[Array[Any]] = () => erasedArrayOf(fields.map(emitPlan(_, registry)))
        val instances: Expr[Any => Array[Any]] = Expr.quote { (unused: Any) =>
          val _ = unused
          Expr.splice(instancesBody())
        }
        val instance = productInstance(tc, values, construct, instances)
        Expr.quote(Expr.splice(registry).set(Expr.splice(Expr(id)), Expr.splice(instance)))
      case Plan.Coproduct(id, tc, ordinal, variants) =>
        val instancesBody: () => Expr[Array[Any]] = () => erasedArrayOf(variants.map(emitPlan(_, registry)))
        val instances: Expr[Any => Array[Any]] = Expr.quote { (unused: Any) =>
          val _ = unused
          Expr.splice(instancesBody())
        }
        val instance = coproductInstance(tc, ordinal, instances)
        Expr.quote(Expr.splice(registry).set(Expr.splice(Expr(id)), Expr.splice(instance)))
    }
  }

  private def productInstance(
      tc: ErasedTC,
      values: Expr[Any => Array[Any]],
      construct: Expr[Array[Any] => Any],
      instances: Expr[Any => Array[Any]]
  ): Expr[Any] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    tc match {
      case Functor =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productFunctor(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case Contravariant =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productContravariant(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case Invariant =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productInvariant(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case Foldable =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productFoldable(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case Traverse =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productTraverse(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case Reducible =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .reducibleInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case NonEmptyTraverse =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .nonEmptyTraverseInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case Apply =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .applyInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case Applicative =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .applicativeInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case Pure =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .pureInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case EmptyK =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productEmptyK(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case SemigroupK =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productSemigroupK(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case MonoidK =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .productMonoidK(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
        )
      case NonEmptyAlternative =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .nonEmptyAlternativeInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
      case Alternative =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime
            .alternativeInstance[hearth.kindlings.catsderivation.internal.runtime.HktWrappedRuntime.AnyF](
              Expr.splice(values),
              Expr.splice(construct),
              Expr.splice(instances)
            ): Any
        )
    }
  }

  private def coproductInstance(
      tc: ErasedTC,
      ordinal: Expr[Any => Int],
      instances: Expr[Any => Array[Any]]
  ): Expr[Any] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    import ErasedTC.*
    tc match {
      case Contravariant =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductContravariant(Expr.splice(ordinal), Expr.splice(instances))
        )
      case Invariant =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductInvariant(Expr.splice(ordinal), Expr.splice(instances))
        )
      case Foldable =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductFoldable(Expr.splice(ordinal), Expr.splice(instances))
        )
      case Traverse =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductTraverse(Expr.splice(ordinal), Expr.splice(instances))
        )
      case Reducible =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductReducible(Expr.splice(ordinal), Expr.splice(instances))
        )
      case NonEmptyTraverse =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductNonEmptyTraverse(Expr.splice(ordinal), Expr.splice(instances))
        )
      case _ =>
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
            .coproductFunctor(Expr.splice(ordinal), Expr.splice(instances))
        )
    }
  }

  /** Set by the Scala 2 entry points when `F` is a type alias (see `AliasAwareCtor1`). */
  protected var typeConstructorIsAlias: Boolean = false

  /** Whether `F[A]` nests type constructors (`G[H[A]]`): such types are derived by composing instances, not from the
    * fields or subtypes of the outer type.
    */
  protected def isNestedTypeConstructor[F[_]](implicit FCtor: Type.Ctor1[F]): Boolean = {
    val lam = lamOf(FCtor)
    !lam.isId && !lam.isConst && outerAndInner(lam).exists { case (_, inner) => !inner.isId }
  }

  /** Whether `F` is a case class whose fields are all either constant or a type constructor applied to the type
    * parameter (`G[A]`): the shapes the specialised SemigroupK / MonoidK / EmptyK derivations give kittens' meaning to.
    */
  protected def fieldsAreConstantOrApplied[F[_]](implicit FCtor: Type.Ctor1[F]): Boolean = {
    val lam = lamOf(FCtor)
    (CaseClass.parse(using lam.int).toOption, CaseClass.parse(using lam.string).toOption) match {
      case (Some(ccInt), Some(ccString)) =>
        ccInt.primaryConstructor.totalParameters.flatten.toList
          .zip(ccString.primaryConstructor.totalParameters.flatten.toList)
          .forall { case ((_, pInt), (_, pString)) =>
            val field = Lam(
              pInt.tpe.Underlying.asInstanceOf[Type[Any]],
              pString.tpe.Underlying.asInstanceOf[Type[Any]],
              pInt.tpe.Underlying.asInstanceOf[Type[Any]]
            )
            field.isConst || (typeArgumentsOf(field.int).size == 1 && outerAndInner(field).exists(_._2.isId))
          }
      case _ => false
    }
  }

  /** Derives the instance of `tc` for `F` with the general rules. */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  protected def deriveErasedInstance[F[_], Out: Type](tc: ErasedTC)(implicit FCtor: Type.Ctor1[F]): MIO[Expr[Out]] = {
    implicit val AnyType: Type[Any] = ErasedAnyType
    implicit val IntType: Type[Int] = ErasedIntType
    implicit val RegistryType: Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
      ErasedRegistryType
    MIO.scoped { runSafe =>
      val planner = new Planner(runSafe)
      val lam = lamOf(FCtor)
      planner.plan(tc, lam, isTop = true) match {
        case Right(plan) =>
          val size = planner.registrySize
          Expr.quote {
            hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
              .recursive(
                Expr.splice(Expr(size)),
                (registry: hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry) =>
                  Expr.splice(emitPlan(plan, Expr.quote(registry)))
              )
              .asInstanceOf[Out]
          }
        case Left(reasons) =>
          runSafe {
            failDerivation[Expr[Out]](
              CatsDerivationError.DerivationFailed(
                tc.name,
                (s"${lam.show} does not have the shape of a type kittens derives ${tc.name} for:" :: reasons
                  .map("  " + _)).mkString("\n")
              )
            )
          }
      }
    }
  }

  /** Runs the specialised derivation `specialised`, and the general one ([[deriveErasedInstance]]) when it fails (a
    * shape it does not generate code for). Types nesting type constructors go straight to the general one.
    */
  protected def orErasedInstance[F[_], Out: Type](tc: ErasedTC, specialisedApplies: Boolean = true)(
      specialised: => MIO[Expr[Out]]
  )(implicit FCtor: Type.Ctor1[F]): MIO[Expr[Out]] =
    if (!specialisedApplies || typeConstructorIsAlias || isNestedTypeConstructor[F]) deriveErasedInstance[F, Out](tc)
    else {
      val snapshot = instanceScopedValsSnapshot()
      tentatively(specialised).handleErrorWith { specialisedErrors =>
        restoreInstanceScopedVals(snapshot)
        deriveErasedInstance[F, Out](tc).handleErrorWith { generalErrors =>
          MIO.fail(specialisedErrors ++ generalErrors)
        }
      }
    }
}
