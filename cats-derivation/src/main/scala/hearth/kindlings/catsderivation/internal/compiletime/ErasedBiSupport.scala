package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** The general derivation of `Bifunctor`, `Bifoldable` and `Bitraverse` (kind `* -> * -> *`), following kittens' rules
  * for every shape of `F[A, B]` - the specialised derivations generate code for case classes / sealed traits whose
  * fields are `A`, `B` or constant, and fall back to this one for the rest:
  *
  *   - `A` / `B` themselves, a type mentioning neither (`Const`-like),
  *   - `G[X]` with an implicit `Functor` / `Foldable` / `Traverse` of `G` (kittens' `Left1` / `Right1`, generalised to
  *     any part `X`, e.g. `List[Either[A, B]]`),
  *   - `G[X, Y]` with an implicit `Bifunctor` / `Bifoldable` / `Bitraverse` of `G` (e.g. `Either[A, B]`),
  *   - otherwise a case class field by field, a sealed trait / enum by subtype (so nested data types need no instance
  *     in scope, and recursive types refer to the instance being derived).
  *
  * A type is represented by its application to the probes `(Int, Int)`, `(String, Int)`, `(Int, String)` and to
  * `(Any, Any)`, the last one being what the generated (erased) code works with. The instances are assembled at runtime
  * by `HktBiRuntime`.
  */
trait ErasedBiSupport
    extends CatsDerivationTimeout
    with CatsDerivationErrorSupport
    with hearth.kindlings.derivation.compiletime.InstanceScopedVals {
  this: MacroCommons & StdExtensions =>

  sealed abstract class ErasedBiTC(val name: String)
  object ErasedBiTC {
    case object Bifunctor extends ErasedBiTC("Bifunctor")
    case object Bifoldable extends ErasedBiTC("Bifoldable")
    case object Bitraverse extends ErasedBiTC("Bitraverse")
  }

  private lazy val BiIntType: Type[Int] = Type.of[Int]
  private lazy val BiStringType: Type[String] = Type.of[String]
  private lazy val BiAnyType: Type[Any] = Type.of[Any]
  private lazy val BiArrayAnyType: Type[Array[Any]] = Type.of[Array[Any]]
  private lazy val BiRegistryType: Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
    Type.of[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry]
  private type BiAnyK[X] = Any

  private lazy val BiFunctorK: Type.CtorK1[cats.Functor] = Type.CtorK1.of[cats.Functor]
  private lazy val BiFoldableK: Type.CtorK1[cats.Foldable] = Type.CtorK1.of[cats.Foldable]
  private lazy val BiTraverseK: Type.CtorK1[cats.Traverse] = Type.CtorK1.of[cats.Traverse]

  /** `F[_, _]` applied to the probes. */
  final private case class Lam2(ii: Type[Any], si: Type[Any], is: Type[Any], any: Type[Any]) {
    def isConst: Boolean = ii =:= si && ii =:= is
    def isLeft: Boolean = ii =:= BiIntType && si =:= BiStringType && is =:= BiIntType
    def isRight: Boolean = ii =:= BiIntType && si =:= BiIntType && is =:= BiStringType
    def sameAs(other: Lam2): Boolean = ii =:= other.ii && si =:= other.si && is =:= other.is
    def show: String = if (isLeft) "A" else if (isRight) "B" else any.prettyPrint
  }

  private def lam2Of[F[_, _]](ctor: Type.Ctor2[F]): Lam2 = Lam2(
    ctor.apply[Int, Int](using BiIntType, BiIntType).asInstanceOf[Type[Any]],
    ctor.apply[String, Int](using BiStringType, BiIntType).asInstanceOf[Type[Any]],
    ctor.apply[Int, String](using BiIntType, BiStringType).asInstanceOf[Type[Any]],
    ctor.apply[Any, Any](using BiAnyType, BiAnyType).asInstanceOf[Type[Any]]
  )

  sealed private trait BiPlan
  private object BiPlan {
    final case class Ready(instance: Expr[Any]) extends BiPlan
    final case class Self(id: Int) extends BiPlan
    final case class Map1(outer: Expr[Any], inner: BiPlan) extends BiPlan
    final case class Map2(outer: Expr[Any], first: BiPlan, second: BiPlan) extends BiPlan
    final case class Product(
        id: Int,
        values: Expr[Any => Array[Any]],
        construct: Expr[Array[Any] => Any],
        fields: List[BiPlan]
    ) extends BiPlan
    final case class Coproduct(id: Int, ordinal: Expr[Any => Int], variants: List[BiPlan]) extends BiPlan
  }

  private def summonBi1(tc: ErasedBiTC, ctor: Type.Ctor1[BiAnyK]): Option[Expr[Any]] = {
    def summon[TC[_[_]]](ctorK: Type.CtorK1[TC]): Option[Expr[Any]] = {
      implicit val InstanceType: Type[TC[BiAnyK]] = ctorK.apply[BiAnyK](using ctor)
      Expr.summonImplicit[TC[BiAnyK]].toOption.map(_.asInstanceOf[Expr[Any]])
    }
    tc match {
      case ErasedBiTC.Bifunctor  => summon(BiFunctorK)
      case ErasedBiTC.Bifoldable => summon(BiFoldableK)
      case ErasedBiTC.Bitraverse => summon(BiTraverseK)
    }
  }

  private def summonBi2(tc: ErasedBiTC, ctor: UntypedType): Option[Expr[Any]] = {
    val tcType: UntypedType = tc match {
      case ErasedBiTC.Bifunctor  => Type.of[cats.Bifunctor[Either]].asUntyped
      case ErasedBiTC.Bifoldable => Type.of[cats.Bifoldable[Either]].asUntyped
      case ErasedBiTC.Bitraverse => Type.of[cats.Bitraverse[Either]].asUntyped
    }
    val instanceType: Type[Any] = UntypedType.applyTypeArgs(tcType, List(ctor)).asTyped[Any]
    Expr.summonImplicit[Any](using instanceType).toOption.map(_.asInstanceOf[Expr[Any]])
  }

  private def decomposeAll1(lam: Lam2): Option[(Type.Ctor1[BiAnyK], Lam2)] =
    for {
      (cii, xii) <- Type.decompose1(using lam.ii)
      (csi, xsi) <- Type.decompose1(using lam.si)
      (cis, xis) <- Type.decompose1(using lam.is)
      (cany, xany) <- Type.decompose1(using lam.any)
      if csi.sameTypeConstructorAs(cii.asUntyped) && cis.sameTypeConstructorAs(cii.asUntyped) &&
        cany.sameTypeConstructorAs(cii.asUntyped)
    } yield (
      cii.asInstanceOf[Type.Ctor1[BiAnyK]],
      Lam2(
        xii.Underlying.asInstanceOf[Type[Any]],
        xsi.Underlying.asInstanceOf[Type[Any]],
        xis.Underlying.asInstanceOf[Type[Any]],
        xany.Underlying.asInstanceOf[Type[Any]]
      )
    )

  private def decomposeAll2(lam: Lam2): Option[(UntypedType, Lam2, Lam2)] =
    for {
      (cii, (xii, yii)) <- Type.decompose2(using lam.ii)
      (csi, (xsi, ysi)) <- Type.decompose2(using lam.si)
      (cis, (xis, yis)) <- Type.decompose2(using lam.is)
      (cany, (xany, yany)) <- Type.decompose2(using lam.any)
      if csi.sameTypeConstructorAs(cii.asUntyped) && cis.sameTypeConstructorAs(cii.asUntyped) &&
        cany.sameTypeConstructorAs(cii.asUntyped)
    } yield (
      cii.asUntyped,
      Lam2(
        xii.Underlying.asInstanceOf[Type[Any]],
        xsi.Underlying.asInstanceOf[Type[Any]],
        xis.Underlying.asInstanceOf[Type[Any]],
        xany.Underlying.asInstanceOf[Type[Any]]
      ),
      Lam2(
        yii.Underlying.asInstanceOf[Type[Any]],
        ysi.Underlying.asInstanceOf[Type[Any]],
        yis.Underlying.asInstanceOf[Type[Any]],
        yany.Underlying.asInstanceOf[Type[Any]]
      )
    )

  private def biLeft: Expr[Any] = {
    implicit val AnyType: Type[Any] = BiAnyType
    Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime.left)
  }
  private def biRight: Expr[Any] = {
    implicit val AnyType: Type[Any] = BiAnyType
    Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime.right)
  }
  private def biConst: Expr[Any] = {
    implicit val AnyType: Type[Any] = BiAnyType
    Expr.quote(hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime.const)
  }

  final private class BiPlanner(runSafe: hearth.fp.DirectStyle.RunSafe[MIO], tc: ErasedBiTC) {
    private var nextId: Int = 0
    private var inProgress: List[(Lam2, Int)] = Nil

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

    def plan(lam: Lam2, isTop: Boolean): Either[List[String], BiPlan] =
      if (lam.isLeft) Right(BiPlan.Ready(biLeft))
      else if (lam.isRight) Right(BiPlan.Ready(biRight))
      else if (lam.isConst) Right(BiPlan.Ready(biConst))
      else
        inProgress.collectFirst { case (enclosing, id) if enclosing.sameAs(lam) => id } match {
          case Some(id) => Right(BiPlan.Self(id))
          case None     =>
            val viaInstances: Option[Either[List[String], BiPlan]] =
              decomposeAll2(lam) match {
                // The type being derived itself (`F[A, B]`) is not summoned (it would find the instance being defined).
                case Some((_, first, second)) if isTop && first.isLeft && second.isRight => None
                case Some((ctor, first, second))                                         =>
                  summonBi2(tc, ctor).map { outer =>
                    attempt(for {
                      x <- plan(first, isTop = false)
                      y <- plan(second, isTop = false)
                    } yield BiPlan.Map2(instanceScopedAny(outer), x, y))
                  }
                case None =>
                  decomposeAll1(lam).flatMap { case (ctor, inner) =>
                    summonBi1(tc, ctor).map { outer =>
                      attempt(plan(inner, isTop = false).map(BiPlan.Map1(instanceScopedAny(outer), _)))
                    }
                  }
              }
            viaInstances match {
              case Some(Right(found)) => Right(found)
              case Some(Left(errors)) => generic(lam).left.map(errors ++ _)
              case None               => generic(lam)
            }
        }

    private def generic(lam: Lam2): Either[List[String], BiPlan] = {
      val id = nextId
      nextId += 1
      inProgress = (lam, id) :: inProgress
      try
        productParts(lam) match {
          case Some(Right((values, construct, fields))) =>
            val results = fields.map { case (name, fieldLam) =>
              plan(fieldLam, isTop = false).left.map(reasons => s"field $name of ${lam.show}:" :: reasons.map("  " + _))
            }
            val errors = results.collect { case Left(reasons) => reasons }.flatten
            if (errors.nonEmpty) Left(errors)
            else Right(BiPlan.Product(id, values, construct, results.collect { case Right(p) => p }))
          case Some(Left(reason)) => Left(List(reason))
          case None               =>
            coproductParts(lam) match {
              case Some(Right((ordinal, variants))) =>
                val results = variants.map { case (name, variantLam) =>
                  plan(variantLam, isTop = false).left
                    .map(reasons => s"subtype $name of ${lam.show}:" :: reasons.map("  " + _))
                }
                val errors = results.collect { case Left(reasons) => reasons }.flatten
                if (errors.nonEmpty) Left(errors)
                else Right(BiPlan.Coproduct(id, ordinal, results.collect { case Right(p) => p }))
              case Some(Left(reason)) => Left(List(reason))
              case None               =>
                Left(List(s"${lam.show} is neither a case class nor a sealed trait, and has no ${tc.name}"))
            }
        }
      finally inProgress = inProgress.tail
    }

    @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
    private def productParts(
        lam: Lam2
    ): Option[Either[String, (Expr[Any => Array[Any]], Expr[Array[Any] => Any], List[(String, Lam2)])]] = {
      implicit val AnyType: Type[Any] = BiAnyType
      implicit val ArrayAnyType: Type[Array[Any]] = BiArrayAnyType
      implicit val IntType: Type[Int] = BiIntType
      val whole: ?? = lam.any.as_??
      import whole.Underlying as T
      SingletonValue.unapply(Type[T]).map(_.singletonExpr.upcast[Any]) match {
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
            CaseClass.parse(using lam.ii).toOption,
            CaseClass.parse(using lam.si).toOption,
            CaseClass.parse(using lam.is).toOption
          ) match {
            case (Some(ccAny), Some(ccII), Some(ccSI), Some(ccIS)) =>
              val paramsAny = ccAny.primaryConstructor.totalParameters.flatten.toList
              val paramsII = ccII.primaryConstructor.totalParameters.flatten.toList
              val paramsSI = ccSI.primaryConstructor.totalParameters.flatten.toList
              val paramsIS = ccIS.primaryConstructor.totalParameters.flatten.toList
              val accessible = ccAny.caseFieldValuesAt(Expr.quote(null.asInstanceOf[T])).size
              if (
                accessible != paramsAny.size || paramsII.size != paramsAny.size || paramsSI.size != paramsAny.size ||
                paramsIS.size != paramsAny.size
              )
                Some(Left(s"${lam.show} has constructor parameters that are not accessible as fields"))
              else {
                val valuesBody: Expr[Any] => Expr[Array[Any]] = { fa =>
                  val value: Expr[T] = Expr.quote(Expr.splice(fa).asInstanceOf[T])
                  biArrayOf(ccAny.caseFieldValuesAt(value).toList.map { case (_, fieldValue) =>
                    import fieldValue.Underlying as Field
                    fieldValue.value.asInstanceOf[Expr[Field]].upcast[Any]
                  })
                }
                val constructBody: Expr[Array[Any]] => Expr[Any] = { values =>
                  val args: Map[String, Expr_??] = paramsAny.zipWithIndex.map { case ((name, param), index) =>
                    name -> biCastTo(Expr.quote(Expr.splice(values)(Expr.splice(Expr(index)))), param.tpe)
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
                val fields = paramsAny.indices.toList.map { i =>
                  paramsAny(i)._1 -> Lam2(
                    paramsII(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
                    paramsSI(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
                    paramsIS(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
                    paramsAny(i)._2.tpe.Underlying.asInstanceOf[Type[Any]]
                  )
                }
                Some(Right((valuesFn, constructFn, fields)))
              }
            case _ => None
          }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
    private def coproductParts(lam: Lam2): Option[Either[String, (Expr[Any => Int], List[(String, Lam2)])]] = {
      implicit val AnyType: Type[Any] = BiAnyType
      implicit val IntType: Type[Int] = BiIntType
      val whole: ?? = lam.any.as_??
      import whole.Underlying as T
      (
        parseEnum[T].toOption,
        parseEnum(using lam.ii).toOption,
        parseEnum(using lam.si).toOption,
        parseEnum(using lam.is).toOption
      ) match {
        case (Some(enumAny), Some(enumII), Some(enumSI), Some(enumIS)) =>
          val childrenAny = enumAny.directChildren.toList
          val childrenII = enumII.directChildren.toList
          val childrenSI = enumSI.directChildren.toList
          val childrenIS = enumIS.directChildren.toList
          if (
            childrenAny.isEmpty || childrenII.size != childrenAny.size || childrenSI.size != childrenAny.size ||
            childrenIS.size != childrenAny.size
          )
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
            val variants = childrenAny.indices.toList.map { i =>
              childrenAny(i)._1 -> Lam2(
                childrenII(i)._2.Underlying.asInstanceOf[Type[Any]],
                childrenSI(i)._2.Underlying.asInstanceOf[Type[Any]],
                childrenIS(i)._2.Underlying.asInstanceOf[Type[Any]],
                childrenAny(i)._2.Underlying.asInstanceOf[Type[Any]]
              )
            }
            Some(Right((ordinalFn, variants)))
          }
        case _ => None
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def biCastTo(value: Expr[Any], targetType: ??): Expr_?? = {
    implicit val AnyType: Type[Any] = BiAnyType
    import targetType.Underlying as T
    Expr.quote(Expr.splice(value).asInstanceOf[T]).as_??
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter")
  private def biArrayOf(values: List[Expr[Any]]): Expr[Array[Any]] = {
    implicit val AnyType: Type[Any] = BiAnyType
    implicit val ArrayAnyType: Type[Array[Any]] = BiArrayAnyType
    implicit val IntType: Type[Int] = BiIntType
    val empty: Expr[Array[Any]] = Expr.quote(new Array[Any](Expr.splice(Expr(values.size))))
    values.zipWithIndex.foldLeft(empty) { case (array, (value, index)) =>
      Expr.quote(
        hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
          .arraySet(Expr.splice(array), Expr.splice(Expr(index)), Expr.splice(value))
      )
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private def emitBiPlan(
      plan: BiPlan,
      registry: Expr[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry]
  ): Expr[Any] = {
    implicit val AnyType: Type[Any] = BiAnyType
    implicit val IntType: Type[Int] = BiIntType
    implicit val ArrayAnyType: Type[Array[Any]] = BiArrayAnyType
    implicit val RegistryType: Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
      BiRegistryType
    plan match {
      case BiPlan.Ready(instance)    => instance
      case BiPlan.Self(id)           => Expr.quote(Expr.splice(registry).get(Expr.splice(Expr(id))))
      case BiPlan.Map1(outer, inner) =>
        val in = emitBiPlan(inner, registry)
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime.map1(Expr.splice(outer), Expr.splice(in))
        )
      case BiPlan.Map2(outer, first, second) =>
        val x = emitBiPlan(first, registry)
        val y = emitBiPlan(second, registry)
        Expr.quote(
          hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime
            .map2(Expr.splice(outer), Expr.splice(x), Expr.splice(y))
        )
      case BiPlan.Product(id, values, construct, fields) =>
        val instancesBody: () => Expr[Array[Any]] = () => biArrayOf(fields.map(emitBiPlan(_, registry)))
        val instances: Expr[Any => Array[Any]] = Expr.quote { (unused: Any) =>
          val _ = unused
          Expr.splice(instancesBody())
        }
        Expr.quote(
          Expr
            .splice(registry)
            .set(
              Expr.splice(Expr(id)),
              hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime
                .product(Expr.splice(values), Expr.splice(construct), Expr.splice(instances))
            )
        )
      case BiPlan.Coproduct(id, ordinal, variants) =>
        val instancesBody: () => Expr[Array[Any]] = () => biArrayOf(variants.map(emitBiPlan(_, registry)))
        val instances: Expr[Any => Array[Any]] = Expr.quote { (unused: Any) =>
          val _ = unused
          Expr.splice(instancesBody())
        }
        Expr.quote(
          Expr
            .splice(registry)
            .set(
              Expr.splice(Expr(id)),
              hearth.kindlings.catsderivation.internal.runtime.HktBiRuntime
                .coproduct(Expr.splice(ordinal), Expr.splice(instances))
            )
        )
    }
  }

  /** Whether `F` is a case class, or a sealed trait of case classes / objects, whose fields are all `A`, `B` or
    * constant: the shapes the specialised derivations generate code for. Everything else goes to
    * [[deriveErasedBiInstance]].
    */
  protected def biFieldsAreDirect[F[_, _]](implicit FCtor: Type.Ctor2[F]): Boolean = {
    val lam = lam2Of(FCtor)
    def fieldsDirect(ii: Type[Any], si: Type[Any], is: Type[Any]): Boolean =
      (
        CaseClass.parse(using ii).toOption,
        CaseClass.parse(using si).toOption,
        CaseClass.parse(using is).toOption
      ) match {
        case (Some(cII), Some(cSI), Some(cIS)) =>
          val pII = cII.primaryConstructor.totalParameters.flatten.toList
          val pSI = cSI.primaryConstructor.totalParameters.flatten.toList
          val pIS = cIS.primaryConstructor.totalParameters.flatten.toList
          pII.size == pSI.size && pII.size == pIS.size && pII.indices.forall { i =>
            val field = Lam2(
              pII(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
              pSI(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
              pIS(i)._2.tpe.Underlying.asInstanceOf[Type[Any]],
              BiAnyType
            )
            field.isConst || field.isLeft || field.isRight
          }
        case _ =>
          val single: ?? = ii.as_??
          import single.Underlying as S
          SingletonValue.unapply(Type[S]).isDefined
      }
    if (CaseClass.parse(using lam.any).toOption.isDefined) fieldsDirect(lam.ii, lam.si, lam.is)
    else
      (parseEnum(using lam.ii).toOption, parseEnum(using lam.si).toOption, parseEnum(using lam.is).toOption) match {
        case (Some(eII), Some(eSI), Some(eIS)) =>
          val cII = eII.directChildren.toList
          val cSI = eSI.directChildren.toList
          val cIS = eIS.directChildren.toList
          cII.nonEmpty && cII.size == cSI.size && cII.size == cIS.size && cII.indices.forall { i =>
            fieldsDirect(
              cII(i)._2.Underlying.asInstanceOf[Type[Any]],
              cSI(i)._2.Underlying.asInstanceOf[Type[Any]],
              cIS(i)._2.Underlying.asInstanceOf[Type[Any]]
            )
          }
        case _ => false
      }
  }

  /** Derives the instance of `tc` for `F` with the general rules. */
  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  protected def deriveErasedBiInstance[F[_, _], Out: Type](
      tc: ErasedBiTC
  )(implicit FCtor: Type.Ctor2[F]): MIO[Expr[Out]] = {
    implicit val AnyType: Type[Any] = BiAnyType
    implicit val IntType: Type[Int] = BiIntType
    implicit val RegistryType: Type[hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry] =
      BiRegistryType
    MIO.scoped { runSafe =>
      val planner = new BiPlanner(runSafe, tc)
      val lam = lam2Of(FCtor)
      planner.plan(lam, isTop = true) match {
        case Right(plan) =>
          val size = planner.registrySize
          Expr.quote {
            hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime
              .recursive(
                Expr.splice(Expr(size)),
                (registry: hearth.kindlings.catsderivation.internal.runtime.HktGenericRuntime.Registry) =>
                  Expr.splice(emitBiPlan(plan, Expr.quote(registry)))
              )
              .asInstanceOf[Out]
          }
        case Left(reasons) =>
          runSafe {
            failDerivation[Expr[Out]](
              CatsDerivationError.DerivationFailed(
                tc.name,
                (s"${lam.any.prettyPrint} does not have the shape of a type kittens derives ${tc.name} for:" :: reasons
                  .map("  " + _)).mkString("\n")
              )
            )
          }
      }
    }
  }
}
