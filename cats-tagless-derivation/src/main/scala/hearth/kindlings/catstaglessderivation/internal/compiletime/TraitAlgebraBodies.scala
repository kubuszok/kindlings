package hearth.kindlings.catstaglessderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

/** Builds the transformed trait algebra (`new Alg[G] { ... }`) of `mapK` / `contramapK` / `imapK` / `productK` /
  * `instrument`, the way cats-tagless' `Derive.*` does it: every overridable (non-final) member delegates to the source
  * algebra(s), and each parameter and result type in which the algebra's `F` occurs is transformed:
  *
  *   - `F[A]` itself - with the natural transformation (`fk(fa)`), or a `Tuple2K` projection / constructor,
  *   - `T[F]` where `F` is nested in another type (`EitherT[F, E, A]`, `Kleisli[F, A, B]`, `Cokleisli[F, A, B]`,
  *     another algebra `Alg2[F]`, ...) - with the `FunctorK` / `ContravariantK` / `InvariantK` / `SemigroupalK` of
  *     `[F[_]] =>> T[F]`, summoned (cats-tagless' companion instances, user instances) or, for another algebra, derived
  *     recursively,
  *   - vararg parameters element by element.
  *
  * Members whose signature does not mention `F` are forwarded unchanged (`productK` combines their results with their
  * `Semigroup` when there is one). Where `F` occurs is found by comparing the member's signature in three applications
  * of the algebra: `Alg[Option]` and `Alg[List]` (does it depend on `F`? is it `F[A]`?) and `Alg[KProbe]` (to build the
  * type lambda). A shape cats-tagless rejects (`F` in a parameter of a `FunctorK`, `List[F[A]]` with no `FunctorK`) is
  * a compile error here too.
  */
trait TraitAlgebraBodies { this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  import hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessFactories

  // --- Shapes ---

  /** Where the algebra's `F` occurs in a parameter or result type. */
  sealed trait Occ extends Product with Serializable
  object Occ {
    case object Absent extends Occ
    case object Direct extends Occ

    /** `F` nested in another type; `probeType` is that type in `Alg[KProbe]`. */
    final case class Nested(probeType: UntypedType, printed: String) extends Occ
  }

  final case class ParamShape(name: String, occ: Occ, isVararg: Boolean)
  final case class MethodShape(key: String, name: String, params: List[ParamShape], result: Occ)

  /** The type classes looked up for nested occurrences. */
  sealed abstract class NestedTc(val name: String) extends Product with Serializable {
    def anchor: Type[Any]
  }
  object NestedTc {
    case object FunctorKTc extends NestedTc("FunctorK") {
      def anchor: Type[Any] = Type.of[cats.tagless.FunctorK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
    }
    case object ContravariantKTc extends NestedTc("ContravariantK") {
      def anchor: Type[Any] =
        Type.of[cats.tagless.ContravariantK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
    }
    case object InvariantKTc extends NestedTc("InvariantK") {
      def anchor: Type[Any] = Type.of[cats.tagless.InvariantK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
    }
    case object SemigroupalKTc extends NestedTc("SemigroupalK") {
      def anchor: Type[Any] =
        Type.of[cats.tagless.SemigroupalK[CatsTaglessFactories.DummyHKT]].asInstanceOf[Type[Any]]
    }
  }

  /** The transformation being derived; `sources` are the algebra(s) delegated to. */
  sealed trait TraitOp extends Product with Serializable
  object TraitOp {
    final case class MapK(af: Expr[Any], fk: Expr[Any]) extends TraitOp
    final case class ContramapK(af: Expr[Any], fk: Expr[Any]) extends TraitOp
    final case class ImapK(af: Expr[Any], fk: Expr[Any], gk: Expr[Any]) extends TraitOp
    final case class ProductK(af: Expr[Any], ag: Expr[Any]) extends TraitOp
    final case class Instrument(af: Expr[Any], algebraName: String) extends TraitOp
  }

  /** Derives `TC[X]` for another algebra `X` (the type constructor in `X[F]`) when the current derivation can; `None`
    * makes it a plain implicit lookup.
    */
  type NestedAlgebraDeriver = (NestedTc, UntypedType) => Option[MIO[Expr[Any]]]

  protected def lambdaOverProbe(tpe: UntypedType): UntypedType =
    TypeLambdaOverProbe(this)(tpe).asInstanceOf[UntypedType]

  /** The type of a parameter without its by-name wrapper. `Parameter.byNameUnderlying` keeps the algebra's own type
    * parameters unsubstituted (`Alg#F[Unit]` in `Alg[Option]`), while `Parameter.tpe` (`=> Option[Unit]`) has them
    * substituted, so the wrapper is removed from the latter.
    */
  private def paramType(p: Parameter): ?? =
    if (p.isByName)
      TypeLambdaOverProbe.byNameUnderlying(this)(p.tpe.Underlying.asUntyped).asInstanceOf[UntypedType].as_??
    else p.tpe

  private lazy val KProbeCtor: Type.Ctor1[CatsTaglessFactories.KProbe] = Type.Ctor1.of[CatsTaglessFactories.KProbe]
  private lazy val KProbeAppliedUntyped: UntypedType = Type.of[CatsTaglessFactories.KProbe[Any]].asUntyped

  private val defaultArgumentGetter = ".*\\$default\\$\\d+".r

  /** The members a derived instance overrides, like cats-tagless: everything but `final` members, members of
    * `Any`/`AnyRef`, default-argument getters and `var`s.
    */
  private[compiletime] def overridableMembers[A](ai: AnonymousInstance[A]): List[ClassifiedMethod] = {
    val excludedOwners = List(Type.of[Any].as_??, Type.of[AnyRef].as_??, Type.of[AnyVal].as_??)
    (ai.mustOverride ++ ai.diamondConflicts ++ ai.mayOverride).filterNot { cm =>
      val m = cm.method
      // Only instance members (the companion's members are listed too).
      !m.isInstanceOf[Method.OnInstance] ||
      excludedOwners.exists(o => cm.declaredIn.Underlying =:= o.Underlying) ||
      defaultArgumentGetter.matches(m.name) || m.isVar ||
      // A concrete `lazy val` keeps its own implementation (an anonymous instance cannot override it lazily).
      (m.isLazy && !m.isAbstract) || m.name.endsWith("_=") || m.isSynthetic ||
      m.name == "<init>" || m.name == "$init$"
    }
  }

  /** Keys members by name and position among same-named members, so that overloads match across applications. */
  private[compiletime] def keyed(members: List[ClassifiedMethod]): List[(String, ClassifiedMethod)] = {
    val seen = scala.collection.mutable.Map.empty[String, Int]
    members.map { cm =>
      val n = cm.method.name
      val i = seen.getOrElse(n, 0)
      seen.update(n, i + 1)
      (s"$n#$i", cm)
    }
  }

  /** Parameters (with the method's type parameters applied as `Any`) and result type of a probe method. */
  private def probeSignature(method: Method): (List[Parameter], Option[??]) = {
    implicit val AnyType: Type[Any] = InvariantKTypes.Any
    val AnyT: ?? = Type.of[Any].as_??
    def nullOf(t: ??): Expr_?? = {
      import t.Underlying as T
      Expr.quote(null.asInstanceOf[T]).as_??
    }
    def loop(m: Method, acc: List[Parameter]): (List[Parameter], Option[??]) = m match {
      case oi: Method.OnInstance =>
        // Only the types are inspected; this receiver is never emitted.
        loop(oi.applyUntyped(Expr.quote(null.asInstanceOf[Any]).asUntyped), acc)
      case at: Method.ApplyTypes =>
        loop(at(at.typeParams.flatten.map(_ -> AnyT).toMap), acc)
      case av: Method.ApplyValues =>
        val ps = av.parameters.flatten.toList
        val args: Arguments = ps.map { case (n, p) => n -> nullOf(paramType(p)) }.toMap
        val next =
          try Some(av(args))
          catch { case scala.util.control.NonFatal(_) => None }
        next match {
          case Some(nm) if !(nm eq av) => loop(nm, acc ++ ps.map(_._2))
          case _                       => (acc ++ ps.map(_._2), av.knownReturning)
        }
      case other => (acc, other.knownReturning)
    }
    val (params, result) = loop(method, Nil)
    // Fall back to the declared parameter types when staging did not reach every clause.
    val all = method.totalParameters.flatten.toList.map(_._2)
    // Staged parameters are only needed to substitute a polymorphic method's type parameters.
    val isPolymorphic = method.expectations.exists {
      case _: MethodExpectation.NeedsTypes => true
      case _                               => false
    }
    (if (isPolymorphic && params.size == all.size) params else all, result.orElse(knownReturningOfProbe(method)))
  }

  private def sameType(a: ??, b: ??): Boolean =
    a.Underlying =:= b.Underlying || a.Underlying.plainPrint == b.Underlying.plainPrint

  private def elementOf(t: ??, isVararg: Boolean): ?? = if (!isVararg) t
  else
    UntypedType.typeArguments(t.Underlying.asUntyped) match {
      case elem :: Nil => elem.as_??
      case _           => t
    }

  private def classify(opt: ??, list: ??, probe: ??): Occ =
    if (sameType(opt, list)) Occ.Absent
    else {
      val OptionCtor = Type.Ctor1.of[Option]
      val ListCtor = Type.Ctor1.of[List]
      val isDirect = (
        OptionCtor.unapply(opt.Underlying.asInstanceOf[Type[Any]]),
        ListCtor.unapply(list.Underlying.asInstanceOf[Type[Any]]),
        KProbeCtor.unapply(probe.Underlying.asInstanceOf[Type[Any]])
      ) match {
        case (Some(io), Some(il), Some(_)) => sameType(io, il)
        case _                             => false
      }
      if (isDirect) Occ.Direct else Occ.Nested(probe.Underlying.asUntyped, probe.Underlying.prettyPrint)
    }

  /** The shape of every overridable member of `Alg`, keyed like [[keyed]]. */
  private[compiletime] def analyseTraitAlgebra[Alg[_[_]]](
      algCtorK1: Type.CtorK1[Alg]
  ): MIO[Map[String, MethodShape]] = {
    val AlgOptionType = algCtorK1.apply[Option](using Type.Ctor1.of[Option]).asInstanceOf[Type[Any]]
    val AlgListType = algCtorK1.apply[List](using Type.Ctor1.of[List]).asInstanceOf[Type[Any]]
    val AlgProbeType = algCtorK1.apply[CatsTaglessFactories.KProbe](using KProbeCtor).asInstanceOf[Type[Any]]
    parsedOrFail(AnonymousInstance.parse(using AlgOptionType).toEither, "Alg[Option]")
      .parTuple(parsedOrFail(AnonymousInstance.parse(using AlgListType).toEither, "Alg[List]"))
      .parTuple(parsedOrFail(AnonymousInstance.parse(using AlgProbeType).toEither, "Alg[KProbe]"))
      .map { case ((aiOption, aiList), aiProbe) =>
        val listByKey = keyed(overridableMembers(aiList)).toMap
        val probeByKey = keyed(overridableMembers(aiProbe)).toMap
        keyed(overridableMembers(aiOption)).flatMap { case (key, om) =>
          for {
            lm <- listByKey.get(key)
            pm <- probeByKey.get(key)
          } yield {
            val (optParams, optResult) = probeSignature(om.method)
            val (listParams, listResult) = probeSignature(lm.method)
            val (probeParams, probeResult) = probeSignature(pm.method)
            val params = optParams.lazyZip(listParams).lazyZip(probeParams).toList.map { case (o, l, p) =>
              val vararg = o.isVararg
              def tpe(param: Parameter): ?? = elementOf(paramType(param), vararg)
              ParamShape(o.name, classify(tpe(o), tpe(l), tpe(p)), vararg)
            }
            val result = (optResult, listResult, probeResult) match {
              case (Some(o), Some(l), Some(p)) => classify(o, l, p)
              case _                           => Occ.Absent
            }
            key -> MethodShape(key, om.method.name, params, result)
          }
        }.toMap
      }
  }

  // --- Nested occurrences ---

  /** `X` when `probeType` is `X[KProbe]` (another algebra applied to `F`). */
  private def etaReduced(probeType: UntypedType): Option[UntypedType] = {
    val dealiased = UntypedType.dealias(probeType)
    UntypedType.typeArguments(dealiased) match {
      case arg :: Nil
          if UntypedType.sameTypeConstructorAs(arg, KProbeAppliedUntyped) &&
            UntypedType.typeArguments(arg).isEmpty =>
        Some(UntypedType.typeConstructor(dealiased))
      case _ => None
    }
  }

  private def resolveNested(
      typeClass: String,
      methodName: String,
      position: String,
      tc: NestedTc,
      nested: Occ.Nested,
      deriveNested: NestedAlgebraDeriver
  ): MIO[Expr[Any]] = {
    val eta = etaReduced(nested.probeType)
    eta.flatMap(ctor => deriveNested(tc, ctor)) match {
      case Some(derived) => derived
      case None          =>
        val lambda = eta.getOrElse(lambdaOverProbe(nested.probeType))
        val tcType = applyTypeClassToAlgebra[Any, Any](tc.anchor, lambda)
        tcType.summonExprIgnoring().toEither match {
          case Right(instance) => MIO.pure(instanceScopedAny(instance.asInstanceOf[Expr[Any]]))
          case Left(_)         =>
            val err = CatsTaglessDerivationError.UnsupportedMethod(
              typeClass,
              methodName,
              s"has F nested in its $position type ${nested.printed.replace("KProbe", "F")}: " +
                s"no implicit ${tc.name}[${nested.printed.replace("KProbe", "F")} as a function of F] was found " +
                s"(cats-tagless provides them for EitherT, Kleisli, OptionT, IorT, WriterT, Func, Cokleisli, ...)"
            )
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }

  // --- Building the instance ---

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private[compiletime] def buildTraitAlgebra[Alg[_[_]]](
      typeClass: String,
      algCtorK1: Type.CtorK1[Alg],
      targetType: Type[Any],
      sourceTypes: List[Type[Any]],
      op: TraitOp,
      deriveNested: NestedAlgebraDeriver
  ): MIO[Expr[Any]] = {
    import NestedTc.*

    def unsupported(methodName: String, reason: String): CatsTaglessDerivationError =
      CatsTaglessDerivationError.UnsupportedMethod(typeClass, methodName, reason)

    // Which instance a nested occurrence needs (None: the shape is not supported by this transformation).
    def paramTc(occ: Occ): Either[String, Option[NestedTc]] = (op, occ) match {
      case (_, Occ.Absent)               => Right(None)
      case (_: TraitOp.MapK, Occ.Direct) =>
        Left("has F in parameter position: FunctorK needs F only in results (use InvariantK)")
      case (_: TraitOp.MapK, _: Occ.Nested)       => Right(Some(ContravariantKTc))
      case (_: TraitOp.ContramapK, Occ.Direct)    => Right(None)
      case (_: TraitOp.ContramapK, _: Occ.Nested) => Right(Some(FunctorKTc))
      case (_: TraitOp.ImapK, Occ.Direct)         => Right(None)
      case (_: TraitOp.ImapK, _: Occ.Nested)      => Right(Some(InvariantKTc))
      case (_: TraitOp.ProductK, Occ.Direct)      => Right(None)
      case (_: TraitOp.ProductK, _: Occ.Nested)   => Right(Some(FunctorKTc))
      case (_: TraitOp.Instrument, _)             =>
        Left("has F in parameter position: Instrument needs F only in results")
    }
    def resultTc(occ: Occ): Either[String, Option[NestedTc]] = (op, occ) match {
      case (_, Occ.Absent)                     => Right(None)
      case (_: TraitOp.MapK, Occ.Direct)       => Right(None)
      case (_: TraitOp.MapK, _: Occ.Nested)    => Right(Some(FunctorKTc))
      case (_: TraitOp.ContramapK, Occ.Direct) =>
        Left("returns F[A] (F in covariant position): ContravariantK needs F only in parameters (use InvariantK)")
      case (_: TraitOp.ContramapK, _: Occ.Nested) => Right(Some(ContravariantKTc))
      case (_: TraitOp.ImapK, Occ.Direct)         => Right(None)
      case (_: TraitOp.ImapK, _: Occ.Nested)      => Right(Some(InvariantKTc))
      case (_: TraitOp.ProductK, Occ.Direct)      => Right(None)
      case (_: TraitOp.ProductK, _: Occ.Nested)   => Right(Some(SemigroupalKTc))
      case (_: TraitOp.Instrument, Occ.Direct)    => Right(None)
      case (_: TraitOp.Instrument, n: Occ.Nested) =>
        Left(s"returns ${n.printed.replace("KProbe", "F")}: Instrument needs results of type F[A]")
    }

    // Resolves every nested instance up front (override bodies are synchronous), aggregating errors.
    def resolveAll(shapes: List[MethodShape]): MIO[Map[(String, Int), Expr[Any]]] =
      shapes
        .flatMap { shape =>
          val params = shape.params.zipWithIndex.map { case (p, i) => (i, p.occ, paramTc(p.occ), "parameter") }
          val result = List((-1, shape.result, resultTc(shape.result), "result"))
          (params ++ result).map { case (pos, occ, tc, where) => (shape, pos, occ, tc, where) }
        }
        .parTraverse {
          case (shape, _, _, Left(reason), _) =>
            val err = unsupported(shape.name, reason)
            Log.error(err.message) >> MIO.fail[Option[((String, Int), Expr[Any])]](err)
          case (shape, pos, n: Occ.Nested, Right(Some(tc)), where) =>
            resolveNested(typeClass, shape.name, where, tc, n, deriveNested).map(e => Some((shape.key, pos) -> e))
          case _ => MIO.pure(Option.empty[((String, Int), Expr[Any])])
        }
        .map(_.flatten.toMap)

    val sourcesParsed: MIO[List[AnonymousInstance[Any]]] =
      sourceTypes.zipWithIndex.parTraverse { case (t, i) =>
        parsedOrFail(AnonymousInstance.parse(using t).toEither, s"source algebra #${i + 1}")
      }

    parsedOrFail(AnonymousInstance.parse(using targetType).toEither, "target algebra")
      .parTuple(sourcesParsed)
      .parTuple(analyseTraitAlgebra[Alg](algCtorK1))
      .flatMap { case ((target, sources), shapes) =>
        val targetMembers = keyed(overridableMembers(target))
        val sourceMembers = sources.map(s => keyed(overridableMembers(s)).toMap)
        val missing = targetMembers.collect {
          case (key, cm) if !shapes.contains(key) || sourceMembers.exists(!_.contains(key)) =>
            CatsTaglessDerivationError.SourceMethodNotFound(cm.method.name)
        }
        missing match {
          case head :: tail => Log.error((head :: tail).map(_.message).mkString("\n")) >> MIO.fail(head, tail*)
          case Nil          =>
            resolveAll(targetMembers.map { case (key, _) => shapes(key) }).flatMap { instances =>
              val overrides: Map[UntypedMethod, OverrideBody] = targetMembers.map { case (key, cm) =>
                val shape = shapes(key)
                cm.method.asUntyped -> overrideBody(op, shape, sourceMembers.map(_(key).method), instances)
              }.toMap
              // Typed errors thrown from the synchronous OverrideBody callbacks are routed into the error channel.
              MIO(target.construct(None, Map.empty, overrides)).flatMap {
                case Right(expr)  => MIO.pure(expr.asInstanceOf[Expr[Any]])
                case Left(errors) =>
                  failCannotConstruct(typeClass, s"trait instance: ${errors.toVector.mkString(", ")}")
              }
            }
        }
      }
  }

  @scala.annotation.nowarn("msg=is never used|unused implicit parameter|unused explicit parameter")
  private def overrideBody(
      op: TraitOp,
      shape: MethodShape,
      sourceMethods: List[Method],
      instances: Map[(String, Int), Expr[Any]]
  ): OverrideBody = new OverrideBody {
    def apply(octx: OverrideContext): Expr_?? = {
      implicit val AnyType: Type[Any] = InvariantKTypes.Any
      def param(i: Int, sourceIndex: Int, p: Expr_??): Expr_?? = {
        val ps = shape.params(i)
        val x: Expr[Any] = { import p.Underlying as P; p.value.upcast[Any] }
        lazy val inst = instances((shape.key, i))
        def transformed(e: Expr[Any]): Expr_?? = e.as_??
        (op, ps.occ, ps.isVararg) match {
          case (_, Occ.Absent, _) => p
          // Direct F[A] parameters
          case (TraitOp.ContramapK(_, fk), Occ.Direct, false) => transformed(callApplyFk(fk, x))
          case (TraitOp.ContramapK(_, fk), Occ.Direct, true)  => transformed(callApplyFkAll(fk, x))
          case (TraitOp.ImapK(_, _, gk), Occ.Direct, false)   => transformed(callApplyFk(gk, x))
          case (TraitOp.ImapK(_, _, gk), Occ.Direct, true)    => transformed(callApplyFkAll(gk, x))
          case (_: TraitOp.ProductK, Occ.Direct, vararg)      =>
            transformed(
              if (sourceIndex == 0) { if (vararg) callFirstAll(x) else callFirst(x) }
              else { if (vararg) callSecondAll(x) else callSecond(x) }
            )
          // Nested T[F] parameters
          case (TraitOp.MapK(_, fk), _: Occ.Nested, vararg) =>
            transformed(if (vararg) callContramapKAll(inst, x, fk) else callContramapK(inst, x, fk))
          case (TraitOp.ContramapK(_, fk), _: Occ.Nested, vararg) =>
            transformed(if (vararg) callMapKAll(inst, x, fk) else callMapK(inst, x, fk))
          case (TraitOp.ImapK(_, fk, gk), _: Occ.Nested, vararg) =>
            transformed(if (vararg) callImapKAll(inst, x, gk, fk) else callImapK(inst, x, gk, fk))
          case (_: TraitOp.ProductK, _: Occ.Nested, vararg) =>
            val proj = if (sourceIndex == 0) firstKExpr else secondKExpr
            transformed(if (vararg) callMapKAll(inst, x, proj) else callMapK(inst, x, proj))
          case _ =>
            // Unreachable: rejected by paramTc before the instance is built.
            throw CatsTaglessDerivationError.UnsupportedMethod("", shape.name, "unsupported parameter")
        }
      }

      def sourceCall(sourceIndex: Int, src: Expr[Any]): Expr[Any] = {
        val sm = sourceMethods(sourceIndex)
        val args: List[Expr_??] = octx.parameters.zipWithIndex.map { case (p, i) => param(i, sourceIndex, p) }
        try
          sm.fold(
            onInstance = oi => src.as_??(oi.Instance.asInstanceOf[Type[Any]]),
            onTypes = at => at.typeParams.flatten.zip(octx.typeParameters).toMap, // forward the override's tparams
            onValues = av => {
              val names = av.totalParameters.flatten.toList.map(_._1)
              val byName = names.zip(args).toMap
              // Cast the arguments of this clause whose type does not conform to the parameter's (transformed,
              // erased ones; by-name or path-dependent ones Hearth's argument check rejects).
              val current = av.parameters.flatten.toList.map { case (n, p) =>
                // What Hearth's argument check expects (for a by-name parameter: its unsubstituted underlying type).
                val expected = p.byNameUnderlying.getOrElse(p.tpe)
                val arg = byName(n)
                n -> (
                  if (arg.Underlying <:< expected.Underlying) arg
                  else {
                    // A transformed vararg is a `Seq` of the element type.
                    val target: ?? =
                      if (p.isVararg) {
                        val elem = elementOf(expected, isVararg = true)
                        import elem.Underlying as El
                        Type.of[Seq[El]].as_??
                      } else expected
                    import target.Underlying as E
                    import arg.Underlying as A
                    Expr.quote(Expr.splice(arg.value.asInstanceOf[Expr[A]]).asInstanceOf[E]).as_??
                  }
                )
              }
              byName ++ current
            }
          ) match {
            case Right(exprE) => import exprE.Underlying; exprE.value.upcast[Any]
            case Left(e)      =>
              throw CatsTaglessDerivationError.MethodForwardingFailed(shape.name, s"fold returned Left: $e")
          }
        catch {
          case e: CatsTaglessDerivationError  => throw e
          case scala.util.control.NonFatal(e) =>
            throw CatsTaglessDerivationError.MethodForwardingFailed(
              shape.name,
              s"fold crashed: ${hearth.kindlings.derivation.compiletime.DerivationErrorRendering.messageOf(e)}"
            )
        }
      }

      val returnT = octx.returnType
      import returnT.Underlying as R
      lazy val inst = instances((shape.key, -1))
      def cast(e: Expr[Any]): Expr_?? = Expr.quote(Expr.splice(e).asInstanceOf[R]).as_??
      (op, shape.result) match {
        case (TraitOp.MapK(af, _), Occ.Absent)           => cast(sourceCall(0, af))
        case (TraitOp.MapK(af, fk), Occ.Direct)          => cast(callApplyFk(fk, sourceCall(0, af)))
        case (TraitOp.MapK(af, fk), _: Occ.Nested)       => cast(callMapK(inst, sourceCall(0, af), fk))
        case (TraitOp.ContramapK(af, _), Occ.Absent)     => cast(sourceCall(0, af))
        case (TraitOp.ContramapK(af, fk), _: Occ.Nested) => cast(callContramapK(inst, sourceCall(0, af), fk))
        case (TraitOp.ImapK(af, _, _), Occ.Absent)       => cast(sourceCall(0, af))
        case (TraitOp.ImapK(af, fk, _), Occ.Direct)      => cast(callApplyFk(fk, sourceCall(0, af)))
        case (TraitOp.ImapK(af, fk, gk), _: Occ.Nested)  => cast(callImapK(inst, sourceCall(0, af), fk, gk))
        case (TraitOp.ProductK(af, ag), Occ.Direct)      => cast(callTuple2K(sourceCall(0, af), sourceCall(1, ag)))
        case (TraitOp.ProductK(af, ag), _: Occ.Nested)   =>
          cast(callProductK(inst, sourceCall(0, af), sourceCall(1, ag)))
        case (TraitOp.ProductK(af, ag), Occ.Absent) =>
          // Like cats-tagless: combine both results with the result type's `Semigroup` when there is one, otherwise
          // take af's. Resolved once per instance (a `lazy val`) unless it mentions the method's type parameters.
          val callF: Expr[R] = Expr.quote(Expr.splice(sourceCall(0, af)).asInstanceOf[R])
          implicit val SemigroupR: Type[cats.kernel.Semigroup[R]] = Type.Ctor1.of[cats.kernel.Semigroup].apply[R]
          Expr.summonImplicit[cats.kernel.Semigroup[R]].toOption.map { semigroup =>
            if (octx.typeParameters.isEmpty) instanceScoped(semigroup) else semigroup
          } match {
            case Some(semigroup) =>
              val callG: Expr[R] = Expr.quote(Expr.splice(sourceCall(1, ag)).asInstanceOf[R])
              Expr.quote(Expr.splice(semigroup).combine(Expr.splice(callF), Expr.splice(callG))).as_??
            case None => callF.as_??
          }
        case (TraitOp.Instrument(af, _), Occ.Absent)           => cast(sourceCall(0, af))
        case (TraitOp.Instrument(af, algebraName), Occ.Direct) =>
          cast(callInstrumentation(sourceCall(0, af), algebraName, shape.name))
        case _ =>
          // Unreachable: rejected by resultTc before the instance is built.
          throw CatsTaglessDerivationError.UnsupportedMethod("", shape.name, "unsupported result")
      }
    }
  }

  // --- Generated calls (erased runtime helpers) ---

  private def callApplyFk(fk: Expr[Any], x: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.applyFk(Expr.splice(fk), Expr.splice(x))
  }
  private def callApplyFkAll(fk: Expr[Any], xs: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .applyFkAll(Expr.splice(fk), Expr.splice(xs).asInstanceOf[Seq[Any]])
  }
  private def callMapK(tc: Expr[Any], x: Expr[Any], fk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .mapK(Expr.splice(tc), Expr.splice(x), Expr.splice(fk))
  }
  private def callMapKAll(tc: Expr[Any], xs: Expr[Any], fk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .mapKAll(Expr.splice(tc), Expr.splice(xs).asInstanceOf[Seq[Any]], Expr.splice(fk))
  }
  private def callContramapK(tc: Expr[Any], x: Expr[Any], fk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .contramapK(Expr.splice(tc), Expr.splice(x), Expr.splice(fk))
  }
  private def callContramapKAll(tc: Expr[Any], xs: Expr[Any], fk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .contramapKAll(Expr.splice(tc), Expr.splice(xs).asInstanceOf[Seq[Any]], Expr.splice(fk))
  }
  private def callImapK(tc: Expr[Any], x: Expr[Any], fk: Expr[Any], gk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .imapK(Expr.splice(tc), Expr.splice(x), Expr.splice(fk), Expr.splice(gk))
  }
  private def callImapKAll(tc: Expr[Any], xs: Expr[Any], fk: Expr[Any], gk: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .imapKAll(Expr.splice(tc), Expr.splice(xs).asInstanceOf[Seq[Any]], Expr.splice(fk), Expr.splice(gk))
  }
  private def callProductK(tc: Expr[Any], x: Expr[Any], y: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .productK(Expr.splice(tc), Expr.splice(x), Expr.splice(y))
  }
  private def callTuple2K(x: Expr[Any], y: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.mkTuple2K(Expr.splice(x), Expr.splice(y))
  }
  private def callFirst(x: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.tuple2KFirst(Expr.splice(x))
  }
  private def callSecond(x: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.tuple2KSecond(Expr.splice(x))
  }
  private def callFirstAll(xs: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .tuple2KFirstAll(Expr.splice(xs).asInstanceOf[Seq[Any]])
  }
  private def callSecondAll(xs: Expr[Any]): Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
      .tuple2KSecondAll(Expr.splice(xs).asInstanceOf[Seq[Any]])
  }
  private def firstKExpr: Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.firstK
  }
  private def secondKExpr: Expr[Any] = Expr.quote {
    hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime.secondK
  }
  private def callInstrumentation(x: Expr[Any], algebraName: String, methodName: String): Expr[Any] = {
    val alg = Expr(algebraName)
    val method = Expr(methodName)
    Expr.quote {
      hearth.kindlings.catstaglessderivation.internal.runtime.CatsTaglessRuntime
        .mkInstrumentation(Expr.splice(x), Expr.splice(alg), Expr.splice(method))
    }
  }
}
