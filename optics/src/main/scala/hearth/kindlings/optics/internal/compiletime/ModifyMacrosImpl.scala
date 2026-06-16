package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.Id
import hearth.fp.data.NonEmptyVector
import hearth.fp.instances.*
import hearth.fp.syntax.*

/** Shared, macro-platform-agnostic implementation of the `modify` optics macro.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `CaseClass`, `DestructuredExpr`, ...).
  *
  * Phase 1 supports field access only (`_.a.b.c`). Phase 2 adds quicklens-style collection traversal: a path may
  * interleave `.each` / `.eachWhere(cond)` steps (over `Seq`/`List`/`Vector`/`Option`/`Set`/`Array` via
  * [[hearth.kindlings.optics.QuicklensFunctor]], and over map values via
  * [[hearth.kindlings.optics.QuicklensMapFunctor]]). The path lambda is parsed with
  * [[hearth.typed.Exprs.DestructuredExpr.parse]] into an ordered list of [[PathStep]]s; field steps emit a nested
  * copy-with-modification (Phase 1 machinery), and `.each` steps summon the relevant functor by type and emit a call to
  * a runtime helper with an element lambda built via [[hearth.typed.Exprs.LambdaBuilder]] (the sanctioned use).
  *
  * Phase 3 adds indexed access (`.at`/`.index`/`.atOrElse` over Seq/Map and Option), Either traversal
  * (`.eachLeft`/`.eachRight`), and the `.when[Subtype]` prism (a non-exhaustive 2-case
  * `value match { case s: Subtype => f(s); case other => other }` built via [[MatchCase.typeMatch]]). `modifyAll` and
  * lens composition are later phases.
  */
private[optics] trait ModifyMacrosImpl { this: MacroCommons =>

  /** A single step of a parsed `modify` path. */
  sealed private trait PathStep
  private object PathStep {

    /** A plain field access `_.field`. */
    final case class Field(name: String) extends PathStep

    /** A `.each` / `.eachWhere(cond)` over a container `container` (`F[elem]` or `M[k, elem]`).
      *
      * `predicate`, when present, is the `cond: elem => Boolean` of `.eachWhere`. `isMap` distinguishes the map-values
      * traversal (summon [[QuicklensMapFunctor]]) from the single-arg-functor traversal (summon [[QuicklensFunctor]]).
      */
    final case class Each(container: ??, elem: ??, predicate: Option[Expr_??], isMap: Boolean) extends PathStep

    /** What kind of indexed access this is. */
    sealed trait AtKind
    object AtKind {
      case object At extends AtKind // throws if absent
      case object Index extends AtKind // no-op if absent
      case object AtOrElse extends AtKind // inserts `default` if absent
    }

    /** What sort of container an [[At]] step traverses (selects the runtime helper + functor to summon). */
    sealed trait AtShape
    object AtShape {
      case object Seq extends AtShape // QuicklensIndexedFunctor, Int-keyed
      case object Map extends AtShape // QuicklensMapAtFunctor, key-keyed
      case object Single extends AtShape // QuicklensSingleAtFunctor, Option-like (no index)
    }

    /** A `.at(i)` / `.index(i)` / `.atOrElse(i, default)` over an indexed container, or the no-index `.at` / `.index` /
      * `.atOrElse(default)` over an `Option`-like container.
      *
      * `idx` is the index/key argument (absent for the `Single` shape). `default` is present only for `AtOrElse`.
      * `elem` is the focused element type derived from the container.
      */
    final case class At(
        container: ??,
        elem: ??,
        kind: AtKind,
        shape: AtShape,
        idx: Option[Expr_??],
        default: Option[Expr_??]
    ) extends PathStep

    /** A `.eachLeft` / `.eachRight` over an `Either[L, R]`. `isLeft` selects the branch; `branch` is the focused branch
      * type (`L` for `.eachLeft`, `R` for `.eachRight`).
      */
    final case class EachEither(container: ??, branch: ??, isLeft: Boolean) extends PathStep

    /** A `.when[Subtype]` prism: narrows the focus to `subtype`, leaving other subtypes unchanged. */
    final case class When(subtype: ??) extends PathStep
  }

  /** `obj.modify(_.a.b.c)` / `obj.modify(_.xs.each.field)` → `PathModify[S, A]`. Parses the path lambda into a list of
    * [[PathStep]]s, then emits `PathModify(obj, (s, mod) => <rebuilt s>)`.
    *
    * The focused leaf type `A` is the path lambda's result type, which the call site infers precisely: the `.each`
    * markers pin the element type via [[hearth.kindlings.optics.IsElementOf]] (invariantly, so no covariant widening),
    * so even leaf `.each`/`.eachWhere` paths produce a precise `A`. The macro additionally re-derives the element type
    * from the parsed container, so the codegen never depends on `A`'s inference being perfect.
    */
  def modify[S: Type, A: Type](obj: Expr[S], path: Expr[S => A]): Expr[PathModify[S, A]] = {
    val steps: List[PathStep] = parsePath[S, A](path)
    // Build the copy-with-modification function `(s, mod) => <rebuilt s>`, where the leaf transformation applied at the
    // focus is exactly `mod(<leaf>)`. The lambda parameters `s`/`mod` are quote-bound; their `Expr` handles are
    // recovered for the macro-side `buildModify` via `Expr.quote(s)` / `Expr.quote(mod)` (the standard Hearth cross-quote
    // idiom — see cats-derivation `FunctorMacrosImpl`).
    val doModify: Expr[(S, A => A) => S] =
      Expr.quote { (s: S, mod: A => A) =>
        Expr.splice {
          buildModify[S](Expr.quote(s), steps) { leaf =>
            applyLeaf[S, A](Expr.quote(mod), leaf)
          }
        }
      }

    Expr.quote(PathModify[S, A](Expr.splice(obj), Expr.splice(doModify)))
  }

  /** `obj.modifyAll(_.a, _.b.each, _.c)` → `PathModify[S, A]`. Parses each path into its own list of [[PathStep]]s,
    * then merges them into a single copy-with-modification function that applies `mod` to EVERY focused leaf.
    * Overlapping field prefixes are shared — `modifyAll(_.a.b, _.a.c)` copies `a` exactly once (quicklens semantics).
    * All paths must focus the same leaf type `A` (the call site pins this).
    */
  def modifyAll[S: Type, A: Type](obj: Expr[S], paths: List[Expr[S => A]]): Expr[PathModify[S, A]] = {
    if (paths.isEmpty) abort("`modifyAll` requires at least one path")
    val pathSteps: List[List[PathStep]] = paths.map(parsePath[S, A])
    val doModify: Expr[(S, A => A) => S] =
      Expr.quote { (s: S, mod: A => A) =>
        Expr.splice {
          buildModifyMulti[S](Expr.quote(s), pathSteps) { leaf =>
            applyLeaf[S, A](Expr.quote(mod), leaf)
          }
        }
      }
    Expr.quote(PathModify[S, A](Expr.splice(obj), Expr.splice(doModify)))
  }

  /** `modifyLens[T](_.a.b)` → `PathLazyModify[T, U]`: a reusable lens NOT bound to an object. Same machinery as
    * [[modify]], but the rebuilt value is a function of the lambda parameter `t` rather than a captured object.
    */
  def modifyLens[T: Type, Foc: Type](path: Expr[T => Foc]): Expr[PathLazyModify[T, Foc]] = {
    val steps: List[PathStep] = parsePath[T, Foc](path)
    val doModify: Expr[(T, Foc => Foc) => T] =
      Expr.quote { (t: T, mod: Foc => Foc) =>
        Expr.splice {
          buildModify[T](Expr.quote(t), steps) { leaf =>
            applyLeaf[T, Foc](Expr.quote(mod), leaf)
          }
        }
      }
    Expr.quote(PathLazyModify[T, Foc](Expr.splice(doModify)))
  }

  /** `modifyAllLens[T](_.a, _.b)` → `PathLazyModify[T, Foc]`: the multi-path reusable lens form. Same shared-prefix
    * merge as [[modifyAll]], but unbound from any object.
    */
  def modifyAllLens[T: Type, Foc: Type](paths: List[Expr[T => Foc]]): Expr[PathLazyModify[T, Foc]] = {
    if (paths.isEmpty) abort("`modifyAllLens` requires at least one path")
    val pathSteps: List[List[PathStep]] = paths.map(parsePath[T, Foc])
    val doModify: Expr[(T, Foc => Foc) => T] =
      Expr.quote { (t: T, mod: Foc => Foc) =>
        Expr.splice {
          buildModifyMulti[T](Expr.quote(t), pathSteps) { leaf =>
            applyLeaf[T, Foc](Expr.quote(mod), leaf)
          }
        }
      }
    Expr.quote(PathLazyModify[T, Foc](Expr.splice(doModify)))
  }

  /** Rebuild `sExpr: S` applying `transformLeaf` at the focus of EVERY path in `paths`, sharing common `Field` prefixes
    * so a field on the shared prefix is copied only once. Strategy:
    *
    *   - paths that bottom out here (empty) apply `transformLeaf` to the whole `S` (fold each on top);
    *   - `Field`-headed paths are grouped by field name; for each group we descend once, recurse on the group's tails,
    *     and rebuild the case class with all touched fields replaced together (one `.copy`);
    *   - non-field-headed paths (`.each`/`.at`/`.when`/...) cannot share a copy node, so each is applied independently
    *     via the single-path [[buildModify]] folded on top of the result.
    *
    * Folding the independent transforms is observably identical to quicklens: disjoint paths touch disjoint parts, so
    * order does not matter; overlapping field prefixes go through the shared `.copy`.
    */
  private def buildModifyMulti[S: Type](sExpr: Expr[S], paths: List[List[PathStep]])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    val leafHere = paths.filter(_.isEmpty)
    val fieldHeaded = paths.collect { case (f: PathStep.Field) :: rest => (f.name, rest) }
    val otherHeaded = paths.filter(p => p.nonEmpty && !p.head.isInstanceOf[PathStep.Field])

    // 1. Rebuild the shared `.copy` for all field-headed paths (grouped by field name), if any.
    val afterFields: Expr[S] =
      if (fieldHeaded.isEmpty) sExpr
      else
        CaseClass.parse[S] match {
          case ClassViewResult.Compatible(caseClass) =>
            val fieldValues = caseClass.caseFieldValuesAt(sExpr)
            val grouped: Map[String, List[List[PathStep]]] =
              fieldHeaded.groupBy(_._1).map { case (name, group) => name -> group.map(_._2) }
            val updated = grouped.foldLeft(fieldValues) { case (acc, (field, subPaths)) =>
              val focused = acc.getOrElse(
                field,
                abort(s"`modify`: no accessible field `$field` on [${Type.prettyPrint[S]}]")
              )
              val rebuiltFocused = recurseIntoMulti(focused, subPaths, transformLeaf)
              acc.updated(field, rebuiltFocused)
            }
            reconstruct[S](caseClass, updated)
          // Sealed hierarchy / enum: match over the subtypes and apply the field-headed paths within each case.
          case ClassViewResult.Incompatible(ccReason) =>
            Enum.parse[S] match {
              case ClassViewResult.Compatible(enm) =>
                enm
                  .matchOn[Id, S](sExpr) { childExpr =>
                    import childExpr.Underlying as Child
                    val childPaths: List[List[PathStep]] =
                      fieldHeaded.map { case (name, tail) => (PathStep.Field(name): PathStep) :: tail }
                    val rebuilt: Expr[Child] =
                      buildModifyMulti[Child](childExpr.value, childPaths)(leaf =>
                        transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Child]]
                      )
                    whenUpcast[Child, S](rebuilt)
                  }
                  .getOrElse(abort(s"`modify`: [${Type.prettyPrint[S]}] is a sealed hierarchy with no cases"))
              case ClassViewResult.Incompatible(_) =>
                abort(
                  s"`modify` can only descend a field into a case class or a sealed hierarchy/enum, but " +
                    s"[${Type.prettyPrint[S]}] is neither: $ccReason"
                )
            }
        }

    // 2. Apply each non-field-headed path independently, folded on top.
    val afterOthers: Expr[S] = otherHeaded.foldLeft(afterFields) { (acc, path) =>
      buildModify[S](acc, path)(transformLeaf)
    }

    // 3. Apply each leaf-here transform on top.
    leafHere.foldLeft(afterOthers)((acc, _) => transformLeaf(acc))
  }

  /** Multi-path counterpart of [[recurseInto]]: descend into a focused field carrying its existential type, applying
    * all `subPaths` (which share this field) via [[buildModifyMulti]], without leaking the path-dependent `Underlying`
    * into an `Expr.quote`.
    */
  private def recurseIntoMulti[S: Type](
      focused: Expr_??,
      subPaths: List[List[PathStep]],
      transformLeaf: Expr[S] => Expr[S]
  ): Expr_?? = {
    import focused.Underlying as F
    buildModifyMulti[F](focused.value, subPaths)(leaf =>
      transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[F]]
    ).as_??
  }

  /** Parse the path lambda `_.a.each.b` into an ordered list of [[PathStep]]s (root → leaf).
    *
    * Walks the [[hearth.typed.Exprs.DestructuredExpr.MethodCall]] chain: a call with only an instance argument is a
    * field access; a call to `each`/`eachWhere` (the marker extensions) is a collection traversal whose receiver is the
    * container expression (its element type is derived from the container — see [[eachStep]]). Bottoms out at the
    * lambda parameter reference.
    */
  private def parsePath[S: Type, A: Type](path: Expr[S => A]): List[PathStep] = {
    implicit val funType: Type[S => A] = Type.Ctor2.of[Function1].apply[S, A]
    val parsed = DestructuredExpr.parse[S => A](path)
    parsed match {
      case lam: DestructuredExpr.Lambda =>
        lam.params match {
          case List(param) => walk(lam.body, param)
          case params      =>
            abort(s"`modify` expects a single-parameter path lambda, got ${params.size} parameters")
        }
      case other =>
        abort(s"`modify` expects a path lambda like `_.a.b.c`, but got: ${other.plainPrint}")
    }
  }

  private def walk(
      expr: DestructuredExpr,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = expr match {
    case ref: DestructuredExpr.Lambda.ParamRef if ref.param eq rootParam =>
      Nil
    // A `.each` / `.eachWhere(cond)` step: a call named `each`/`eachWhere` whose receiver is the container (recovered by
    // `receiverOf`, which strips the Scala 2 `EachOps` wrapper). The predicate (for `.eachWhere`) is recovered by
    // `predicateArg` (the `Function1`-typed value argument, skipping any synthesized `IsElementOf` evidence).
    case mc: DestructuredExpr.MethodCall if mc.method.name == "each" || mc.method.name == "eachWhere" =>
      val instance = receiverOf(mc).getOrElse(
        abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
      )
      val predicate = if (mc.method.name == "eachWhere") predicateArg(mc).map(predExprOf) else None
      eachStep(instance, predicate, rootParam)
    case mc: DestructuredExpr.MethodCall
        if mc.method.name == "at" || mc.method.name == "index" || mc.method.name == "atOrElse" =>
      atStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall if mc.method.name == "eachLeft" || mc.method.name == "eachRight" =>
      eitherStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall if mc.method.name == "when" =>
      whenStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall =>
      mc.applied match {
        case List(ai: DestructuredExpr.MethodCall.AppliedInstance) =>
          walk(ai.value, rootParam) :+ PathStep.Field(mc.method.name)
        case _ =>
          abort(
            s"`modify` expects a field-access path like `_.a.b.c` (optionally with `.each`), but got an unsupported " +
              s"step: ${mc.plainPrint}"
          )
      }
    case other =>
      abort(
        s"`modify` expects a field-access path like `_.a.b.c` (optionally with `.each`), but got: ${other.plainPrint}"
      )
  }

  /** Build the [[PathStep.Each]] for a recovered container `instance`, deriving the element type from the container's
    * constructor (`F[A]`'s sole argument, or `M[K, V]`'s value argument) rather than from the `.each` marker's result
    * type — the container always carries the precise element type.
    */
  private def eachStep(
      instance: DestructuredExpr,
      predicate: Option[Expr_??],
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val container = instance.tpe
    val isMap = isMapContainer(container)
    val elem = elementTypeOf(container, isMap)
    walk(instance, rootParam) :+ PathStep.Each(container, elem, predicate, isMap)
  }

  /** Parse a `.at(i)` / `.index(i)` / `.atOrElse(i, default)` (Seq/Map) or `.at` / `.index` / `.atOrElse(default)`
    * (Option) step. The container is recovered like `.each`; the shape (Seq/Map/Option) is decided from the container
    * type. Index/default arguments are recovered from the call's value arguments, skipping the synthesized evidence.
    */
  private def atStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
    )
    val container = instance.tpe
    val shape = atShapeOf(container)
    val elem = atElementTypeOf(container, shape)
    val kind = mc.method.name match {
      case "at"       => PathStep.AtKind.At
      case "index"    => PathStep.AtKind.Index
      case "atOrElse" => PathStep.AtKind.AtOrElse
      case other      => abort(s"`modify`: unexpected indexed step `.$other`")
    }
    // Recover the non-evidence value arguments (the synthesized `IsIndexedElementOf`/`IsSingleElementOf` evidence is a
    // leading `using` value arg on Scala 3; on Scala 2 the evidence is on the wrapper class, so it never appears here).
    val realArgs = nonEvidenceArgs(mc)
    val (idx, default) = shape match {
      case PathStep.AtShape.Single =>
        // No index. `atOrElse` carries `(default)`; `at`/`index` carry nothing.
        val default = if (kind == PathStep.AtKind.AtOrElse) realArgs.headOption.map(exprOf) else None
        (None, default)
      case _ =>
        // `at(i)`/`index(i)` carry `(i)`; `atOrElse(i, default)` carries `(i, default)`.
        val idx = realArgs.headOption.map(exprOf)
        val default = if (kind == PathStep.AtKind.AtOrElse) realArgs.lift(1).map(exprOf) else None
        (idx, default)
    }
    walk(instance, rootParam) :+ PathStep.At(container, elem, kind, shape, idx, default)
  }

  /** Parse a `.eachLeft` / `.eachRight` step over an `Either[L, R]`. */
  private def eitherStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
    )
    val container = instance.tpe
    val isLeft = mc.method.name == "eachLeft"
    val branch = eitherBranchTypeOf(container, isLeft)
    walk(instance, rootParam) :+ PathStep.EachEither(container, branch, isLeft)
  }

  /** Parse a `.when[Subtype]` prism step. The narrowed subtype is recovered from the call's type arguments. */
  private def whenStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the receiver of `.when` in ${mc.plainPrint}")
    )
    // On Scala 3 the extension `when[T <: C]` desugars to two type-argument clauses: the extension's `[C]` (e.g.
    // `[Animal]`) followed by the method's own `[T]` (e.g. `[Dog]`). The narrowed subtype is the LAST type-argument
    // clause (on Scala 2 there is a single clause, so `last` is also correct).
    val subtype = mc.applied
      .collect { case at: DestructuredExpr.MethodCall.AppliedTypes if at.typeArgs.nonEmpty => at.typeArgs.last }
      .lastOption
      .getOrElse(abort(s"`modify`: `.when` requires an explicit subtype argument, e.g. `.when[Sub]`"))
    walk(instance, rootParam) :+ PathStep.When(subtype)
  }

  /** All value arguments of a call that are NOT the synthesized marker evidence (`IsElementOf`/`IsIndexedElementOf`/
    * `IsSingleElementOf`/`IsEither`), in order. On Scala 3 the `using` evidence appears as a leading value argument; on
    * Scala 2 it lives on the wrapper class and never reaches here.
    */
  private def nonEvidenceArgs(mc: DestructuredExpr.MethodCall): List[DestructuredExpr] = {
    val allValueArgs = mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }.flatten
    allValueArgs.filterNot(isEvidenceTyped)
  }

  private lazy val PathStepEvidenceType: Type[PathStepEvidence] = Type.of[PathStepEvidence]

  private def isEvidenceTyped(d: DestructuredExpr): Boolean = {
    import d.tpe.Underlying as T
    implicit val ev: Type[PathStepEvidence] = PathStepEvidenceType
    Type.isSubtypeOf[T, PathStepEvidence]
  }

  private def exprOf(d: DestructuredExpr): Expr_?? = d.toUntypedExpr.as_??

  /** Decide the [[PathStep.AtShape]] of a container targeted by `.at`/`.index`/`.atOrElse`: a binary `M[K, V]` is a
    * `Map`; a unary `F[A]` that is an `Option`-like (matched by [[QuicklensSingleAtFunctor]]'s shape) is `Single`;
    * otherwise a `Seq`-like is `Seq`. We distinguish `Single` from `Seq` by checking the constructor: `Option` summons
    * a `QuicklensSingleAtFunctor`, everything else a `QuicklensIndexedFunctor`. We probe summonability of the single-at
    * functor first.
    */
  private def atShapeOf(container: ??): PathStep.AtShape = {
    import container.Underlying as C
    if (isMapContainer(container)) PathStep.AtShape.Map
    else
      Type.decompose1[C] match {
        case Some((fCtor, _)) =>
          // Prefer the single-at (Option) shape when a `QuicklensSingleAtFunctor[F]` is summonable; otherwise treat it
          // as an indexed `Seq`-like.
          if (canSummonSingleAt(fCtor)) PathStep.AtShape.Single else PathStep.AtShape.Seq
        case None =>
          abort(s"`modify`: `.at`/`.index`/`.atOrElse` expected a container `F[A]`, but [${Type
              .prettyPrint[C]}] is not applied")
      }
  }

  private def canSummonSingleAt(fCtor: Type.Ctor1[AnyK1]): Boolean = {
    implicit val singleType: Type[QuicklensSingleAtFunctor[AnyK]] =
      SingleAtFunctorCtor.apply(using fCtor).asInstanceOf[Type[QuicklensSingleAtFunctor[AnyK]]]
    Expr.summonImplicit[QuicklensSingleAtFunctor[AnyK]].toOption.isDefined
  }

  /** The focused element type of an `.at`/`.index`/`.atOrElse` over a container of the given [[PathStep.AtShape]]. */
  private def atElementTypeOf(container: ??, shape: PathStep.AtShape): ?? = shape match {
    case PathStep.AtShape.Map => elementTypeOf(container, isMap = true)
    case _                    => elementTypeOf(container, isMap = false)
  }

  /** The focused branch type of `.eachLeft` (`L`) / `.eachRight` (`R`) over an `Either[L, R]` container. */
  private def eitherBranchTypeOf(container: ??, isLeft: Boolean): ?? = {
    import container.Underlying as C
    Type
      .decompose2[C]
      .map { case (_, (left, right)) => if (isLeft) left else right }
      .getOrElse(
        abort(s"`modify`: `.eachLeft`/`.eachRight` expected an `Either[L, R]`, but [${Type.prettyPrint[C]}] is not one")
      )
  }

  /** Recover the container expression an `.each`/`.eachWhere` call is applied to. The `.each` marker is an extension
    * over `EachOps[C, A]` (Scala 2 implicit class) / an `extension (c: C)(using IsElementOf.Aux[C, A])` (Scala 3): on
    * Scala 2 the `each` call's instance is the `EachOps` wrapper whose value argument is the container; on Scala 3 the
    * extension method's instance IS the container directly. We dig through the instance/value-arg chain to the
    * container.
    */
  private def receiverOf(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] = {
    // The instance of the `each` call is the implicit-class wrapper around the container; its first value/instance
    // argument is the container expression.
    val instanceArg: Option[DestructuredExpr] = mc.applied.collectFirst {
      case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value
    }
    instanceArg.flatMap(unwrapEachWrapper)
  }

  private val EachWrapperNames = Set("EachOps", "AtOps", "SingleAtOps", "EitherOps", "WhenOps", "<init>")

  /** Strip the marker implicit-class wrapper (`EachOps`/`AtOps`/... constructor or its `new …` call), yielding the
    * wrapped container expression. If the instance is already the container (no wrapper node survived destructuring,
    * e.g. on Scala 3 where the markers are `extension` methods), it is returned as-is.
    */
  private def unwrapEachWrapper(expr: DestructuredExpr): Option[DestructuredExpr] = expr match {
    case mc: DestructuredExpr.MethodCall =>
      val name = mc.method.name
      if (mc.method.isConstructor || EachWrapperNames.contains(name)) {
        // The container is the wrapper's value argument (or, defensively, its instance argument).
        mc.applied
          .collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues if av.args.nonEmpty => av.args.head }
          .orElse(mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value })
      } else Some(expr)
    case _ => Some(expr)
  }

  /** The `.eachWhere` predicate among the call's value arguments: the (single) `Function1`-typed argument. This skips
    * the synthesized `IsElementOf` evidence, which on Scala 3 appears as a leading `using` value argument of the
    * `eachWhere` extension method (on Scala 2 the evidence is on the wrapper class, so only the predicate is present).
    */
  private def predicateArg(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] = {
    val allValueArgs = mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }.flatten
    allValueArgs.find(isFunction1Typed).orElse(allValueArgs.headOption)
  }

  private def isFunction1Typed(d: DestructuredExpr): Boolean = {
    import d.tpe.Underlying as T
    Type.decompose2[T].exists { case (ctor, _) => ctor.sameTypeConstructorAs(Function1Ctor.asUntyped) }
  }

  private lazy val Function1Ctor: Type.Ctor2[Function1] = Type.Ctor2.of[Function1]

  private def predExprOf(d: DestructuredExpr): Expr_?? = d.toUntypedExpr.as_??

  /** Whether `container` is a binary type constructor `M[K, V]` (a map-like) rather than a unary `F[A]`. */
  private def isMapContainer(container: ??): Boolean = {
    import container.Underlying as C
    Type.decompose2[C].isDefined && Type.decompose1[C].isEmpty
  }

  /** The element type traversed by `.each`: the sole type argument of a unary `F[A]`, or the *value* (second) argument
    * of a map-like `M[K, V]`.
    */
  private def elementTypeOf(container: ??, isMap: Boolean): ?? = {
    import container.Underlying as C
    if (isMap)
      Type
        .decompose2[C]
        .map { case (_, (_, value)) => value }
        .getOrElse(abort(s"`modify`: `.each` over a map expected `M[K, V]`, but [${Type.prettyPrint[C]}] is not one"))
    else
      Type
        .decompose1[C]
        .map { case (_, elem) => elem }
        .getOrElse(abort(s"`modify`: `.each` expected a container `F[A]`, but [${Type.prettyPrint[C]}] is not applied"))
  }

  /** Apply the user modification `mod: A => A` to the focused leaf. At the leaf the focused field's type IS the path's
    * leaf type `A`, so the incoming `leaf: Expr[S]` (whose `S` is structurally that field type) is reinterpreted as
    * `Expr[A]` and the result back to `Expr[S]`. The casts are macro-side `Expr` reinterpretations only — the generated
    * tree is a plain `mod(leaf)` call.
    */
  private def applyLeaf[S: Type, A: Type](mod: Expr[A => A], leaf: Expr[S]): Expr[S] =
    Expr.quote(Expr.splice(mod).apply(Expr.splice(leaf.asInstanceOf[Expr[A]]))).asInstanceOf[Expr[S]]

  /** Rebuild a value `sExpr: S` with the focus reached by `steps` transformed by `transformLeaf`. When `steps` is
    * empty, `sExpr` itself is the focus, so `transformLeaf` is applied directly. A [[PathStep.Field]] head names a case
    * field to descend into (read all fields, recurse into the focused one, reconstruct). A [[PathStep.Each]] head maps
    * the remaining path over every element of the container via the summoned functor.
    */
  private def buildModify[S: Type](sExpr: Expr[S], steps: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    steps match {
      case Nil                              => transformLeaf(sExpr)
      case PathStep.Field(field) :: rest    => buildFieldStep[S](sExpr, field, rest)(transformLeaf)
      case (each: PathStep.Each) :: rest    => buildEachStep[S](sExpr, each, rest)(transformLeaf)
      case (at: PathStep.At) :: rest        => buildAtStep[S](sExpr, at, rest)(transformLeaf)
      case (e: PathStep.EachEither) :: rest => buildEitherStep[S](sExpr, e, rest)(transformLeaf)
      case (w: PathStep.When) :: rest       => buildWhenStep[S](sExpr, w, rest)(transformLeaf)
    }

  private def buildFieldStep[S: Type](sExpr: Expr[S], field: String, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    CaseClass.parse[S] match {
      case ClassViewResult.Compatible(caseClass) =>
        val fieldValues = caseClass.caseFieldValuesAt(sExpr)
        val focused = fieldValues.getOrElse(
          field,
          abort(s"`modify`: no accessible field `$field` on [${Type.prettyPrint[S]}]")
        )
        val rebuiltFocused = recurseInto(focused, rest, transformLeaf)
        reconstruct[S](caseClass, fieldValues.updated(field, rebuiltFocused))
      // Not a case class — if it is a sealed hierarchy / `enum`, descend the field into EVERY subtype (each must have
      // the field) by generating an exhaustive match. This is quicklens-style common-field modification, so a field
      // shared by all cases can be modified directly without writing `.when[Sub]` per case.
      case ClassViewResult.Incompatible(ccReason) =>
        Enum.parse[S] match {
          case ClassViewResult.Compatible(enm) => buildFieldStepEnum[S](enm, sExpr, field, rest)(transformLeaf)
          case ClassViewResult.Incompatible(_) =>
            abort(
              s"`modify` can only descend a field into a case class or a sealed hierarchy/enum (whose every case has " +
                s"that field), but [${Type.prettyPrint[S]}] is neither: $ccReason"
            )
        }
    }

  /** Descend field `field` (and `rest`) into every subtype of a sealed hierarchy / `enum`, generating an exhaustive
    * `s match { case c: Child => <field path on c> }`. Each subtype is recursed through [[buildFieldStep]] (so a
    * subtype must itself be a case class with `field`, or a nested sealed hierarchy), and its rebuilt value is widened
    * back to `S` in-tree (sound: `Child <: S`).
    */
  private def buildFieldStepEnum[S: Type](enm: Enum[S], sExpr: Expr[S], field: String, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    enm
      .matchOn[Id, S](sExpr) { childExpr =>
        import childExpr.Underlying as Child
        val rebuilt: Expr[Child] =
          buildFieldStep[Child](childExpr.value, field, rest)(leaf =>
            transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Child]]
          )
        whenUpcast[Child, S](rebuilt)
      }
      .getOrElse(
        abort(s"`modify`: [${Type.prettyPrint[S]}] is a sealed hierarchy with no cases to descend `$field` into")
      )

  /** Emit the `.each` step over `sExpr: S` (where `S` IS the container type `F[A]`/`M[K,A]`). Summon the relevant
    * functor, build the element lambda `a => <rest of path on a>` via `LambdaBuilder`, and call the runtime helper.
    */
  private def buildEachStep[S: Type](sExpr: Expr[S], each: PathStep.Each, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    import each.elem.Underlying as Elem
    // The element lambda transforms one element of type `Elem` by applying the remaining path steps. The leaf transform
    // is the same `transformLeaf` re-typed: `transformLeaf` is over `S`, but at the leaf the element type and `S`/`A`
    // coincide structurally (it is only ever invoked when `rest` bottoms out at the focus), so the casts are sound.
    // The element lambda is built directly as `Any => Any` (rather than `Elem => Elem` cast macro-side) so its tree
    // genuinely has type `Any => Any` and conforms to the erased runtime-helper parameter. The `Any` input is cast
    // in-tree to `Elem`, the remaining path is applied, and the `Elem` result is cast back to `Any` in-tree.
    implicit val anyType: Type[Any] = Type.of[Any]
    val elementLambda: Expr[Any => Any] =
      LambdaBuilder
        .of1[Any]("each$elem")
        .buildWith { (anyA: Expr[Any]) =>
          val a = castInTree[Elem](anyA)
          val rebuilt: Expr[Elem] =
            buildModify[Elem](a, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Elem]])
          // Widen `Expr[Elem]` to `Expr[Any]` macro-side (the lambda body position expects `Any`); the underlying tree
          // is still the `Elem`-typed `rebuilt`, which conforms since `Elem <: Any`.
          rebuilt.asInstanceOf[Expr[Any]]
        }

    val functor = summonFunctor[S](each, each.isMap)
    // The user predicate is `Elem => Boolean`; wrap it as a genuine `Any => Boolean` tree (`(x: Any) =>
    // pred(x.asInstanceOf[Elem])`) so it conforms to the erased runtime-helper parameter (a macro-side cast would leave
    // the tree typed `Elem => Boolean`, which does not conform to `Any => Boolean`).
    implicit val boolType: Type[Boolean] = Type.of[Boolean]
    val pred: Option[Expr[Any => Boolean]] = each.predicate.map { p =>
      val predElem = p.value.asInstanceOf[Expr[Elem => Boolean]]
      LambdaBuilder
        .of1[Any]("each$pred")
        .buildWith { (anyA: Expr[Any]) =>
          val a = castInTree[Elem](anyA)
          Expr.quote(Expr.splice(predElem).apply(Expr.splice(a)))
        }
    }

    (each.isMap, pred) match {
      case (false, None)       => eachFunctor[S](functor, sExpr, elementLambda)
      case (false, Some(cond)) => eachFunctorWhere[S](functor, sExpr, cond, elementLambda)
      case (true, None)        => eachMap[S](functor, sExpr, elementLambda)
      case (true, Some(cond))  => eachMapWhere[S](functor, sExpr, cond, elementLambda)
    }
  }

  /** Build the per-element `Any => Any` transformation lambda for a non-field step over `S` whose focused element type
    * is `Elem`: cast the erased `Any` input to `Elem` in-tree, apply the remaining path, widen the `Elem` result to
    * `Any`. Shared by `.each`/`.at`/`.eachLeft`/`.eachRight` (each delegates to a runtime helper expecting an erased
    * `Any => Any`). The leaf transform re-types `transformLeaf` (over `S`) at the structurally-coincident `Elem`.
    */
  private def elementLambdaFor[S: Type, Elem: Type](label: String, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[Any => Any] = {
    implicit val anyType: Type[Any] = Type.of[Any]
    LambdaBuilder
      .of1[Any](label)
      .buildWith { (anyA: Expr[Any]) =>
        val a = castInTree[Elem](anyA)
        val rebuilt: Expr[Elem] =
          buildModify[Elem](a, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Elem]])
        rebuilt.asInstanceOf[Expr[Any]]
      }
  }

  /** Emit a `.at(i)` / `.index(i)` / `.atOrElse(i, default)` (Seq/Map) or `.at` / `.index` / `.atOrElse(default)`
    * (Option) step over `sExpr: S` (where `S` IS the container). Summon the relevant indexed functor, build the element
    * lambda for `rest`, and call the matching runtime helper, threading the index/default through as erased `Any`s.
    */
  private def buildAtStep[S: Type](sExpr: Expr[S], at: PathStep.At, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    import at.elem.Underlying as Elem
    val f = elementLambdaFor[S, Elem]("at$elem", rest)(transformLeaf)
    val functor = summonAtFunctor[S](at)
    def idx: Expr[Any] =
      at.idx.map(eraseToAny).getOrElse(abort(s"`modify`: `.${at.kind}` is missing its index argument"))
    def default: Expr[Any] =
      at.default.map(eraseToAny).getOrElse(abort(s"`modify`: `.atOrElse` is missing its default argument"))
    import PathStep.{AtKind, AtShape}
    (at.shape, at.kind) match {
      case (AtShape.Seq, AtKind.At)          => atIndexedRT[S](functor, sExpr, idx, f)
      case (AtShape.Seq, AtKind.Index)       => indexIndexedRT[S](functor, sExpr, idx, f)
      case (AtShape.Seq, AtKind.AtOrElse)    => atOrElseIndexedRT[S](functor, sExpr, idx, default, f)
      case (AtShape.Map, AtKind.At)          => atMapRT[S](functor, sExpr, idx, f)
      case (AtShape.Map, AtKind.Index)       => indexMapRT[S](functor, sExpr, idx, f)
      case (AtShape.Map, AtKind.AtOrElse)    => atOrElseMapRT[S](functor, sExpr, idx, default, f)
      case (AtShape.Single, AtKind.At)       => atSingleRT[S](functor, sExpr, f)
      case (AtShape.Single, AtKind.Index)    => indexSingleRT[S](functor, sExpr, f)
      case (AtShape.Single, AtKind.AtOrElse) => atOrElseSingleRT[S](functor, sExpr, default, f)
    }
  }

  /** Emit a `.eachLeft` / `.eachRight` step over `sExpr: S` (an `Either[L, R]`). Summon the branch functor, build the
    * branch element lambda for `rest`, and call the runtime helper.
    */
  private def buildEitherStep[S: Type](sExpr: Expr[S], step: PathStep.EachEither, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    import step.branch.Underlying as Branch
    val f = elementLambdaFor[S, Branch](if (step.isLeft) "eachLeft$elem" else "eachRight$elem", rest)(transformLeaf)
    val functor = summonEitherFunctor[S](step)
    if (step.isLeft) eachLeftRT[S](functor, sExpr, f) else eachRightRT[S](functor, sExpr, f)
  }

  /** Emit a `.when[Subtype]` prism step over `sExpr: S`: a non-exhaustive 2-case match
    * `s match { case t: Subtype => <rest on t>; case other => other }`. Built with `MatchCase.typeMatch[Subtype]`
    * (recurse on the narrowed value) and `MatchCase.typeMatch[S]` (the parent type — a catch-all returning the value
    * unchanged). On Scala 3 `matchOn` annotates the scrutinee `@unchecked`, so the non-exhaustive match compiles
    * cleanly on both platforms.
    */
  private def buildWhenStep[S: Type](sExpr: Expr[S], step: PathStep.When, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    import step.subtype.Underlying as Sub
    if (!Type.isSubtypeOf[Sub, S])
      abort(s"`modify`: `.when[${Type.prettyPrint[Sub]}]` requires a subtype of [${Type.prettyPrint[S]}]")
    val matchedCase = MatchCase.typeMatch[Sub]().map { (sub: Expr[Sub]) =>
      // Recurse into the narrowed subtype; the leaf transform re-types `transformLeaf` (over `S`) at the
      // structurally-coincident `Sub`, then the rebuilt `Sub` is widened back to `S` (sound: `Sub <: S`).
      val rebuilt: Expr[Sub] =
        buildModify[Sub](sub, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Sub]])
      whenUpcast[Sub, S](rebuilt)
    }
    val fallthrough = MatchCase.typeMatch[S]().map((other: Expr[S]) => other)
    MatchCase.matchOn[S, S](sExpr)(NonEmptyVector(matchedCase, fallthrough))
  }

  /** Widen `Expr[Sub]` to `Expr[S]` via an in-tree `.asInstanceOf[S]` — sound because `Sub <: S` (checked in
    * `buildWhenStep`), and kept in-tree so the generated match arms agree on the result type `S`. (`Sub <: S` is not
    * visible to the macro at the static type level since both are abstract, so a plain ascription does not type-check;
    * the cast is a runtime no-op given the verified subtype relationship.)
    */
  private def whenUpcast[Sub: Type, S: Type](e: Expr[Sub]): Expr[S] =
    Expr.quote(Expr.splice(e).asInstanceOf[S])

  /** Erase a value-argument expression to `Expr[Any]` in-tree (so it conforms to the erased runtime-helper `Any`
    * parameter). The underlying tree keeps its precise type, which conforms since everything `<: Any`.
    */
  private def eraseToAny(e: Expr_??): Expr[Any] = e.value.asInstanceOf[Expr[Any]]

  /** A real, in-tree `.asInstanceOf[T]` cast (a genuine cast node typed `T`), as opposed to a macro-side `Expr`
    * reinterpretation which would leave the tree's underlying type unchanged.
    */
  private def castInTree[T: Type](e: Expr[Any]): Expr[T] =
    Expr.quote(Expr.splice(e).asInstanceOf[T])

  // The runtime helpers return `Any` (the discovered container constructor is erased), so the result is cast back to
  // `S` *inside* the quote — a real `.asInstanceOf[S]` tree node, so the emitted expression is genuinely typed `S` (a
  // macro-side `Expr` reinterpretation would leave the tree typed `Any`, widening the inferred `A` to `Any`).
  private def eachFunctor[S: Type](functor: Expr[Any], sExpr: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .eachFunctor(
          Expr.splice(functor),
          Expr.splice(sExpr.asInstanceOf[Expr[Any]]),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def eachFunctorWhere[S: Type](
      functor: Expr[Any],
      sExpr: Expr[S],
      cond: Expr[Any => Boolean],
      f: Expr[Any => Any]
  ): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .eachFunctorWhere(
          Expr.splice(functor),
          Expr.splice(sExpr.asInstanceOf[Expr[Any]]),
          Expr.splice(cond),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def eachMap[S: Type](functor: Expr[Any], sExpr: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .eachMap(
          Expr.splice(functor),
          Expr.splice(sExpr.asInstanceOf[Expr[Any]]),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def eachMapWhere[S: Type](
      functor: Expr[Any],
      sExpr: Expr[S],
      cond: Expr[Any => Boolean],
      f: Expr[Any => Any]
  ): Expr[S] =
    Expr
      .quote {
        QuicklensRuntime
          .eachMapWhere(
            Expr.splice(functor),
            Expr.splice(sExpr.asInstanceOf[Expr[Any]]),
            Expr.splice(cond),
            Expr.splice(f)
          )
          .asInstanceOf[S]
      }
      .asInstanceOf[Expr[S]]

  // --- Runtime-helper calls for `.at`/`.index`/`.atOrElse` and Either. Each splices the erased functor, the container
  // (widened to `Any` in-tree), the index/default (already erased `Any`), and the element lambda, then casts the `Any`
  // result back to `S` in-tree (a real `.asInstanceOf[S]` node, so the emitted tree is genuinely typed `S`).

  private def atIndexedRT[S: Type](functor: Expr[Any], s: Expr[S], idx: Expr[Any], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atIndexed(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(idx), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def indexIndexedRT[S: Type](functor: Expr[Any], s: Expr[S], idx: Expr[Any], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .indexIndexed(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(idx), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def atOrElseIndexedRT[S: Type](
      functor: Expr[Any],
      s: Expr[S],
      idx: Expr[Any],
      default: Expr[Any],
      f: Expr[Any => Any]
  ): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atOrElseIndexed(
          Expr.splice(functor),
          Expr.splice(s.asInstanceOf[Expr[Any]]),
          Expr.splice(idx),
          Expr.splice(default),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def atMapRT[S: Type](functor: Expr[Any], s: Expr[S], key: Expr[Any], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atMap(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(key), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def indexMapRT[S: Type](functor: Expr[Any], s: Expr[S], key: Expr[Any], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .indexMap(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(key), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def atOrElseMapRT[S: Type](
      functor: Expr[Any],
      s: Expr[S],
      key: Expr[Any],
      default: Expr[Any],
      f: Expr[Any => Any]
  ): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atOrElseMap(
          Expr.splice(functor),
          Expr.splice(s.asInstanceOf[Expr[Any]]),
          Expr.splice(key),
          Expr.splice(default),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def atSingleRT[S: Type](functor: Expr[Any], s: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atSingle(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def indexSingleRT[S: Type](functor: Expr[Any], s: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .indexSingle(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def atOrElseSingleRT[S: Type](
      functor: Expr[Any],
      s: Expr[S],
      default: Expr[Any],
      f: Expr[Any => Any]
  ): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .atOrElseSingle(
          Expr.splice(functor),
          Expr.splice(s.asInstanceOf[Expr[Any]]),
          Expr.splice(default),
          Expr.splice(f)
        )
        .asInstanceOf[S]
    }

  private def eachLeftRT[S: Type](functor: Expr[Any], s: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .eachLeft(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(f))
        .asInstanceOf[S]
    }

  private def eachRightRT[S: Type](functor: Expr[Any], s: Expr[S], f: Expr[Any => Any]): Expr[S] =
    Expr.quote {
      QuicklensRuntime
        .eachRight(Expr.splice(functor), Expr.splice(s.asInstanceOf[Expr[Any]]), Expr.splice(f))
        .asInstanceOf[S]
    }

  /** Phantom unary constructor label for summoning a functor whose constructor was discovered at runtime. */
  private type AnyK[X] = Any

  private lazy val QuicklensFunctorCtor: Type.CtorK1[QuicklensFunctor] = Type.CtorK1.of[QuicklensFunctor]
  private lazy val MapFunctorCtor: Type.Ctor1[QuicklensMapFunctor.ForMap] = Type.Ctor1.of[QuicklensMapFunctor.ForMap]
  private lazy val IndexedFunctorCtor: Type.CtorK1[IntKeyedIndexedFunctor] =
    Type.CtorK1.of[IntKeyedIndexedFunctor]
  private lazy val MapAtFunctorCtor: Type.Ctor1[QuicklensMapAtFunctor.ForMap] =
    Type.Ctor1.of[QuicklensMapAtFunctor.ForMap]
  private lazy val SingleAtFunctorCtor: Type.CtorK1[QuicklensSingleAtFunctor] =
    Type.CtorK1.of[QuicklensSingleAtFunctor]
  private lazy val EitherLeftFunctorCtor: Type.Ctor1[QuicklensEitherFunctor.ForLeft] =
    Type.Ctor1.of[QuicklensEitherFunctor.ForLeft]
  private lazy val EitherRightFunctorCtor: Type.Ctor1[QuicklensEitherFunctor.ForRight] =
    Type.Ctor1.of[QuicklensEitherFunctor.ForRight]

  /** A `* -> *` projection of [[QuicklensIndexedFunctor]] with the index pinned to `Int` (the `Seq`-like shape). The
    * macro discovers the container constructor `F` and summons `QuicklensIndexedFunctor[F, Int]` by building
    * `Type[IntKeyedIndexedFunctor[F]]` via `Type.CtorK1#apply`.
    */
  private type IntKeyedIndexedFunctor[F[_]] = QuicklensIndexedFunctor[F, Int]

  /** Summon a `QuicklensFunctor[F]` (or, for `isMap`, a `QuicklensMapFunctor[Map, K]`) for the container type,
    * returning it erased as `Expr[Any]`. The container's constructor is discovered via `Type.decompose1`/`decompose2`,
    * the functor type is built with `Type.CtorK1#apply` (`F[_]`) / `Type.Ctor1#apply` (the unary `ForMap[K]`
    * projection), and the instance is summoned by type — mirroring cats-derivation's `summonConsKForFieldType`.
    */
  private def summonFunctor[S: Type](each: PathStep.Each, isMap: Boolean): Expr[Any] = {
    import each.container.Underlying as C
    if (isMap) {
      Type.decompose2[C] match {
        case Some((_, (keyTpe, _))) =>
          import keyTpe.Underlying as K
          implicit val mapFunctorType: Type[QuicklensMapFunctor.ForMap[K]] = MapFunctorCtor.apply[K]
          Expr.summonImplicit[QuicklensMapFunctor.ForMap[K]].toOption match {
            case Some(f) => f.asInstanceOf[Expr[Any]]
            case None    =>
              abort(s"`modify`: no `QuicklensMapFunctor` for [${Type.prettyPrint[C]}] is in scope for `.each`")
          }
        case None =>
          abort(s"`modify`: `.each` over a map expected a `M[K, V]` container, but [${Type.prettyPrint[C]}] is not one")
      }
    } else {
      Type.decompose1[C] match {
        case Some((fCtor, _)) =>
          implicit val functorType: Type[QuicklensFunctor[AnyK]] =
            QuicklensFunctorCtor.apply(using fCtor).asInstanceOf[Type[QuicklensFunctor[AnyK]]]
          Expr.summonImplicit[QuicklensFunctor[AnyK]].toOption match {
            case Some(f) => f.asInstanceOf[Expr[Any]]
            case None    =>
              abort(s"`modify`: no `QuicklensFunctor` for [${Type.prettyPrint[C]}] is in scope for `.each`")
          }
        case None =>
          abort(s"`modify`: `.each` expected a container `F[A]`, but [${Type.prettyPrint[C]}] is not an applied type")
      }
    }
  }

  /** Summon the indexed functor for a `.at`/`.index`/`.atOrElse` step, erased to `Expr[Any]`: a
    * `QuicklensIndexedFunctor[F, Int]` for the `Seq` shape, a `QuicklensMapAtFunctor[Map, K]` for the `Map` shape, or a
    * `QuicklensSingleAtFunctor[F]` for the `Single` (Option) shape — mirroring `summonFunctor`.
    */
  private def summonAtFunctor[S: Type](at: PathStep.At): Expr[Any] = {
    import at.container.Underlying as C
    at.shape match {
      case PathStep.AtShape.Map =>
        Type.decompose2[C] match {
          case Some((_, (keyTpe, _))) =>
            import keyTpe.Underlying as K
            implicit val mapAtType: Type[QuicklensMapAtFunctor.ForMap[K]] = MapAtFunctorCtor.apply[K]
            Expr.summonImplicit[QuicklensMapAtFunctor.ForMap[K]].toOption match {
              case Some(f) => f.asInstanceOf[Expr[Any]]
              case None    =>
                abort(s"`modify`: no `QuicklensMapAtFunctor` for [${Type.prettyPrint[C]}] is in scope")
            }
          case None =>
            abort(s"`modify`: `.at` over a map expected `M[K, V]`, but [${Type.prettyPrint[C]}] is not one")
        }
      case PathStep.AtShape.Single =>
        Type.decompose1[C] match {
          case Some((fCtor, _)) =>
            implicit val singleType: Type[QuicklensSingleAtFunctor[AnyK]] =
              SingleAtFunctorCtor.apply(using fCtor).asInstanceOf[Type[QuicklensSingleAtFunctor[AnyK]]]
            Expr.summonImplicit[QuicklensSingleAtFunctor[AnyK]].toOption match {
              case Some(f) => f.asInstanceOf[Expr[Any]]
              case None    => abort(s"`modify`: no `QuicklensSingleAtFunctor` for [${Type.prettyPrint[C]}] is in scope")
            }
          case None =>
            abort(s"`modify`: `.at` expected a container `F[A]`, but [${Type.prettyPrint[C]}] is not applied")
        }
      case PathStep.AtShape.Seq =>
        Type.decompose1[C] match {
          case Some((fCtor, _)) =>
            implicit val indexedType: Type[IntKeyedIndexedFunctor[AnyK]] =
              IndexedFunctorCtor.apply(using fCtor).asInstanceOf[Type[IntKeyedIndexedFunctor[AnyK]]]
            Expr.summonImplicit[IntKeyedIndexedFunctor[AnyK]].toOption match {
              case Some(f) => f.asInstanceOf[Expr[Any]]
              case None    => abort(s"`modify`: no `QuicklensIndexedFunctor` for [${Type.prettyPrint[C]}] is in scope")
            }
          case None =>
            abort(s"`modify`: `.at` expected a container `F[A]`, but [${Type.prettyPrint[C]}] is not applied")
        }
    }
  }

  /** Summon the `QuicklensEitherFunctor[Either, L, R]` for a `.eachLeft`/`.eachRight` step, erased to `Expr[Any]`. For
    * `.eachLeft` we summon by the left type via the `ForLeft[L]` projection; for `.eachRight` by the right type via
    * `ForRight[R]`. The runtime instance is the same `eitherFunctor` regardless, so pinning the other branch to `Any`
    * is sound (it is never inspected at the type level past summoning).
    */
  private def summonEitherFunctor[S: Type](step: PathStep.EachEither): Expr[Any] = {
    import step.container.Underlying as C
    Type.decompose2[C] match {
      case Some((_, (leftTpe, rightTpe))) =>
        if (step.isLeft) {
          import leftTpe.Underlying as L
          implicit val leftType: Type[QuicklensEitherFunctor.ForLeft[L]] = EitherLeftFunctorCtor.apply[L]
          Expr.summonImplicit[QuicklensEitherFunctor.ForLeft[L]].toOption match {
            case Some(f) => f.asInstanceOf[Expr[Any]]
            case None    => abort(s"`modify`: no `QuicklensEitherFunctor` for [${Type.prettyPrint[C]}] is in scope")
          }
        } else {
          import rightTpe.Underlying as R
          implicit val rightType: Type[QuicklensEitherFunctor.ForRight[R]] = EitherRightFunctorCtor.apply[R]
          Expr.summonImplicit[QuicklensEitherFunctor.ForRight[R]].toOption match {
            case Some(f) => f.asInstanceOf[Expr[Any]]
            case None    => abort(s"`modify`: no `QuicklensEitherFunctor` for [${Type.prettyPrint[C]}] is in scope")
          }
        }
      case None =>
        abort(s"`modify`: `.eachLeft`/`.eachRight` expected an `Either[L, R]`, but [${Type.prettyPrint[C]}] is not one")
    }
  }

  /** Recurse into a focused field expression carrying its existential type, so the path-dependent `Underlying` never
    * leaks into an `Expr.quote` (the di/mock Scala 2 pitfall). The `transformLeaf` is over `S`, but at the leaf the
    * field type and `S`/`A` coincide structurally — `transformLeaf` is only ever invoked when the remaining steps
    * bottom out at the focus and the field type IS the focused `A`, so the cast is sound.
    */
  private def recurseInto[S: Type](
      focused: Expr_??,
      rest: List[PathStep],
      transformLeaf: Expr[S] => Expr[S]
  ): Expr_?? = {
    import focused.Underlying as F
    buildModify[F](focused.value, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[F]]).as_??
  }

  /** Reconstruct a case-class value from its (possibly modified) field expressions, by name. */
  private def reconstruct[S: Type](caseClass: CaseClass[S], fieldValues: Map[String, Expr_??]): Expr[S] =
    caseClass.construct[Id] { parameter =>
      fieldValues.getOrElse(
        parameter.name,
        abort(s"`modify`: missing value for field `${parameter.name}` of [${Type.prettyPrint[S]}]")
      )
    } match {
      case Some(expr) => expr
      case None       =>
        abort(s"`modify`: the primary constructor of [${Type.prettyPrint[S]}] is not accessible at the call site")
    }

  private def abort(msg: String): Nothing = Environment.reportErrorAndAbort(msg)
}
