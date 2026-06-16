package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.Id
import hearth.fp.instances.*

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
  * a runtime helper with an element lambda built via [[hearth.typed.Exprs.LambdaBuilder]] (the sanctioned use). `.at` /
  * `.when` / Either / `modifyAll` / lens composition are later phases.
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

  private val EachWrapperNames = Set("EachOps", "<init>")

  /** Strip the marker implicit-class wrapper (`EachOps` constructor or its `new …` call), yielding the wrapped
    * container expression. If the instance is already the container (no wrapper node survived destructuring), it is
    * returned as-is.
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
      case Nil                           => transformLeaf(sExpr)
      case PathStep.Field(field) :: rest => buildFieldStep[S](sExpr, field, rest)(transformLeaf)
      case (each: PathStep.Each) :: rest => buildEachStep[S](sExpr, each, rest)(transformLeaf)
    }

  private def buildFieldStep[S: Type](sExpr: Expr[S], field: String, rest: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    CaseClass.parse[S] match {
      case ClassViewResult.Incompatible(reason) =>
        abort(s"`modify` can only descend into case classes, but [${Type.prettyPrint[S]}] is not one: $reason")
      case ClassViewResult.Compatible(caseClass) =>
        val fieldValues = caseClass.caseFieldValuesAt(sExpr)
        val focused = fieldValues.getOrElse(
          field,
          abort(s"`modify`: no accessible field `$field` on [${Type.prettyPrint[S]}]")
        )
        val rebuiltFocused = recurseInto(focused, rest, transformLeaf)
        reconstruct[S](caseClass, fieldValues.updated(field, rebuiltFocused))
    }

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

  /** Phantom unary constructor label for summoning a functor whose constructor was discovered at runtime. */
  private type AnyK[X] = Any

  private lazy val QuicklensFunctorCtor: Type.CtorK1[QuicklensFunctor] = Type.CtorK1.of[QuicklensFunctor]
  private lazy val MapFunctorCtor: Type.Ctor1[QuicklensMapFunctor.ForMap] = Type.Ctor1.of[QuicklensMapFunctor.ForMap]

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
