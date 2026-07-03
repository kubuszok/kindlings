package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommons
import hearth.kindlings.macros.compiletime.{NodeKind, Storage, WiringGraph}
import cats.effect.kernel.Resource

/** Shared, macro-platform-agnostic implementation of the `wireResource` macro.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `Method`, `LambdaBuilder`, ...).
  *
  * This is an F-agnostic re-imagining of macwire's `autocats` (`com.softwaremill.macwire.autocats.autowire`), which
  * hardcodes `Resource[cats.effect.IO, _]`. Here `F[_]` is an abstract type constructor: the same derivation produces
  * `Resource[F, T]` for any `F` (e.g. `cats.effect.IO`, `cats.effect.SyncIO`, ...).
  *
  * ==Constraint decision (no typeclass constraints)==
  *
  * In cats-effect 3.6.3 the three primitives we splice are all constraint-free:
  *   - `Resource.pure[F, A](a: A): Resource[F, A]`
  *   - `Resource.eval[F, A](fa: F[A]): Resource[F, A]`
  *   - `Resource#flatMap[B](f: A => Resource[F, B]): Resource[F, B]`
  *
  * None of them require `Applicative[F]`/`Sync[F]`/`MonadCancel[F]`, so `wireResource` needs neither a summoned
  * implicit nor an `(implicit F: ...)` parameter. (`Resource.onFinalize`, `Resource.make`, etc. DO need constraints,
  * but we never emit those — acquisition/release is provided pre-built by the caller as a `Resource[F, X]` dependency.)
  *
  * ==Scope (V2 — macwire autocats parity)==
  *
  * Implemented:
  *   - plain instances (spliced directly), `Resource[F, X]` and `F[X]` effect dependencies (folded as `flatMap`s);
  *   - root/intermediate construction via the primary constructor, a single companion `apply`, OR a companion
  *     `def resource[F[_]: <constraints>]: Resource[F, T]` factory (Kindlings extension over macwire — we apply OUR `F`
  *     and let the compiler resolve the context-bound implicits; see `companionResourceCall`);
  *   - parameter resolution to the single provider whose `resultType <:< paramType` (subtype-aware), with
  *     macwire-parity errors (missing dependency path / ambiguous);
  *   - factory-method (`FunctionN`) dependencies: a passed lambda / eta-expanded method whose params are themselves
  *     resolved from the graph, the result wrapped per its return shape (`Resource`/effect/plain);
  *   - recursive intermediate construction (`wireRec`-like): a needed type with no direct provider that is itself
  *     constructible is built from the graph, and SHARED — every distinct type is instantiated exactly once;
  *   - a stable, dependency-respecting topological order over inter-provider dependencies (input order preserved for
  *     independent providers), so factories/intermediates that feed each other are acquired in a valid order;
  *   - the unused-provider error (`Not used providers for the following types [...]`).
  *
  * ==Deliberate divergence from macwire (`constructInputProvider`)==
  *
  * macwire's autocats NEVER reuses a directly-passed root instance: it always rebuilds the root via a creator, so
  * `autowire[B](new B(new A("s")))` FAILS on the missing `String` (macwire flags this itself with
  * `// TODO we should add a warning in this case.`). di-cats deliberately diverges: a provided instance whose type
  * matches the root is reused as the root (wrapped in `Resource.pure`). This is the least-surprising behavior and is
  * consistent with how every other parameter is resolved to a provided value by type. See the "deliberate divergence
  * from macwire" test group in `ResourceWiringSpec`.
  *
  * ==Deferred (vs. macwire)==
  *
  * macwire's `taggingParameters` (softwaremill `@@`-tagged dependency disambiguation) is not yet ported — it needs a
  * tagging-type provider. Left cleanly unimplemented (no failing test).
  */
private[dicats] trait ResourceWiringMacrosImpl
    extends hearth.kindlings.macros.compiletime.GenerationLogging
    with hearth.kindlings.macros.compiletime.WiringGraphLogging { this: MacroCommons =>

  protected val generationLoggingNamespace: String = "diCats"
  protected val wiringLoggingNamespace: String = "diCats"
  protected def generationMarkerImported: Boolean = {
    implicit val tpe: Type[DICats.LogGeneration] = Type.of[DICats.LogGeneration]
    Expr.summonImplicit[DICats.LogGeneration].isDefined
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Provider model (port of macwire's CatsProviders.scala)
  // ---------------------------------------------------------------------------------------------------------------

  /** A classified input dependency. `resultType` is the type this provider ultimately yields into the graph:
    *   - [[Instance]]: the dependency's own type (spliced directly into a construction — NO flatMap).
    *   - [[ResourceP]]/[[EffectP]]: the `X` of `Resource[F, X]` / `F[X]` (bound by a flatMap lambda var).
    *   - [[FactoryP]]: the underlying `X` of the factory's return type (`Resource[F, X]` / `F[X]` / plain `X`).
    */
  sealed private trait Provider {
    def resultType: ??
    def describe: String = {
      val rt = resultType
      import rt.Underlying as R
      Type.prettyPrint[R]
    }
  }

  /** A plain value `X`. Spliced directly wherever its type is needed; never flatMapped. */
  final private case class Instance(value: Expr_??) extends Provider {
    val resultType: ?? = {
      import value.Underlying as X
      Type[X].as_??
    }
  }

  /** A `Resource[F, X]` dependency: contributes `value.flatMap((x: X) => acc)` to the fold. */
  final private case class ResourceP(value: Expr_??, x: ??) extends Provider {
    def resultType: ?? = x
  }

  /** An `F[X]` effect dependency: contributes `Resource.eval[F, X](value).flatMap((x: X) => acc)` to the fold. */
  final private case class EffectP(value: Expr_??, x: ??) extends Provider {
    def resultType: ?? = x
  }

  /** A `FunctionN` factory dependency. `applyM` is the function value's `apply` (its params are resolved from the graph
    * at emit time); `returnKind` classifies how its result is wrapped into the chain; `x` is the underlying result type
    * `X` (the function's return type, or the `X` of its `Resource[F, X]`/`F[X]` return).
    */
  final private case class FactoryP(
      value: Expr_??,
      applyM: Method.OnInstance,
      returnKind: ReturnKind,
      x: ??
  ) extends Provider {
    def resultType: ?? = x
  }

  /** How a factory's (or intermediate construction's) result feeds the chain. */
  sealed private trait ReturnKind
  private object ReturnKind {
    case object PlainK extends ReturnKind
    case object ResourceK extends ReturnKind
    case object EffectK extends ReturnKind
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Entry point
  // ---------------------------------------------------------------------------------------------------------------

  /** Build a `Resource[F, T]` that constructs a `T` from the supplied `dependencies`. */
  def wireResource[F[_], T](
      dependencies: List[Expr_??]
  )(implicit T: Type[T], FCtor: Type.Ctor1[F]): Expr[Resource[F, T]] = {
    implicit val anyType: Type[Any] = Type.of[Any]
    implicit val fAnyType: Type[F[Any]] = FCtor.apply[Any]

    val providers = dependencies.map(classify[F])

    buildResource[F, T](providers) match {
      case Right(expr)  => expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Classification
  // ---------------------------------------------------------------------------------------------------------------

  /** Classify one dependency by its static type. Order matters: factory (`FunctionN`) first (a function's *value* is
    * never a wireable instance), then `Resource[F, X]`, then effect `F[X]`, else a plain [[Instance]].
    */
  private def classify[F[_]](
      dep: Expr_??
  )(implicit fAnyType: Type[F[Any]], anyType: Type[Any]): Provider = {
    import dep.Underlying as D
    if (isFunction[D]) factoryProvider[F](dep)
    else if (isResource[D]) ResourceP(dep, resourceUnderlying[D])
    else if (isEffect[F, D]) EffectP(dep, effectUnderlying[F, D])
    else Instance(dep)
  }

  /** A dependency whose static type is a `scala.FunctionN` is a factory method (a lambda or eta-expanded method). */
  private def isFunction[D: Type]: Boolean = Type.fqcn[D].startsWith("scala.Function")

  /** Decompose a factory function value into its (instance-resolved) `apply`, parameter types, and return shape. */
  private def factoryProvider[F[_]](
      dep: Expr_??
  )(implicit fAnyType: Type[F[Any]], anyType: Type[Any]): Provider = {
    import dep.Underlying as D
    // A function value may expose several `apply`s (specialized/bridge variants); the genuine SAM has the most
    // parameters with the most specific (non-erased) types, so prefer the highest-arity one.
    val applyOpt = new Class[D]()
      .method("apply")
      .collect {
        case m: Method.OnInstance if m.isAvailable(AtCallSite) => m
      }
      .sortBy(m => -m.totalParameters.flatten.size)
      .headOption
    applyOpt match {
      case None =>
        Environment.reportErrorAndAbort(s"The factory ${Type.prettyPrint[D]} has no accessible apply method.")
      case Some(applyM) =>
        val ret = applyM.knownReturning.getOrElse(
          Environment.reportErrorAndAbort(s"Cannot determine the return type of factory ${Type.prettyPrint[D]}.")
        )
        val (kind, x) = classifyReturn[F](ret)
        FactoryP(dep, applyM, kind, x)
    }
  }

  /** Classify a (factory or construction) result type into `(ReturnKind, underlyingX)`. */
  private def classifyReturn[F[_]](
      ret: ??
  )(implicit fAnyType: Type[F[Any]], anyType: Type[Any]): (ReturnKind, ??) = {
    import ret.Underlying as R
    if (isResource[R]) (ReturnKind.ResourceK, resourceUnderlying[R])
    else if (isEffect[F, R]) (ReturnKind.EffectK, effectUnderlying[F, R])
    else (ReturnKind.PlainK, ret)
  }

  private def isResource[D: Type]: Boolean =
    Type.fqcn[D] == "cats.effect.kernel.Resource" && Type.decompose2[D].isDefined

  private def resourceUnderlying[D: Type]: ?? =
    Type.decompose2[D] match {
      case Some((_, (_, x))) => x
      case None              => Type[D].as_??
    }

  private def isEffect[F[_], D: Type](implicit fAnyType: Type[F[Any]], anyType: Type[Any]): Boolean =
    Type.decompose1[D].isDefined && Type.hasSameTypeConstructor[D, F[Any]]

  private def effectUnderlying[F[_], D: Type]: ?? =
    Type.decompose1[D] match {
      case Some((_, x)) => x
      case None         => Type[D].as_??
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Resolution — build a dependency plan (compile-time, no tree building)
  // ---------------------------------------------------------------------------------------------------------------

  /** A node to be emitted into the Resource chain. Each non-instance node binds a fresh flatMap variable of type [[x]];
    * its monadic value (a `Resource[F, X]`) is built by [[monadic]] from the already-bound variables.
    */
  final private case class Node(
      key: String,
      x: ??,
      // Given the bound variables resolved so far (keyed by type fqcn), produce the monadic `Resource[F, X]` value.
      monadic: (Map[String, Expr_??]) => Expr_??
  )

  /** Mutable resolution state threaded through the DFS. */
  final private class Resolver(providers: List[Provider]) {
    private val indexed: List[(Provider, Int)] = providers.zipWithIndex
    // Memo of already-planned types (fqcn -> node key). Instances are recorded as direct bindings.
    val nodes: scala.collection.mutable.ListBuffer[Node] = scala.collection.mutable.ListBuffer.empty
    val instanceBindings: scala.collection.mutable.LinkedHashMap[String, Expr_??] =
      scala.collection.mutable.LinkedHashMap.empty
    val planned: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
    // Keys currently under construction (the DFS stack), used to detect dependency cycles before they recurse forever.
    val inProgress: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
    val used: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set.empty
    // Nodes collected for the ZIO-Magic-style wiring graph (keyed by type FQCN), populated during resolution.
    val graphNodes: scala.collection.mutable.LinkedHashMap[String, WiringGraph.RawNode] =
      scala.collection.mutable.LinkedHashMap.empty

    /** Providers whose `resultType` conforms to `t` (subtype-aware, excluding bottom types), with their input index. */
    def matchingProviders(t: ??): List[(Provider, Int)] = {
      import t.Underlying as P
      indexed.filter { case (provider, _) =>
        val rt = provider.resultType
        import rt.Underlying as R
        Type[R] <:< Type[P] && !(Type[R] =:= Type.of[Nothing]) && !(Type[R] =:= Type.of[Null])
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Wiring-graph collection (ZIO-Magic style) — di-cats has no val/lazy/def storage (every node is shared via the
  // monadic Resource chain), so nodes are always emitted with Storage.Inline (no storage annotation in the graph).
  // ---------------------------------------------------------------------------------------------------------------

  private def keyOf(t: ??): String = { import t.Underlying as T; Type.fqcn[T] }
  private def displayOf(t: ??): String = {
    import t.Underlying as T
    Type.prettyPrint[T].replaceAll("\\[[0-9;]*m", "")
  }

  /** Record a graph node once (first write wins). */
  private def recordGraphNode(resolver: Resolver, key: String, t: ??, kind: NodeKind, depKeys: List[String]): Unit =
    if (!resolver.graphNodes.contains(key)) {
      val _ = resolver.graphNodes += (key -> WiringGraph.RawNode(key, displayOf(t), kind, Storage.Inline, depKeys))
    }

  /** The graph key a dependency of type `t` resolves to — a single matching provider's result type, or (when nothing
    * matches but `t` is wireable) `t` itself (a constructed intermediate). Mirrors [[resolveTypeViaResolver]]'s
    * dispatch, read-only (does not consume `used`), so graph edges point at the same node keys the plan uses.
    */
  private def resolvedKeyOf(resolver: Resolver, t: ??): Option[String] =
    resolver.matchingProviders(t) match {
      case (provider, _) :: Nil => Some(keyOf(provider.resultType))
      case Nil                  => if (isWireable(t)) Some(keyOf(t)) else None
      case _                    => None
    }

  /** The graph keys of a method's/factory's parameters (implicit params are recorded as `Summoned` leaf nodes). */
  private def depKeysOf(resolver: Resolver, params: List[(String, Parameter)]): List[String] =
    params.flatMap { case (_, parameter) =>
      if (parameter.isImplicit) {
        val k = keyOf(parameter.tpe)
        recordGraphNode(resolver, k, parameter.tpe, NodeKind.Summoned, Nil)
        Some(k)
      } else resolvedKeyOf(resolver, parameter.tpe)
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Build
  // ---------------------------------------------------------------------------------------------------------------

  private def buildResource[F[_], T](providers: List[Provider])(implicit
      T: Type[T],
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Either[String, Expr[Resource[F, T]]] = {
    val resolver = new Resolver(providers)

    // DFS-resolve the root construction; collect missing-dependency paths macwire-style.
    val missing = scala.collection.mutable.LinkedHashSet.empty[String]

    // The root: prefer a provider/factory matching T directly, else construct T from the graph.
    val rootBuilderOpt: Option[Map[String, Expr_??] => Expr_??] =
      resolver.matchingProviders(Type[T].as_??) match {
        case (provider, idx) :: Nil =>
          resolver.used += idx
          // A provider matching the root directly: its (already-bound) value IS the root.
          Some(emitProviderOrFactory[F](resolver, provider, idx, List("root"), missing))
        case Nil =>
          constructValue[F](
            resolver,
            Type[T].as_??,
            Nil,
            missing,
            (tt, pp) => resolveTypeViaResolver[F](resolver, tt, pp, missing)
          )
        case multiple =>
          Environment.reportErrorAndAbort(
            s"Ambiguous instances of types [${multiple.map { case (p, _) => p.describe }.mkString(", ")}] for [${Type.prettyPrint[T]}]"
          )
      }

    rootBuilderOpt match {
      case None =>
        // Missing dependencies — emit macwire-style aggregate error.
        Left(missingError[T](missing.toList))
      case _ if missing.nonEmpty =>
        Left(missingError[T](missing.toList))
      case Some(rootBuilder) =>
        // Unused-provider check (macwire's verifyOrder).
        val unused = providers.zipWithIndex.collect { case (p, i) if !resolver.used.contains(i) => p.describe }
        if (unused.nonEmpty) Left(s"Not used providers for the following types [${unused.mkString(", ")}]")
        else {
          val expr = emitChain[F, T](resolver.nodes.toList, resolver.instanceBindings.toMap, rootBuilder)
          val rootKey = resolvedKeyOf(resolver, Type[T].as_??).getOrElse(keyOf(Type[T].as_??))
          emitWiringGraphIfEnabled("DICats.wireResource", rootKey, resolver.graphNodes, wiringLogModeFromSettings)
          if (shouldWeLogGeneration) {
            val trace = new Trace
            trace.step(s"wireResource[${Type.prettyPrint[T]}] from ${providers.size} provided dependency(ies)")
            trace.step("wiring graph:")
            trace.step(wiringTreeFor(rootKey, resolver.graphNodes))
            emitGenerationLog("DICats.wireResource", trace, expr.prettyPrint)
          }
          Right(expr)
        }
    }
  }

  /** macwire's `isWireable`: refuse to recursively construct standard-library types (`java.*`/`scala.*`). */
  private def isWireable(t: ??): Boolean = {
    import t.Underlying as T
    val fqcn = Type.fqcn[T]
    !fqcn.startsWith("java.") && !fqcn.startsWith("scala.")
  }

  /** Record a missing dependency macwire-style: `Missing dependency of type [X]. Path crumb -> crumb`. The path is the
    * breadcrumb of `[constructor/method Owner].param` crumbs leading to the unmet type (omitted when empty).
    */
  private def recordMissing(
      missing: scala.collection.mutable.LinkedHashSet[String],
      t: ??,
      path: List[String]
  ): Unit = {
    import t.Underlying as P
    val pathStr = if (path.isEmpty) "" else s" Path ${path.mkString(" -> ")}"
    val _ = missing += s"Missing dependency of type [${Type.prettyPrint[P]}].$pathStr"
  }

  private def missingError[T: Type](lines: List[String]): String =
    lines.distinct.mkString(s"Failed to create an instance of [${Type.prettyPrint[T]}].\n", "\n", "")

  // ---------------------------------------------------------------------------------------------------------------
  // Emit individual providers into the plan
  // ---------------------------------------------------------------------------------------------------------------

  /** Emit a non-factory provider as a (memoized) plan node, returning a function producing its value from bound vars.
    */
  private def emitProvider[F[_]](
      resolver: Resolver,
      provider: Provider,
      idx: Int,
      path: List[String]
  )(implicit FCtor: Type.Ctor1[F], fAnyType: Type[F[Any]], anyType: Type[Any]): Map[String, Expr_??] => Expr_?? = {
    val _ = (idx, path)
    val x = provider.resultType
    import x.Underlying as X
    val key = Type.fqcn[X]
    provider match {
      case Instance(value) =>
        // Instances are spliced directly (no binding needed); record once for stability.
        if (!resolver.instanceBindings.contains(key)) resolver.instanceBindings += (key -> value)
        recordGraphNode(resolver, key, x, NodeKind.Provided("instance"), Nil)
        (_: Map[String, Expr_??]) => resolver.instanceBindings.getOrElse(key, value)

      case ResourceP(value, _) =>
        ensureNode(resolver, key, x)(_ => value)
        recordGraphNode(resolver, key, x, NodeKind.Provided("resource"), Nil)
        (bound: Map[String, Expr_??]) => bound(key)

      case EffectP(value, _) =>
        ensureNode(resolver, key, x)(_ => evalToResource[F, X](value))
        recordGraphNode(resolver, key, x, NodeKind.Provided("effect"), Nil)
        (bound: Map[String, Expr_??]) => bound(key)

      case _: FactoryP =>
        // Handled by emitFactory; reached only when already planned.
        (bound: Map[String, Expr_??]) => bound(key)
    }
  }

  /** Register a monadic node under `key` (idempotent), with a builder producing its `Resource[F, X]` value. */
  private def ensureNode(resolver: Resolver, key: String, x: ??)(monadic: Map[String, Expr_??] => Expr_??): Unit =
    if (!resolver.planned.contains(key)) {
      resolver.planned += key
      resolver.nodes += Node(key, x, monadic)
    }

  /** Wrap an effect `F[X]` as `Resource.eval[F, X](fx)`. */
  private def evalToResource[F[_], X: Type](fx: Expr_??)(implicit FCtor: Type.Ctor1[F]): Expr_?? = {
    implicit val resourceFX: Type[Resource[F, X]] = Type.of[Resource[F, X]]
    val effect = fx.value.asInstanceOf[Expr[F[X]]]
    Expr.quote(Resource.eval(Expr.splice(effect))).as_??
  }

  /** Wrap a plain value `X` as `Resource.pure[F, X](x)`. */
  private def pureToResource[F[_], X: Type](x: Expr[X])(implicit FCtor: Type.Ctor1[F]): Expr_?? = {
    implicit val resourceFX: Type[Resource[F, X]] = Type.of[Resource[F, X]]
    Expr.quote(Resource.pure[F, X](Expr.splice(x))).as_??
  }

  // (factory + intermediate emission defined below)

  // ---------------------------------------------------------------------------------------------------------------
  // Factories and recursive intermediate construction
  // ---------------------------------------------------------------------------------------------------------------

  // forward declarations resolved via the closures captured in buildResource
  private def buildIntermediate[F[_]](
      resolver: Resolver,
      t: ??,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Option[Map[String, Expr_??] => Expr_??] =
    constructValue[F](resolver, t, path, missing, (tt, pp) => resolveTypeViaResolver[F](resolver, tt, pp, missing))

  /** Re-entrant resolveType for nested constructions (mirrors the closure in buildResource). */
  private def resolveTypeViaResolver[F[_]](
      resolver: Resolver,
      t: ??,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Option[Map[String, Expr_??] => Expr_??] = {
    import t.Underlying as P
    resolver.matchingProviders(t) match {
      case (provider, idx) :: Nil =>
        resolver.used += idx
        Some(emitProviderOrFactory[F](resolver, provider, idx, path, missing))
      case Nil =>
        if (isWireable(t)) buildIntermediate[F](resolver, t, path, missing)
        else { recordMissing(missing, t, path); None }
      case multiple =>
        Environment.reportErrorAndAbort(
          s"Ambiguous instances of types [${multiple.map { case (p, _) => p.describe }.mkString(", ")}] for [${Type.prettyPrint[P]}]"
        )
    }
  }

  /** Emit a provider, dispatching factories to [[emitFactory]] (which resolves the factory's own params). */
  private def emitProviderOrFactory[F[_]](
      resolver: Resolver,
      provider: Provider,
      idx: Int,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String]
  )(implicit FCtor: Type.Ctor1[F], fAnyType: Type[F[Any]], anyType: Type[Any]): Map[String, Expr_??] => Expr_?? =
    provider match {
      case f: FactoryP => emitFactory[F](resolver, f, path, missing)
      case other       => emitProvider[F](resolver, other, idx, path)
    }

  /** Emit a factory provider: resolve each of its parameters from the graph, then apply it; wrap its result per its
    * return kind into a memoized monadic node.
    */
  private def emitFactory[F[_]](
      resolver: Resolver,
      f: FactoryP,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String]
  )(implicit FCtor: Type.Ctor1[F], fAnyType: Type[F[Any]], anyType: Type[Any]): Map[String, Expr_??] => Expr_?? = {
    val x = f.x
    import x.Underlying as X
    val key = Type.fqcn[X]
    val methodLabel = s"[method ${f.applyM.name}]"

    val factoryParams = f.applyM.totalParameters.flatten.toList
    // Resolve the factory's params (records their nodes/missing paths now).
    val paramResolvers: List[(String, Map[String, Expr_??] => Expr_??)] =
      factoryParams.map { case (pname, parameter) =>
        if (parameter.isImplicit) pname -> summonResolver(parameter)
        else
          pname -> resolveTypeViaResolver[F](resolver, parameter.tpe, path :+ s"$methodLabel.$pname", missing)
            .getOrElse((_: Map[String, Expr_??]) => nullPlaceholder(parameter.tpe))
      }
    recordGraphNode(resolver, key, x, NodeKind.Provided("factory"), depKeysOf(resolver, factoryParams))

    ensureNode(resolver, key, x) { bound =>
      val args: Map[String, Expr_??] = paramResolvers.map { case (n, r) => n -> r(bound) }.toMap
      val applied = f.applyM.apply(f.value.value.asInstanceOf[Expr[f.applyM.Instance]])
      val resultExpr = applyArgumentsOrAbort(applied, args)
      wrapResult[F, X](f.returnKind, resultExpr)
    }
    (bound: Map[String, Expr_??]) => bound(key)
  }

  /** Kindlings extension over macwire's autocats: use a companion `object T { def resource[F[_]:
    * <constraints>](<explicit value params>): Resource[F, T] }` factory when present. We apply OUR `F` as the method's
    * type argument, resolve any EXPLICIT value parameters from the wiring graph (just like a factory method's params),
    * and summon its context-bound implicits (which Hearth does not auto-supply), then register the folded
    * `Resource[F, T]` as a shared node. Returns a node builder, or `None` if no suitable `resource` method exists.
    */
  private def tryCompanionResource[F[_]](
      resolver: Resolver,
      t: ??,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String]
  )(implicit
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Option[Map[String, Expr_??] => Expr_??] = {
    import t.Underlying as X
    val key = Type.fqcn[X]
    implicit val resourceFX: Type[Resource[F, X]] = Type.of[Resource[F, X]]
    Type.companionObject[X].flatMap { companion =>
      companionResourceExplicitParamTypes(companion, "resource", FCtor.asUntyped.as_??, Type[Resource[F, X]].as_??)
        .map { explicitTypes =>
          // Resolve the resource method's EXPLICIT value params from the graph now (records their nodes + any missing
          // paths). Its implicit / context-bound clauses are summoned by the compiler inside companionResourceCall.
          val paramResolvers: List[Map[String, Expr_??] => Expr_??] =
            explicitTypes.zipWithIndex.map { case (ptype, i) =>
              resolveTypeViaResolver[F](resolver, ptype, path :+ s"[method resource].arg$i", missing)
                .getOrElse((_: Map[String, Expr_??]) => nullPlaceholder(ptype))
            }
          val depKeys = explicitTypes.flatMap(pt => resolvedKeyOf(resolver, pt))
          recordGraphNode(resolver, key, t, NodeKind.Provided("resource"), depKeys)
          ensureNode(resolver, key, t) { bound =>
            val explicitArgs = paramResolvers.map(_(bound))
            companionResourceCall(
              companion,
              "resource",
              FCtor.asUntyped.as_??,
              Type[Resource[F, X]].as_??,
              explicitArgs
            ).getOrElse(
              Environment.reportErrorAndAbort(s"Could not build [${Type.prettyPrint[X]}.resource[F]] from the graph.")
            )
          }
          (bound: Map[String, Expr_??]) => bound(key)
        }
    }
  }

  /** Reflectively inspect `companion.resource[F]` and, if it exists and (after applying `F`) yields a subtype of
    * `expected` (`Resource[F, <:T]`), return the F-substituted types of its single EXPLICIT value-parameter clause
    * (empty when it only has implicit / context-bound clauses). Returns `None` when there is no such method, its result
    * does not conform, or it has more than one explicit value clause.
    *
    * Platform specific for the same reason as [[companionResourceCall]]: Hearth's `Method` API drops a value/implicit
    * clause that follows a type-parameter clause (see `UntypedMethodsScala3.methodExpectations`; upstream
    * kubuszok/hearth#331), so we read the clause structure by raw reflection.
    */
  protected def companionResourceExplicitParamTypes(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??
  ): Option[List[??]]

  /** Build `T.resource[F](<explicitArgs>)` from `companion` (the companion object of `T`): apply the type argument `F`,
    * apply the already-resolved `explicitArgs` (in declared order) for the explicit value clause, and let the compiler
    * resolve any context-bound implicits (`Sync[F]`, ...), ascribing the result to `expected` (`Resource[F, T]`).
    * Returns `None` when there is no such method or it does not yield a `Resource[F, <:T]`.
    *
    * Platform specific: Hearth's `Method` API cannot supply an implicit/`using` clause that follows a type-parameter
    * clause (it exposes it as an empty parameter list — see `UntypedMethodsScala3.methodExpectations`), so we build the
    * call by raw reflection and rely on the compiler's own implicit search, which is exactly what the user's
    * `def resource[F[_]: Sync]` expects anyway.
    */
  protected def companionResourceCall(
      companion: Expr_??,
      methodName: String,
      fType: ??,
      expected: ??,
      explicitArgs: List[Expr_??]
  ): Option[Expr_??]

  /** Construct a value of type `t` via its primary constructor (or a single companion `apply`), resolving each
    * parameter via `resolveType`. Returns a builder producing the (constructed) value, or `None` if missing deps were
    * recorded. The construction is wrapped (`Resource.pure`) and memoized as a node so it is SHARED.
    */
  private def constructValue[F[_]](
      resolver: Resolver,
      t: ??,
      path: List[String],
      missing: scala.collection.mutable.LinkedHashSet[String],
      resolveType: (??, List[String]) => Option[Map[String, Expr_??] => Expr_??]
  )(implicit
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Option[Map[String, Expr_??] => Expr_??] = {
    import t.Underlying as P
    val key = Type.fqcn[P]
    if (resolver.planned.contains(key)) return Some((bound: Map[String, Expr_??]) => bound(key))
    // Kindlings extension over macwire's autocats: prefer a companion `T.resource[F](...): Resource[F, T]` factory if one
    // exists — apply our `F`, resolve its explicit value params from the graph, summon its context-bound implicits, and
    // fold the whole construction as a shared Resource node.
    tryCompanionResource[F](resolver, t, path, missing) match {
      case Some(builder) => return Some(builder)
      case None          => ()
    }
    // Cycle guard: a type re-entered while still under construction (its node not yet `planned`) is a dependency
    // cycle. Without this the DFS recurses forever — a compile-time StackOverflow. Mirrors di's `verifyNotCyclic`.
    if (resolver.inProgress.contains(key)) {
      val pathStr = if (path.isEmpty) "" else s" Path ${path.mkString(" -> ")}"
      Environment.reportErrorAndAbort(s"Cyclic dependency detected for [${Type.prettyPrint[P]}].$pathStr")
    }
    resolver.inProgress += key

    val result: Option[Map[String, Expr_??] => Expr_??] = callableFor(t) match {
      case None                       => recordMissing(missing, t, path); None
      case Some((method, ownerLabel)) =>
        val ctorParams = method.totalParameters.flatten.toList
        val paramResolvers: List[(String, Option[Map[String, Expr_??] => Expr_??])] =
          ctorParams.map { case (pname, parameter) =>
            if (parameter.isImplicit) pname -> Some(summonResolver(parameter))
            else pname -> resolveType(parameter.tpe, path :+ s"$ownerLabel.$pname")
          }
        val anyMissing = paramResolvers.exists { case (_, r) => r.isEmpty }
        recordGraphNode(resolver, key, t, NodeKind.Constructed, depKeysOf(resolver, ctorParams))
        // Reserve the memo key BEFORE building, so shared sub-structures resolve to a single binding.
        ensureNode(resolver, key, t) { bound =>
          val args = paramResolvers.collect { case (n, Some(r)) => n -> r(bound) }.toMap
          val built = applyArgumentsOrAbort(method, args)
          pureWrap[F](t, built)
        }
        if (anyMissing) None else Some((bound: Map[String, Expr_??]) => bound(key))
    }
    resolver.inProgress -= key
    result
  }

  /** A callable producing `t`: its primary constructor (label `[constructor T]`), or a single companion `apply` (label
    * `[method apply]`), already applied to the companion module. The returned label is used in path messages.
    */
  private def callableFor(t: ??): Option[(Method, String)] = {
    import t.Underlying as T
    Type[T].primaryConstructor.filter(_.isAvailable(AtCallSite)) match {
      case Some(ctor) => Some((ctor, s"[constructor ${Type.prettyPrint[T]}]"))
      case None       =>
        Type.companionObject[T].flatMap { companionExpr =>
          import companionExpr.{Underlying as Companion, value as companion}
          Class[Companion].method("apply").collect {
            case m: Method.OnInstance if m.isAvailable(AtCallSite) && returnsSubtypeOf[T](m) => m
          } match {
            case single :: Nil => Some((single.apply(companion.asInstanceOf[Expr[single.Instance]]), "[method apply]"))
            case _             => None
          }
        }
    }
  }

  private def returnsSubtypeOf[T: Type](m: Method): Boolean = m.knownReturning.exists { rt =>
    import rt.Underlying as R
    Type[R] <:< Type[T]
  }

  /** Resolve an implicit parameter at the call site (used by factory + construction param resolution). */
  private def summonResolver(parameter: Parameter): Map[String, Expr_??] => Expr_?? = {
    import parameter.tpe.Underlying as P
    Expr.summonImplicit[P].toOption match {
      case Some(found) => (_: Map[String, Expr_??]) => found.as_??
      case None => Environment.reportErrorAndAbort(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
    }
  }

  private def nullPlaceholder(t: ??): Expr_?? = {
    import t.Underlying as P
    Expr.quote(null.asInstanceOf[P]).as_??
  }

  /** Wrap a constructed plain value as `Resource.pure[F, X]`. */
  private def pureWrap[F[_]](t: ??, built: Expr_??)(implicit FCtor: Type.Ctor1[F]): Expr_?? = {
    import t.Underlying as X
    import built.Underlying
    pureToResource[F, X](built.value.upcast[X])
  }

  /** Wrap a factory result of underlying type `X` into `Resource[F, X]` per its return kind. */
  private def wrapResult[F[_], X: Type](kind: ReturnKind, resultExpr: Expr_??)(implicit FCtor: Type.Ctor1[F]): Expr_?? =
    kind match {
      case ReturnKind.ResourceK => resultExpr // already Resource[F, X]
      case ReturnKind.EffectK   => evalToResource[F, X](resultExpr)
      case ReturnKind.PlainK    =>
        import resultExpr.Underlying
        pureToResource[F, X](resultExpr.value.upcast[X])
    }

  private def applyArgumentsOrAbort(method: Method, arguments: Map[String, Expr_??]): Expr_?? =
    applyArguments(method, arguments) match {
      case Right(built) => built
      case Left(err)    => Environment.reportErrorAndAbort(err)
    }

  private def applyArguments(method: Method, arguments: Map[String, Expr_??]): Either[String, Expr_??] =
    method match {
      case av: Method.ApplyValues =>
        av.apply(arguments) match {
          case r: Method.Result[?] =>
            import r.Returned
            r.build().map(_.as_??)
          case other => Left(s"Unexpected step after applying arguments: ${other.getClass.getSimpleName}")
        }
      case r: Method.Result[?] =>
        import r.Returned
        r.build().map(_.as_??)
      case other => Left(s"Not callable: ${other.getClass.getSimpleName}")
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Emit the Resource chain (fold the monadic nodes; innermost = Resource.pure(root))
  // ---------------------------------------------------------------------------------------------------------------

  private def emitChain[F[_], T](
      nodes: List[Node],
      instanceBindings: Map[String, Expr_??],
      rootBuilder: Map[String, Expr_??] => Expr_??
  )(implicit T: Type[T], FCtor: Type.Ctor1[F], fAnyType: Type[F[Any]], anyType: Type[Any]): Expr[Resource[F, T]] = {
    implicit val resourceFT: Type[Resource[F, T]] = Type.of[Resource[F, T]]

    def fold(remaining: List[Node], bound: Map[String, Expr_??]): Expr[Resource[F, T]] =
      remaining match {
        case Nil =>
          // Innermost: the root is either a bound provider value of type T, or a freshly constructed T.
          val rootExpr = rootBuilder(bound)
          import rootExpr.Underlying as R
          // rootBuilder may yield either a `Resource[F, T]` (provider matched root) or a plain T construction.
          if (isResource[R]) rootExpr.value.asInstanceOf[Expr[Resource[F, T]]]
          else Expr.quote(Resource.pure[F, T](Expr.splice(rootExpr.value.upcast[T])))

        case node :: rest =>
          import node.x.Underlying as X
          val resourceExpr = node.monadic(bound).value.asInstanceOf[Expr[Resource[F, X]]]
          val lambda: Expr[X => Resource[F, T]] =
            LambdaBuilder.of1[X]("dep").buildWith { (x: Expr[X]) =>
              fold(rest, bound + (node.key -> x.as_??))
            }
          resourceFlatMap[F, X, T](resourceExpr, lambda)
      }

    fold(nodes, instanceBindings)
  }

  /** `resource.flatMap(lambda)` — kept in a helper with a regular type parameter `X` so the reified quote never leaks a
    * path-dependent alias into the call-site tree (a Scala 2 fresh-symbol scope error).
    */
  private def resourceFlatMap[F[_], X: Type, T](
      resource: Expr[Resource[F, X]],
      lambda: Expr[X => Resource[F, T]]
  )(implicit FCtor: Type.Ctor1[F], T: Type[T]): Expr[Resource[F, T]] =
    Expr.quote(Expr.splice(resource).flatMap(Expr.splice(lambda)))
}
