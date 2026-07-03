package hearth.kindlings.di
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.DirectStyle
import hearth.kindlings.macros.compiletime.{NodeKind, Storage, WiringGraph}

/** Shared, macro-platform-agnostic implementation of the wiring macros.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `Method`, `enclosingScope`, ...).
  */
private[di] trait WiringMacrosImpl
    extends hearth.kindlings.macros.compiletime.GenerationLogging
    with hearth.kindlings.macros.compiletime.WiringGraphLogging { this: MacroCommons =>

  protected val generationLoggingNamespace: String = "di"
  protected val wiringLoggingNamespace: String = "di"
  protected def generationMarkerImported: Boolean = {
    implicit val tpe: Type[DI.LogGeneration] = Type.of[DI.LogGeneration]
    Expr.summonImplicit[DI.LogGeneration].isDefined
  }

  /** One in-scope candidate value: the expression that reads it, together with the source-level identifier (member
    * name) it was read from — `theDatabaseAccess`, `theB1`, ... The name is what makes ambiguity errors actionable
    * ("Found multiple values of type [A]: [theB1, theB2]"), mirroring macwire which reports member names rather than
    * only the type.
    */
  final protected case class Candidate(name: String, value: Expr_??)

  /** macwire-style `wire[A]`: construct an `A` by resolving each parameter of its primary constructor (or, failing
    * that, of a single matching companion `apply`) from a value of a compatible type found in the enclosing lexical
    * scope. Implicit parameters are resolved by implicit search rather than from scope.
    */
  def wire[A: Type]: Expr[A] = wireLogged("DI.wire", recursive = false)

  private def wireLogged[A: Type](endpoint: String, recursive: Boolean): Expr[A] = {
    val scopes = collectScopes
    buildInstance[A](scopes, recursive = recursive, Nil) match {
      case Right(expr) =>
        if (shouldWeLogGeneration) {
          val trace = new Trace
          trace.step(s"$endpoint[${Type.prettyPrint[A]}] (recursive=$recursive)")
          trace.step(s"${scopes.map(_.size).sum} candidate value(s) in ${scopes.size} enclosing scope(s)")
          emitGenerationLog(endpoint, trace, expr.prettyPrint)
        }
        expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  /** macwire-style `wireRec[A]`: like [[wire]], but any constructor/apply parameter that cannot be found in the
    * enclosing scope is itself constructed recursively (instead of failing), as long as it is "wireable" — i.e. not a
    * `java.*`/`scala.*` library type, mirroring macwire's `isWireable` guard.
    */
  def wireRec[A: Type]: Expr[A] = wireLogged("DI.wireRec", recursive = true)

  /** macwire-style `wireWith[RES](factory)`: resolve each parameter of the supplied factory function from the enclosing
    * scope, then apply the factory to the resolved values. The factory is any `FunctionN` value (a lambda or an
    * eta-expanded method/constructor reference).
    *
    * Implicit parameters of the underlying method: macwire resolves them by implicit search at the wire site (see its
    * `wireWithImplicits` test, where `A.make(n)(implicit s: String)` is wired with an `implicit s` from scope). We
    * mirror that — [[resolveArguments]] routes every implicit parameter through [[resolveParameter]], which performs
    * implicit search. Note that an eta-expanded reference to a method with implicit parameters whose implicits are
    * resolvable at the eta-expansion site already has them applied (so the `FunctionN` SAM only exposes the explicit
    * params); when the eta-expansion exposes the implicit param as a SAM parameter, we resolve it here.
    */
  def wireWith[RES: Type](factory: Expr[Any]): Expr[RES] = {
    val scopes = collectScopes
    // `factory.tpe` carries the precise `FunctionN[...]` type of the passed tree even though it is statically `Any`.
    val applyMethods = new Class[Any]()(factory.tpe).method("apply").collect {
      case m: Method.OnInstance if m.isAvailable(AtCallSite) => m
    }
    // A function value may expose several `apply`s (specialized/bridge variants); the genuine SAM has the most
    // parameters with the most specific (non-erased) types, so prefer the highest-arity one.
    val result = applyMethods.sortBy(m => -m.totalParameters.flatten.size).headOption match {
      case None         => Left(s"The factory passed to wireWith has no accessible apply method.")
      case Some(applyM) =>
        DirectStyle[Either[String, *]].scoped { runSafe =>
          val applied = applyM.apply(factory.asInstanceOf[Expr[applyM.Instance]])
          val arguments = resolveArguments(applied, scopes, recursive = false, runSafe, Nil)
          upcastResult[RES](runSafe(applyArguments(applied, arguments)))
        }
    }
    result match {
      case Right(expr)  => expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  /** macwire-style `wireSet[A]`: collect every value of a type conforming to `A` found in the enclosing scope into a
    * `Set[A]`.
    */
  def wireSet[A: Type]: Expr[Set[A]] =
    buildSet[A](matchingCandidates(collectScopes, Type[A].as_??))

  /** macwire-style `wireList[A]`: like [[wireSet]] but preserves the (declaration) order of the collected values as a
    * `List[A]`.
    */
  def wireList[A: Type]: Expr[List[A]] =
    buildList[A](matchingCandidates(collectScopes, Type[A].as_??))

  private def buildSet[A: Type](elements: List[Candidate]): Expr[Set[A]] =
    elements.foldRight(Expr.quote(Set.empty[A]))((element, acc) => consSet[A](upcastTo[A](element.value), acc))
  private def consSet[A: Type](element: Expr[A], acc: Expr[Set[A]]): Expr[Set[A]] =
    Expr.quote(Expr.splice(acc) + Expr.splice(element))

  private def buildList[A: Type](elements: List[Candidate]): Expr[List[A]] =
    elements.foldRight(Expr.quote(List.empty[A]))((element, acc) => consList[A](upcastTo[A](element.value), acc))
  private def consList[A: Type](element: Expr[A], acc: Expr[List[A]]): Expr[List[A]] =
    Expr.quote(Expr.splice(element) :: Expr.splice(acc))

  private def upcastTo[A: Type](candidate: Expr_??): Expr[A] = {
    import candidate.Underlying
    candidate.value.upcast[A]
  }

  /** macwire-style `wiredInModule(instance)`: capture every public, parameterless member (`val`/no-arg `def`) of
    * `instance` whose type is a reference type into a runtime [[hearth.kindlings.di.Wired]] registry, each keyed by its
    * declared type. Primitive-typed members are excluded (mirroring macwire's `result <:< AnyRef` filter). The registry
    * stores factories (`() => Any`) so a `def` member is re-read on each lookup.
    */
  def wiredInModule(instance: Expr[Any]): Expr[Wired] = {
    val typed = preciseExpr(instance)
    import typed.Underlying as T
    val members = membersAsValues(typed, Class[T].methods).filter { c =>
      import c.value.Underlying as M
      Type[M] <:< Type.of[AnyRef]
    }
    val entries = members.foldRight(Expr.quote(List.empty[(java.lang.Class[Any], () => Any)])) { (candidate, acc) =>
      import candidate.value.Underlying as M
      consWiredEntry[M](candidate.value.value, acc)
    }
    Expr.quote(Wired(Expr.splice(entries)))
  }

  /** Prepend one `(class, () => member)` entry to the accumulated registry list. A helper with a regular type parameter
    * `M` so the member's (path-dependent) type does not leak into the generated tree on Scala 2.
    *
    * The class key is obtained from a summoned `ClassTag[M]` rather than a `classOf[M]` literal: `classOf[M]` cannot be
    * written for the abstract type parameter `M` (it is type-checked at macro-definition time → "class type required"),
    * and reifying `classOf` from a `Type` is not exposed uniformly by Hearth across platforms. Summoning `ClassTag[M]`
    * (materialised by the compiler at macro-execution time, where `M` is the concrete member type) and reading its
    * `runtimeClass` is fully cross-platform.
    */
  private def consWiredEntry[M: Type](
      member: Expr[M],
      acc: Expr[List[(java.lang.Class[Any], () => Any)]]
  ): Expr[List[(java.lang.Class[Any], () => Any)]] = {
    implicit val classTagType: Type[scala.reflect.ClassTag[M]] = Type.of[scala.reflect.ClassTag[M]]
    Expr.summonImplicit[scala.reflect.ClassTag[M]].toOption match {
      case Some(classTag) =>
        Expr.quote {
          (Expr.splice(classTag).runtimeClass.asInstanceOf[java.lang.Class[Any]], (() => (Expr.splice(member): Any))) ::
            Expr.splice(acc)
        }
      // No ClassTag materialisable (should not happen for a concrete member type) — skip the entry.
      case None => acc
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Candidate collection
  // ---------------------------------------------------------------------------------------------------------------

  /** Collect every value reachable from the enclosing lexical scope, grouped by scope and ordered from the INNERMOST
    * scope outward (mirroring macwire's `EligibleValuesFinder`). Each element is one scope's candidate values:
    *   - accessible nullary members (`val`/`def`/`lazy val`) of the enclosing classes and objects (declared and
    *     inherited), read off their `this`/module reference;
    *   - the public parameterless members of any in-scope value whose type is annotated with [[Module]] (or inherits
    *     such a type), exposed as a slightly-wider scope right after the value's own scope; and
    *   - `val`s local to the enclosing method declared before the call site (Scala 2 only — see
    *     [[hearth.Environments.Enclosure.LocalValue]]).
    *
    * Resolution then prefers the nearest scope that yields exactly one match (see [[resolveSingle]]); set/list wiring
    * flattens across all scopes.
    */
  protected def collectScopes: List[List[Candidate]] =
    enclosingScope.iterator
      .flatMap {
        case enc: Enclosure.Class  => scopesForMembers(enc.thisRef, enc.members)
        case enc: Enclosure.Object => scopesForMembers(Some(enc.thisRef), enc.members)
        case enc: Enclosure.Method => List(enc.localValues.map(lv => Candidate(lv.name, lv.ref)))
        case _: Enclosure.Value    => Nil
        case _: Enclosure.Package  => Nil
      }
      .toList
      .filter(_.nonEmpty)

  /** Direct members of an enclosing instance form one scope; the public parameterless members of any `@Module`-typed
    * value among them form an immediately-wider scope.
    */
  private def scopesForMembers(self: Option[Expr_??], members: List[Method]): List[List[Candidate]] =
    self.toList.flatMap { s =>
      val direct = membersAsValues(s, members)
      val moduleExpanded = direct.flatMap(expandIfModule)
      if (moduleExpanded.isEmpty) List(direct) else List(direct, moduleExpanded)
    }

  /** If `value`'s type is annotated with [[Module]] (or inherits such a type), expose its public parameterless members
    * as candidate values read off `value`.
    */
  private def expandIfModule(candidate: Candidate): List[Candidate] = {
    import candidate.value.Underlying as T
    if (isModuleType[T]) membersAsValues(candidate.value, Class[T].methods) else Nil
  }

  /** A value counts as a module when its type — or any of its base classes, mirroring macwire's
    * `tpe.baseClasses.exists(hasModuleAnnotation)` — is annotated with [[Module]].
    */
  private def isModuleType[T: Type]: Boolean = {
    implicit val moduleTpe: Type[Module] = Type.of[Module]
    Type[T].baseClasses.exists { base =>
      import base.Underlying as B
      Type[B].annotationsOfType[Module].nonEmpty
    }
  }

  /** The universal members every reference inherits from `java.lang.Object`/`scala.Any`. They are nullary with a known
    * return type (`hashCode: Int`, `toString: String`, ...), so without this guard they would leak into the candidate
    * pool — e.g. `hashCode`/`##` would match an `Int` parameter and `toString` a `String` one. macwire excludes them
    * too (its `import base._` only ever exposes genuine members); we mirror that by name, the only
    * cross-platform-stable signal available here.
    */
  private val ObjectMemberNames: Set[String] =
    Set("hashCode", "##", "toString", "getClass", "clone", "notify", "notifyAll", "finalize")

  /** Turn the accessible nullary members of an enclosing instance into candidate value expressions, each tagged with
    * the member name it was read from. Universal `Object` members are filtered out (see [[ObjectMemberNames]]).
    */
  private def membersAsValues(self: Expr_??, members: List[Method]): List[Candidate] = {
    import self.value as selfExpr
    members.collect {
      case m: Method.OnInstance
          if m.isNullary && m.knownReturning.isDefined && m.isAvailable(AtCallSite) &&
            !ObjectMemberNames.contains(m.name) =>
        m.apply(selfExpr.asInstanceOf[Expr[m.Instance]]) match {
          case r: Method.Result[?] =>
            import r.Returned
            r.build().toOption.map(e => Candidate(m.name, e.as_??))
          case _ => None
        }
    }.flatten
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Resolution
  // ---------------------------------------------------------------------------------------------------------------

  /** Resolve a single constructor/apply parameter: implicit parameters go through implicit search, everything else is
    * resolved from the collected scope candidates.
    */
  protected def resolveParameter(scopes: List[List[Candidate]], recursive: Boolean, breadcrumb: List[??])(
      parameter: Parameter
  ): Either[String, Expr_??] = {
    // For a by-name parameter (`=> A`) the declared type is the `<byname>[A]` wrapper; we resolve a strict `A` from
    // scope and let the method application thunk it (mirroring macwire's `wireByNameParameters` handling).
    val effectiveType = effectiveParamType(parameter)
    if (parameter.isImplicit) {
      import effectiveType.Underlying as P
      Expr.summonImplicit[P].toOption match {
        case Some(found) => Right(found.as_??)
        case None        => Left(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
      }
    } else if (recursive && matchingCandidates(scopes, effectiveType).isEmpty && isWireable(effectiveType)) {
      // Nothing of this type is in scope; in recursive mode, build it from scratch. The breadcrumb threads the chain
      // of types currently under construction so [[buildInstance]] can detect a dependency cycle instead of looping.
      import effectiveType.Underlying as P
      buildInstance[P](scopes, recursive = true, breadcrumb).map(_.as_??)
    } else resolveSingle(scopes)(effectiveType)
  }

  /** The type to resolve a parameter against. For a by-name parameter (`=> A`) we resolve the underlying `A` so a
    * strict value from scope satisfies it (the call site thunks it back). Hearth's `Parameter.byNameUnderlying`
    * surfaces that `A` directly on both platforms (Scala 3's `ByNameType` is not an applied type, so the old
    * `typeArguments` strip could not recover it — Hearth 0.3.1-48 closes that gap).
    */
  private def effectiveParamType(parameter: Parameter): ?? =
    parameter.byNameUnderlying.getOrElse(parameter.tpe)

  /** macwire's `isWireable`: refuse to recursively construct standard-library types (`java.*`/`scala.*`), which have no
    * meaningful wiring and would otherwise loop or produce confusing errors.
    */
  private def isWireable(tpe: ??): Boolean = {
    import tpe.Underlying as T
    val fqcn = Type.fqcn[T]
    !fqcn.startsWith("java.") && !fqcn.startsWith("scala.")
  }

  /** Resolve exactly one candidate whose type conforms to `parameterType`, preferring the nearest scope: search from
    * the innermost scope outward and return as soon as a scope yields matches — one match wins, several in the same
    * scope are an ambiguity error (mirroring macwire's `findInFirstScope` and its error messages).
    */
  protected def resolveSingle(scopes: List[List[Candidate]])(parameterType: ??): Either[String, Expr_??] = {
    import parameterType.Underlying as P
    def search(remaining: List[List[Candidate]]): Either[String, Expr_??] = remaining match {
      case Nil           => Left(s"Cannot find a value of type: [${Type.prettyPrint[P]}]")
      case scope :: rest =>
        matchingIn(scope, parameterType) match {
          case Nil           => search(rest)
          case single :: Nil => Right(single.value)
          // macwire reports the conflicting member NAMES so the error is actionable; we mirror that.
          case multiple =>
            Left(s"Found multiple values of type [${Type.prettyPrint[P]}]: [${multiple.map(_.name).mkString(", ")}]")
        }
    }
    search(scopes)
  }

  /** Every candidate of every scope whose type conforms to `parameterType` (flattened, innermost first) — used by
    * set/list wiring.
    */
  protected def matchingCandidates(scopes: List[List[Candidate]], parameterType: ??): List[Candidate] =
    scopes.flatMap(matchingIn(_, parameterType))

  /** Candidates within a single scope whose type conforms to `parameterType` (excluding the bottom types
    * `Nothing`/`Null`).
    */
  private def matchingIn(scope: List[Candidate], parameterType: ??): List[Candidate] = {
    import parameterType.Underlying as P
    scope.filter { c =>
      import c.value.Underlying as C
      Type[C] <:< Type[P] && !(Type[C] =:= Type.of[Nothing]) && !(Type[C] =:= Type.of[Null])
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Construction
  // ---------------------------------------------------------------------------------------------------------------

  /** Build an `A`: prefer its accessible primary constructor; otherwise fall back to a single matching companion
    * `apply`. Errors from both attempts are combined to explain why wiring failed.
    */
  private def buildInstance[A: Type](
      scopes: List[List[Candidate]],
      recursive: Boolean,
      breadcrumb: List[??]
  ): Either[String, Expr[A]] = {
    // Detect a dependency cycle before descending — without this, recursive wiring of a self-referential graph loops
    // the compiler (a StackOverflow). Mirrors `autowire`'s `verifyNotCyclic`. The chain passed downward includes `A`.
    verifyNotCyclic(Type[A].as_??, breadcrumb)
    val chain = breadcrumb :+ Type[A].as_??
    constructViaConstructor[A](scopes, recursive, chain) match {
      case Right(expr) => Right(expr)
      // The constructor is present but its parameters could not be wired — that IS the real error; reporting the
      // companion-apply fallback on top would only obscure it (mirrors macwire, which never falls back in this case).
      case Left(WiringFailed(error)) => Left(error)
      // The constructor is inaccessible/absent — try a companion `apply`, combining both explanations on failure.
      case Left(NoPublicCtor(ctorMsg)) =>
        constructViaCompanionApply[A](scopes, recursive, chain) match {
          case Right(expr)      => Right(expr)
          case Left(applyError) => Left(s"$ctorMsg\n$applyError")
        }
    }
  }

  /** Why constructing `A` via its primary constructor was abandoned. [[NoPublicCtor]] means there is no accessible
    * primary constructor (so a companion `apply` should be tried); [[WiringFailed]] means the constructor exists but
    * its parameters could not be resolved (a real, final error).
    */
  sealed private trait CtorFailure
  final private case class NoPublicCtor(message: String) extends CtorFailure
  final private case class WiringFailed(message: String) extends CtorFailure

  private def constructViaConstructor[A: Type](
      scopes: List[List[Candidate]],
      recursive: Boolean,
      breadcrumb: List[??]
  ): Either[CtorFailure, Expr[A]] =
    Type[A].primaryConstructor.filter(_ => isInstantiable[A]).filter(_.isAvailable(AtCallSite)) match {
      case None       => Left(NoPublicCtor(s"No public primary constructor found for [${Type.prettyPrint[A]}]"))
      case Some(ctor) =>
        DirectStyle[Either[String, *]]
          .scoped { runSafe =>
            val arguments = resolveArguments(ctor, scopes, recursive, runSafe, breadcrumb)
            upcastResult[A](runSafe(applyArguments(ctor, arguments)))
          }
          .left
          .map(WiringFailed.apply)
    }

  private def constructViaCompanionApply[A: Type](
      scopes: List[List[Candidate]],
      recursive: Boolean,
      breadcrumb: List[??]
  ): Either[String, Expr[A]] =
    Type.companionObject[A] match {
      case None =>
        Left(s"Companion object for [${Type.prettyPrint[A]}] has no apply methods constructing target type.")
      case Some(companionExpr) =>
        import companionExpr.{Underlying as Companion, value as companion}
        // Filter to `apply`s that actually return `A` (a subtype) — `apply`s with a foreign return type are "fake".
        val applyMethods = Class[Companion].method("apply").collect {
          case m: Method.OnInstance if m.isAvailable(AtCallSite) && returnsSubtypeOf[A](m) => m
        }
        applyMethods match {
          case Nil =>
            Left(s"Companion object for [${Type.prettyPrint[A]}] has no apply methods constructing target type.")
          case single :: Nil =>
            DirectStyle[Either[String, *]].scoped { runSafe =>
              val arguments = resolveArguments(single, scopes, recursive, runSafe, breadcrumb)
              val applied = single.apply(companion.asInstanceOf[Expr[single.Instance]])
              upcastResult[A](runSafe(applyArguments(applied, arguments)))
            }
          case _ =>
            Left(
              s"and multiple matching apply methods in its companion object were found for [${Type.prettyPrint[A]}]."
            )
        }
    }

  private def upcastResult[A: Type](built: Expr_??): Expr[A] = {
    import built.Underlying
    built.value.upcast[A]
  }

  private def returnsSubtypeOf[A: Type](m: Method): Boolean = m.knownReturning.exists { rt =>
    import rt.Underlying as R
    Type[R] <:< Type[A]
  }

  /** Resolve every value parameter of `method` (implicit lookups included), aborting the surrounding direct-style scope
    * with the first failure.
    */
  private def resolveArguments(
      method: Method,
      scopes: List[List[Candidate]],
      recursive: Boolean,
      runSafe: DirectStyle.RunSafe[Either[String, *]],
      breadcrumb: List[??]
  ): Map[String, Expr_??] =
    method.totalParameters.flatten.toList.map { case (name, parameter) =>
      name -> runSafe(resolveParameter(scopes, recursive, breadcrumb)(parameter))
    }.toMap

  /** Apply resolved arguments to a callable [[Method]] step (constructor or `apply`) and build the result. */
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
  // autowire — explicit-dependency object-graph construction
  // ---------------------------------------------------------------------------------------------------------------

  /** macwire-style `autowire[A](dependencies*)`: build a complete object graph producing an `A` from an explicitly
    * provided list of dependencies (instances and factory functions), constructing everything else via public
    * constructors / companion `apply`s. Each distinct type is instantiated exactly once and shared through a generated
    * local `val` (a let-block ending in the root), mirroring macwire. Detects cyclic dependencies, refuses primitives
    * and `String`, and reports any provided dependency that ended up unused.
    */
  def autowire[A: Type](rawDeps: List[Expr_??]): Expr[A] = {
    checkDuplicateDependencyTypes(rawDeps)
    val used = scala.collection.mutable.Set.empty[Int]
    // Collected in resolution order, keyed by type FQCN — feeds both the ZIO-Magic wiring graph and the full log.
    val graphNodes = scala.collection.mutable.LinkedHashMap.empty[String, WiringGraph.RawNode]
    val rootKey: String = Type.fqcn[A]

    val block: Expr[A] = DirectStyle[ValDefs].scoped { runSafe =>
      val memo = scala.collection.mutable.LinkedHashMap.empty[String, Expr_??]

      def valNode[T: Type](built: Expr_??): Expr_?? = {
        import built.Underlying
        runSafe(ValDefs.createVal[T](built.value.upcast[T])).as_??
      }

      // Build via `method`, returning the expression AND the FQCN keys of the dependencies it consumed (for the graph).
      def buildViaMethod(t: ??, method: Method, breadcrumb: List[??]): (Expr_??, List[String]) = {
        val params = method.totalParameters.flatten.toList
        val arguments: Map[String, Expr_??] = params.map { case (name, parameter) =>
          val ref =
            if (parameter.isImplicit) {
              val pk = keyOf(parameter.tpe)
              val _ = graphNodes.getOrElseUpdate(
                pk,
                WiringGraph.RawNode(pk, displayOf(parameter.tpe), NodeKind.Summoned, Storage.Inline, Nil)
              )
              summonOrAbort(parameter.tpe)
            } else expand(parameter.tpe, breadcrumb :+ t)
          name -> ref
        }.toMap
        val depKeys = params.map { case (_, parameter) => keyOf(parameter.tpe) }
        applyArguments(method, arguments) match {
          case Right(built) => (built, depKeys)
          case Left(err)    => Environment.reportErrorAndAbort(err)
        }
      }

      def expand(t: ??, breadcrumb: List[??]): Expr_?? = {
        import t.Underlying as T
        val key = Type.fqcn[T]
        memo.getOrElseUpdate(
          key, {
            verifyNotCyclic(t, breadcrumb)
            verifyNotPrimitive(t, breadcrumb)
            findInstanceProvider(t, rawDeps) match {
              case Some((dep, i)) =>
                used += i
                graphNodes(key) =
                  WiringGraph.RawNode(key, displayOf(t), NodeKind.Provided("instance"), Storage.Inline, Nil)
                import dep.Underlying
                dep.value.upcast[T].as_??
              case None =>
                findFactoryProvider(t, rawDeps) match {
                  case Some((dep, i, applyM)) =>
                    used += i
                    val applied = applyM.apply(dep.value.asInstanceOf[Expr[applyM.Instance]])
                    val (built, depKeys) = buildViaMethod(t, applied, breadcrumb)
                    graphNodes(key) =
                      WiringGraph.RawNode(key, displayOf(t), NodeKind.Provided("factory"), Storage.Val, depKeys)
                    valNode[T](built)
                  case None =>
                    providerCallable[T] match {
                      case Some(method) =>
                        val (built, depKeys) = buildViaMethod(t, method, breadcrumb)
                        graphNodes(key) =
                          WiringGraph.RawNode(key, displayOf(t), NodeKind.Constructed, Storage.Val, depKeys)
                        valNode[T](built)
                      case None =>
                        Environment.reportErrorAndAbort(
                          s"cannot find a provided dependency, public constructor or public apply method for: ${Type
                              .prettyPrint[T]}; ${wiringPath(breadcrumb :+ t)}"
                        )
                    }
                }
            }
          }
        )
      }

      val rootRef = expand(Type[A].as_??, Nil)
      import rootRef.Underlying
      rootRef.value.upcast[A]
    }.close

    verifyAllDependenciesUsed(rawDeps, used.toSet)
    emitWiringGraphIfEnabled("DI.autowire", rootKey, graphNodes, wiringLogModeFromSettings)
    if (shouldWeLogGeneration) {
      val trace = new Trace
      trace.step(s"autowire[${Type.prettyPrint[A]}] with ${rawDeps.size} provided dependency(ies)")
      trace.step("wiring graph:")
      trace.step(wiringTreeFor(rootKey, graphNodes))
      emitGenerationLog("DI.autowire", trace, block.prettyPrint)
    }
    block
  }

  /** Stable per-type key (FQCN) used to identify nodes in the wiring graph. */
  private def keyOf(t: ??): String = { import t.Underlying as T; Type.fqcn[T] }

  /** Human-readable type name for the wiring graph (ANSI colour codes from `prettyPrint` stripped for clean output). */
  private def displayOf(t: ??): String = {
    import t.Underlying as T
    Type.prettyPrint[T].replaceAll("\u001b\\[[0-9;]*m", "")
  }

  // ===================================================================================================================
  // DI.plan — Kindlings' own opinionated wiring endpoint (NOT macwire): always recursive, always caching, with a
  // fluent builder for the storage strategy (val / lazy val / def, whole-graph default + per-type override) and
  // per-type construction overrides (`provide[T]`). See DIPlan.
  // ===================================================================================================================

  /** One `provide[T](factory)` construction override: the target type and the (by-name) factory expression. */
  final private case class PlanProvider(tpe: ??, factory: DestructuredExpr)

  /** The configuration recovered from a `DI.plan[A]....` builder chain. */
  final private case class PlanConfig(
      default: Storage,
      overrides: Map[String, Storage],
      providers: Map[String, PlanProvider],
      debug: Option[WiringLogMode]
  ) {
    def withOverride(key: String, s: Storage): PlanConfig = copy(overrides = overrides.updated(key, s))
    def withProvider(t: ??, factory: DestructuredExpr): PlanConfig = {
      import t.Underlying as T
      copy(providers = providers.updated(Type.fqcn[T], PlanProvider(t, factory)))
    }
    def storageFor(key: String): Storage = overrides.getOrElse(key, default)
  }
  private object PlanConfig {
    val empty: PlanConfig = PlanConfig(Storage.Val, Map.empty, Map.empty, None)
  }

  /** `DI.plan[A]....build`: parse the builder chain, then build a complete, always-cached object graph for `A`,
    * honouring the requested storage strategy and construction overrides.
    */
  def buildPlan[A: Type](plan: Expr[DIPlan[A]]): Expr[A] = {
    val config = parsePlanConfig[A](plan)
    val graphNodes = scala.collection.mutable.LinkedHashMap.empty[String, WiringGraph.RawNode]
    val rootKey: String = Type.fqcn[A]

    val block: Expr[A] = DirectStyle[ValDefs].scoped { runSafe =>
      val memo = scala.collection.mutable.LinkedHashMap.empty[String, Expr_??]

      def store[T: Type](value: Expr[T], storage: Storage): Expr_?? = {
        val vd = storage match {
          case Storage.Val     => ValDefs.createVal[T](value)
          case Storage.LazyVal => ValDefs.createLazy[T](value)
          case Storage.Def     => ValDefs.createDef[T](value)
          case Storage.Inline  => ValDefs.createVal[T](value)
        }
        runSafe(vd).as_??
      }

      def buildViaMethod(t: ??, method: Method, breadcrumb: List[??]): (Expr_??, List[String]) = {
        val params = method.totalParameters.flatten.toList
        val arguments: Map[String, Expr_??] = params.map { case (name, parameter) =>
          val ref =
            if (parameter.isImplicit) {
              val pk = keyOf(parameter.tpe)
              val _ = graphNodes.getOrElseUpdate(
                pk,
                WiringGraph.RawNode(pk, displayOf(parameter.tpe), NodeKind.Summoned, Storage.Inline, Nil)
              )
              summonOrAbort(parameter.tpe)
            } else expand(parameter.tpe, breadcrumb :+ t)
          name -> ref
        }.toMap
        val depKeys = params.map { case (_, parameter) => keyOf(parameter.tpe) }
        applyArguments(method, arguments) match {
          case Right(built) => (built, depKeys)
          case Left(err)    => Environment.reportErrorAndAbort(err)
        }
      }

      def expand(t: ??, breadcrumb: List[??]): Expr_?? = {
        import t.Underlying as T
        val key = Type.fqcn[T]
        memo.getOrElseUpdate(
          key, {
            verifyNotCyclic(t, breadcrumb)
            verifyNotPrimitive(t, breadcrumb)
            val storage = config.storageFor(key)
            config.providers.get(key) match {
              case Some(provider) =>
                // Construction override: initialise `T` with the user-supplied factory instead of a constructor.
                val factory: Expr[T] = provider.factory.toUntypedExpr.asTyped[T]
                graphNodes(key) = WiringGraph.RawNode(key, displayOf(t), NodeKind.Provided("provide"), storage, Nil)
                store[T](factory, storage)
              case None =>
                providerCallable[T] match {
                  case Some(method) =>
                    val (built, depKeys) = buildViaMethod(t, method, breadcrumb)
                    graphNodes(key) = WiringGraph.RawNode(key, displayOf(t), NodeKind.Constructed, storage, depKeys)
                    import built.Underlying
                    store[T](built.value.upcast[T], storage)
                  case None =>
                    Environment.reportErrorAndAbort(
                      s"DI.plan: cannot find a public constructor, companion apply, or `.provide[_]` override for: ${Type
                          .prettyPrint[T]}; ${wiringPath(breadcrumb :+ t)}"
                    )
                }
            }
          }
        )
      }

      val rootRef = expand(Type[A].as_??, Nil)
      import rootRef.Underlying
      rootRef.value.upcast[A]
    }.close

    emitWiringGraphIfEnabled("DI.plan", rootKey, graphNodes, config.debug.orElse(wiringLogModeFromSettings))
    if (shouldWeLogGeneration) {
      val trace = new Trace
      trace.step(s"plan[${Type.prettyPrint[A]}] (default storage: ${config.default.label})")
      if (config.overrides.nonEmpty) trace.step(s"storage overrides: ${config.overrides.size}")
      if (config.providers.nonEmpty) trace.step(s"construction overrides: ${config.providers.keys.mkString(", ")}")
      trace.step("wiring graph:")
      trace.step(wiringTreeFor(rootKey, graphNodes))
      emitGenerationLog("DI.plan", trace, block.prettyPrint)
    }
    block
  }

  /** Walk the `DI.plan[A].asLazyVals.storeAsDef[X].provide[Y](f).debugTree` chain (via [[DestructuredExpr]]) and fold
    * it into a [[PlanConfig]]. The receiver of each builder call is processed first, so an outer (later) call wins over
    * an inner (earlier) one for the whole-graph default.
    */
  private def parsePlanConfig[A: Type](plan: Expr[DIPlan[A]]): PlanConfig = {
    implicit val planTpe: Type[DIPlan[A]] = Type.of[DIPlan[A]]
    def loop(d: DestructuredExpr, acc: PlanConfig): PlanConfig = d match {
      case mc: DestructuredExpr.MethodCall =>
        val receiver = mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }
        val base = receiver.map(loop(_, acc)).getOrElse(acc)
        applyPlanCall(mc, base)
      case _ => acc
    }
    loop(DestructuredExpr.parse[DIPlan[A]](plan), PlanConfig.empty)
  }

  private def applyPlanCall(mc: DestructuredExpr.MethodCall, acc: PlanConfig): PlanConfig = {
    def typeArg: Option[??] =
      mc.applied.collectFirst {
        case at: DestructuredExpr.MethodCall.AppliedTypes if at.typeArgs.nonEmpty => at.typeArgs.last
      }
    def valueArg: Option[DestructuredExpr] =
      mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }.flatten.headOption
    def overrideStorage(s: Storage): PlanConfig =
      typeArg.fold(acc) { t => import t.Underlying as T; acc.withOverride(Type.fqcn[T], s) }
    // Enum-argument DSL: read the `PlanStorage.*` / `PlanDebug.*` case-object passed as the value argument by matching
    // its (singleton) static type. Returns None if the argument is not one of the known singletons.
    def storageArg: Option[Storage] = valueArg.flatMap { d =>
      import d.tpe.Underlying as V
      if (Type[V] <:< Type.of[PlanStorage.Val.type]) Some(Storage.Val)
      else if (Type[V] <:< Type.of[PlanStorage.LazyVal.type]) Some(Storage.LazyVal)
      else if (Type[V] <:< Type.of[PlanStorage.Def.type]) Some(Storage.Def)
      else None
    }
    def debugArg: Option[WiringLogMode] = valueArg.flatMap { d =>
      import d.tpe.Underlying as V
      if (Type[V] <:< Type.of[PlanDebug.Tree.type]) Some(WiringLogMode.Tree)
      else if (Type[V] <:< Type.of[PlanDebug.Mermaid.type]) Some(WiringLogMode.Mermaid)
      else None
    }
    mc.method.name match {
      case "asVals"         => acc.copy(default = Storage.Val)
      case "asLazyVals"     => acc.copy(default = Storage.LazyVal)
      case "asDefs"         => acc.copy(default = Storage.Def)
      case "storeAsVal"     => overrideStorage(Storage.Val)
      case "storeAsLazyVal" => overrideStorage(Storage.LazyVal)
      case "storeAsDef"     => overrideStorage(Storage.Def)
      // Enum-argument forms (Issue #4): the same effect, chosen by a `PlanStorage` / `PlanDebug` value.
      case "defaultStorage" => storageArg.fold(acc)(s => acc.copy(default = s))
      case "storeAs"        => storageArg.fold(acc)(overrideStorage)
      case "debug"          => debugArg.fold(acc)(m => acc.copy(debug = Some(m)))
      case "provide"        =>
        (typeArg, valueArg) match {
          case (Some(t), Some(factory)) => acc.withProvider(t, factory)
          case _                        => acc
        }
      case "debugTree"    => acc.copy(debug = Some(WiringLogMode.Tree))
      case "debugMermaid" => acc.copy(debug = Some(WiringLogMode.Mermaid))
      case _              => acc // `plan`, wrappers, etc.
    }
  }

  /** Recover the precise static type of a single provided dependency expression. Both platforms hand the macro the
    * individual argument expressions (Scala 2 directly as `c.Expr[Any]*`, Scala 3 via `scala.quoted.Varargs`); their
    * static `Any` type is widened back to the real type through [[DestructuredExpr]].
    */
  def preciseExpr(e: Expr[Any]): Expr_?? = {
    implicit val anyType: Type[Any] = Type.of[Any]
    toTypedExpr(DestructuredExpr.parse(e))
  }

  /** [[autowire]] entry point that first expands any `DI.autowireMembersOf(instance)` marker argument into the public
    * parameterless members of `instance` (each as an instance dependency), mirroring macwire's `autowireMembersOf`.
    * Non-marker arguments are precise-typed as usual. The expanded members flow through the ordinary `autowire` pool,
    * so they participate in the same duplicate-type detection (a member type clashing with another dependency fails).
    */
  def autowireWithMembers[A: Type](rawArgs: List[Expr[Any]]): Expr[A] = {
    implicit val anyType: Type[Any] = Type.of[Any]
    val expanded: List[Expr_??] = rawArgs.flatMap { raw =>
      val parsed = DestructuredExpr.parse(raw)
      membersOfMarkerInstance(parsed) match {
        case Some(instance) => expandMembersOf(instance)
        case None           => List(toTypedExpr(parsed))
      }
    }
    autowire[A](expanded)
  }

  /** If `parsed` is a `DI.autowireMembersOf(instance)` call, recover the (precise-typed) wrapped `instance`. Recognized
    * by the marker method name (as macwire does) and the single value argument it was applied to.
    */
  private def membersOfMarkerInstance(parsed: DestructuredExpr): Option[Expr_??] =
    parsed
      .collect { case mc: DestructuredExpr.MethodCall if mc.method.name == "autowireMembersOf" => mc }
      .headOption
      .flatMap(_.applied.collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues => av.args })
      .flatMap(_.headOption)
      .map(toTypedExpr)

  /** Every public, parameterless member of `instance` (`val`/no-arg `def`) as an instance dependency, matched by its
    * declared type — reuses [[membersAsValues]] (which already filters `Object` members and keeps only accessible
    * nullary members), additionally dropping synthetic case-class/tuple accessors so they cannot leak as providers.
    */
  private def expandMembersOf(instance: Expr_??): List[Expr_??] = {
    import instance.Underlying as T
    membersAsValues(instance, Class[T].methods).collect {
      case c if !isSyntheticMember(c.name) => c.value
    }
  }

  /** Synthetic, structurally-generated members that macwire's `providersFromMembersOf` also excludes (so e.g. a case
    * class's `productArity: Int` does not become an `Int` provider, nor a tuple's `_1`/`_2`).
    */
  private def isSyntheticMember(name: String): Boolean =
    name == "productArity" || name == "productPrefix" || name == "productIterator" ||
      name == "productElementNames" || name.matches("_\\d+")

  private def toTypedExpr(e: DestructuredExpr): Expr_?? = {
    import e.tpe.Underlying as T
    e.toUntypedExpr.asTyped[T].as_??
  }

  /** A provided dependency is a factory when its type is a `scala.FunctionN`. */
  private def isFunctionExpr(dep: Expr_??): Boolean = {
    import dep.Underlying as D
    Type.fqcn[D].startsWith("scala.Function")
  }

  /** The highest-arity `apply` of a factory function value (its single SAM, ignoring specialized/bridge variants). */
  private def factoryApply(dep: Expr_??): Option[Method.OnInstance] = {
    import dep.Underlying as D
    new Class[D]()
      .method("apply")
      .collect {
        case m: Method.OnInstance if m.isAvailable(AtCallSite) => m
      }
      .sortBy(m => -m.totalParameters.flatten.size)
      .headOption
  }

  private def findInstanceProvider(t: ??, rawDeps: List[Expr_??]): Option[(Expr_??, Int)] = {
    import t.Underlying as T
    rawDeps.zipWithIndex.find { case (dep, _) =>
      !isFunctionExpr(dep) && {
        import dep.Underlying as D
        Type[D] <:< Type[T]
      }
    }
  }

  private def findFactoryProvider(t: ??, rawDeps: List[Expr_??]): Option[(Expr_??, Int, Method.OnInstance)] = {
    import t.Underlying as T
    rawDeps.zipWithIndex.iterator
      .flatMap { case (dep, i) =>
        if (isFunctionExpr(dep)) factoryApply(dep).flatMap { applyM =>
          applyM.knownReturning.collect {
            case rt if { import rt.Underlying as R; Type[R] <:< Type[T] } => (dep, i, applyM)
          }
        }
        else None
      }
      .nextOption()
  }

  /** A callable [[Method]] producing `T`: its public primary constructor, or a single matching companion `apply`
    * (already applied to the companion module).
    */
  private def providerCallable[T: Type]: Option[Method] =
    Type[T].primaryConstructor
      .filter(_ => isInstantiable[T])
      .filter(_.isAvailable(AtCallSite))
      .orElse(companionAppliedMethod[T])

  /** A type can be built via its primary constructor only when it is concrete — not a `trait` and not `abstract`
    * (mirroring macwire, which refuses to instantiate abstract types and reports them as un-constructible).
    */
  private def isInstantiable[T: Type]: Boolean = !Type[T].isAbstract && !Type[T].isTrait

  private def companionAppliedMethod[T: Type]: Option[Method] =
    Type.companionObject[T].flatMap { companionExpr =>
      import companionExpr.{Underlying as Companion, value as companion}
      Class[Companion].method("apply").collect {
        case m: Method.OnInstance if m.isAvailable(AtCallSite) && returnsSubtypeOf[T](m) => m
      } match {
        case single :: Nil => Some(single.apply(companion.asInstanceOf[Expr[single.Instance]]))
        case _             => None
      }
    }

  private def summonOrAbort(pt: ??): Expr_?? = {
    import pt.Underlying as P
    Expr.summonImplicit[P].toOption match {
      case Some(found) => found.as_??
      case None => Environment.reportErrorAndAbort(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
    }
  }

  private def checkDuplicateDependencyTypes(rawDeps: List[Expr_??]): Unit = {
    val seen = scala.collection.mutable.ListBuffer.empty[??]
    rawDeps.foreach { dep =>
      import dep.Underlying as D
      // Only instance dependencies can collide on type; factory functions are matched by their return type elsewhere.
      if (!isFunctionExpr(dep)) {
        val clash = seen.exists { s =>
          import s.Underlying as S
          Type[S] <:< Type[D] || Type[D] <:< Type[S]
        }
        if (clash)
          Environment.reportErrorAndAbort(
            s"duplicate type in dependencies list: ${Type.prettyPrint[D]}, for: ${describeDep(dep)}"
          )
        val _ = seen += Type[D].as_??
      }
    }
  }

  /** A best-effort source rendering of a provided dependency, used in autowire diagnostics ("for: a", "unused
    * dependencies: C.apply()"). Falls back to the type when the original source text is unavailable.
    */
  private def describeDep(dep: Expr_??): String =
    dep.value.sourceCode.getOrElse(dep.value.plainPrint)

  /** macwire-style breadcrumb of the in-progress wiring graph, e.g. `wiring path: A -> B -> A`. Surfacing it makes
    * autowire errors (cycles, missing providers, primitive leaves) actionable by showing HOW the failing type was
    * reached from the root.
    */
  private def wiringPath(path: List[??]): String =
    "wiring path: " + path.map { p => import p.Underlying as P; Type.prettyPrint[P] }.mkString(" -> ")

  private def verifyNotCyclic(t: ??, breadcrumb: List[??]): Unit = {
    import t.Underlying as T
    if (breadcrumb.exists { b => import b.Underlying as B; Type[B] <:< Type[T] })
      Environment.reportErrorAndAbort(s"cyclic dependencies detected; ${wiringPath(breadcrumb :+ t)}")
  }

  private def verifyNotPrimitive(t: ??, breadcrumb: List[??]): Unit = {
    import t.Underlying as T
    val isPrimitive =
      Type[T] =:= Type.of[Int] || Type[T] =:= Type.of[Long] || Type[T] =:= Type.of[Byte] ||
        Type[T] =:= Type.of[Short] || Type[T] =:= Type.of[Char] || Type[T] =:= Type.of[Boolean] ||
        Type[T] =:= Type.of[Double] || Type[T] =:= Type.of[Float] || Type[T] =:= Type.of[String]
    if (isPrimitive)
      Environment.reportErrorAndAbort(
        s"cannot use a primitive type or String in autowiring; ${wiringPath(breadcrumb :+ t)}"
      )
  }

  private def verifyAllDependenciesUsed(rawDeps: List[Expr_??], used: Set[Int]): Unit = {
    val unused = rawDeps.zipWithIndex.collect { case (dep, i) if !used.contains(i) => describeDep(dep) }
    if (unused.nonEmpty) Environment.reportErrorAndAbort(s"unused dependencies: ${unused.mkString(", ")}")
  }
}
