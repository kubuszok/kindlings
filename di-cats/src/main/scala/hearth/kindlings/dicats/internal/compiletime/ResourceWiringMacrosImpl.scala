package hearth.kindlings.dicats
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.DirectStyle
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
  * None of them require `Applicative[F]`/`Sync[F]`/`MonadCancel[F]`, so `wireResource` needs neither a summoned implicit
  * nor an `(implicit F: ...)` parameter. (`Resource.onFinalize`, `Resource.make`, etc. DO need constraints, but we never
  * emit those — acquisition/release is provided pre-built by the caller as a `Resource[F, X]` dependency.)
  */
private[dicats] trait ResourceWiringMacrosImpl { this: MacroCommons =>

  // ---------------------------------------------------------------------------------------------------------------
  // Provider model (port of macwire's CatsProviders.scala)
  // ---------------------------------------------------------------------------------------------------------------

  /** A classified input dependency. macwire's `Provider` hierarchy, trimmed to the V1 shapes.
    *
    * `resultType` is the type this provider ultimately yields into the construction:
    *   - [[Instance]]: the dependency's own type (spliced directly into the root construction — NO flatMap).
    *   - [[ResourceP]]/[[EffectP]]: the `X` of `Resource[F, X]` / `F[X]` (bound by a flatMap lambda var).
    */
  private sealed trait Provider {
    def resultType: ??
    def describe: String = {
      val rt = resultType
      import rt.Underlying as R
      Type.prettyPrint[R]
    }
  }

  /** A plain value `X`. Spliced directly wherever its type is needed; never flatMapped. */
  private final case class Instance(value: Expr_??) extends Provider {
    val resultType: ?? = {
      import value.Underlying as X
      Type[X].as_??
    }
  }

  /** A `Resource[F, X]` dependency: contributes `value.flatMap((x: X) => acc)` to the fold. */
  private final case class ResourceP(value: Expr_??, x: ??) extends Provider {
    def resultType: ?? = x
  }

  /** An `F[X]` effect dependency: contributes `Resource.eval[F, X](value).flatMap((x: X) => acc)` to the fold. */
  private final case class EffectP(value: Expr_??, x: ??) extends Provider {
    def resultType: ?? = x
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Entry point
  // ---------------------------------------------------------------------------------------------------------------

  /** Build a `Resource[F, T]` that constructs a `T` from the supplied `dependencies`.
    *
    * Algorithm (port of macwire autocats — see `docs/research/di-cats-resource-wiring.md`):
    *   1. Classify each dependency into [[Instance]] / [[ResourceP]] / [[EffectP]].
    *   2. Resolve `T` via its primary constructor (or a single companion `apply`), resolving each parameter to the
    *      single provider whose `resultType <:< paramType`.
    *   3. Fold the distinct monadic providers (Resource/Effect, in input order) into nested `flatMap`s, innermost body
    *      `Resource.pure[F, T](rootConstruction)`.
    *
    * V1 scope (implemented): instances, `Resource[F, X]`, `F[X]`, root via primary ctor OR companion apply, parameter
    * resolution with macwire-parity errors (missing / ambiguous / no-ctor).
    *
    * V2 (deferred — see the design doc): recursive construction of missing wireable params (`wireRec`-like), factory
    * method (FunctionN) dependencies, `verifyOrder` unused-provider error, topological sort for inter-provider deps.
    */
  def wireResource[F[_], T](
      dependencies: List[Expr_??]
  )(implicit T: Type[T], FCtor: Type.Ctor1[F]): Expr[Resource[F, T]] = {
    // Type[F[Any]] / Type[Any] enable `hasSameTypeConstructor[D, F[Any]]` effect detection below.
    implicit val anyType: Type[Any] = Type.of[Any]
    implicit val fAnyType: Type[F[Any]] = FCtor.apply[Any]

    val providers = dependencies.map(classify[F])

    buildResource[F, T](providers) match {
      case Right(expr)  => expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Classification (port of CatsProvidersGraphContext.buildGraph's input partitioning)
  // ---------------------------------------------------------------------------------------------------------------

  /** Classify one dependency by its static type. Order matters: `Resource[F, X]` first (it is also `F`-shaped under
    * `decompose2`), then effect `F[X]`, else a plain [[Instance]].
    */
  private def classify[F[_]](
      dep: Expr_??
  )(implicit fAnyType: Type[F[Any]], anyType: Type[Any]): Provider = {
    import dep.Underlying as D
    if (isResource[D]) {
      val x = resourceUnderlying[D]
      ResourceP(dep, x)
    } else if (isEffect[F, D]) {
      val x = effectUnderlying[F, D]
      EffectP(dep, x)
    } else {
      Instance(dep)
    }
  }

  /** `Resource[F, X]` ⇒ `true`. Mirrors macwire's `fullName.startsWith("cats.effect.kernel.Resource") && typeArgs == 2`. */
  private def isResource[D: Type]: Boolean =
    Type.fqcn[D] == "cats.effect.kernel.Resource" && Type.decompose2[D].isDefined

  /** The `X` of `Resource[F, X]` (the second type argument). */
  private def resourceUnderlying[D: Type]: ?? =
    Type.decompose2[D] match {
      case Some((_, (_, x))) => x
      case None              => Type[D].as_?? // unreachable — guarded by isResource
    }

  /** `F[X]` (an effect in our `F`) ⇒ `true`. Matches macwire's `Effect.isEffect`, but `F`-agnostic: instead of
    * `startsWith("cats.effect.IO")` we require the dependency to share `F`'s type constructor.
    */
  private def isEffect[F[_], D: Type](implicit fAnyType: Type[F[Any]], anyType: Type[Any]): Boolean =
    Type.decompose1[D].isDefined && Type.hasSameTypeConstructor[D, F[Any]]

  /** The `X` of `F[X]` (the sole type argument). */
  private def effectUnderlying[F[_], D: Type]: ?? =
    Type.decompose1[D] match {
      case Some((_, x)) => x
      case None         => Type[D].as_?? // unreachable — guarded by isEffect
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Resolution + construction
  // ---------------------------------------------------------------------------------------------------------------

  private def buildResource[F[_], T](providers: List[Provider])(implicit
      T: Type[T],
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Either[String, Expr[Resource[F, T]]] = {
    // The monadic providers (Resource/Effect) become nested flatMaps, innermost body = root `Resource.pure`.
    val monadic = providers.collect {
      case r: ResourceP => r
      case e: EffectP   => e
    }

    // Validate up-front that the root is constructible from the providers, so genuine resolution failures (missing /
    // ambiguous deps) surface as a clean Left rather than being thrown from inside a LambdaBuilder body. The bound vars
    // are not yet available here, so we only validate resolvability (which is bound-var-independent).
    constructRoot[T](providers, Map.empty[Provider, Expr_??], validateOnly = true) match {
      case Left(errors) => Left(errors)
      case Right(_)     =>
        Right(foldMonadic[F, T](monadic, providers, Map.empty[Provider, Expr_??]))
    }
  }

  /** Fold the monadic providers (Resource/Effect) into nested `flatMap`s, innermost body = `Resource.pure[F, T](root)`.
    *
    * `boundVars` accumulates, for each already-entered monadic provider, the `Expr[X]` of its flatMap lambda variable —
    * the back-reference used when the root construction needs that provider's value.
    */
  private def foldMonadic[F[_], T](
      remaining: List[Provider],
      allProviders: List[Provider],
      boundVars: Map[Provider, Expr_??]
  )(implicit
      T: Type[T],
      FCtor: Type.Ctor1[F],
      fAnyType: Type[F[Any]],
      anyType: Type[Any]
  ): Expr[Resource[F, T]] = {
    // Cross-quotes resolves `Resource[F, T]` from `FCtor` (the F type constructor) and `Type[T]` in scope. Needed as the
    // `To` type of the flatMap-lambda `buildWith` below.
    implicit val resourceFT: Type[Resource[F, T]] = Type.of[Resource[F, T]]
    remaining match {
      case Nil =>
        // Innermost: build T, wrap in Resource.pure[F, T](root). Resolvability already validated in buildResource.
        val root = constructRoot[T](allProviders, boundVars, validateOnly = false) match {
          case Right(expr)  => expr
          case Left(errors) => Environment.reportErrorAndAbort(errors)
        }
        Expr.quote { Resource.pure[F, T](Expr.splice(root)) }

      case (r: ResourceP) :: rest =>
        import r.x.Underlying as X
        val resourceExpr = r.value.value.asInstanceOf[Expr[Resource[F, X]]]
        val lambda: Expr[X => Resource[F, T]] =
          LambdaBuilder.of1[X]("dep").buildWith { (x: Expr[X]) =>
            foldMonadic[F, T](rest, allProviders, boundVars + (r -> x.as_??))
          }
        resourceFlatMap[F, X, T](resourceExpr, lambda)

      case (e: EffectP) :: rest =>
        import e.x.Underlying as X
        val effectExpr = e.value.value.asInstanceOf[Expr[F[X]]]
        val lambda: Expr[X => Resource[F, T]] =
          LambdaBuilder.of1[X]("dep").buildWith { (x: Expr[X]) =>
            foldMonadic[F, T](rest, allProviders, boundVars + (e -> x.as_??))
          }
        evalFlatMap[F, X, T](effectExpr, lambda)

      case _ :: rest =>
        // Instances are not flatMapped; they are spliced directly into the root construction. Skip here.
        foldMonadic[F, T](rest, allProviders, boundVars)
    }
  }

  /** `resource.flatMap(lambda)` — kept in a helper with a regular type parameter `X` so the reified quote never leaks a
    * path-dependent alias (`r.x.X`) into the call-site tree (a Scala 2 fresh-symbol scope error).
    */
  private def resourceFlatMap[F[_], X: Type, T](
      resource: Expr[Resource[F, X]],
      lambda: Expr[X => Resource[F, T]]
  )(implicit FCtor: Type.Ctor1[F], T: Type[T]): Expr[Resource[F, T]] =
    Expr.quote { Expr.splice(resource).flatMap(Expr.splice(lambda)) }

  /** `Resource.eval(effect).flatMap(lambda)` — same path-dependent-type isolation as [[resourceFlatMap]]. */
  private def evalFlatMap[F[_], X: Type, T](
      effect: Expr[F[X]],
      lambda: Expr[X => Resource[F, T]]
  )(implicit FCtor: Type.Ctor1[F], T: Type[T]): Expr[Resource[F, T]] =
    Expr.quote { Resource.eval(Expr.splice(effect)).flatMap(Expr.splice(lambda)) }

  /** Construct the root `T` via its primary constructor (or a single matching companion `apply`), resolving each
    * parameter against the providers. For monadic providers the resolved value is the bound flatMap-lambda var; for
    * instances it is the spliced expression itself.
    *
    * When `validateOnly` is true, monadic providers without a bound var still validate (we never read their value), so
    * resolution errors can be reported eagerly before any lambda is built.
    */
  private def constructRoot[T: Type](
      providers: List[Provider],
      boundVars: Map[Provider, Expr_??],
      validateOnly: Boolean
  ): Either[String, Expr[T]] =
    constructViaConstructor[T](providers, boundVars, validateOnly) match {
      case Right(expr)     => Right(expr)
      case Left(ctorError) =>
        constructViaCompanionApply[T](providers, boundVars, validateOnly) match {
          case Right(expr)      => Right(expr)
          case Left(applyError) => Left(s"$ctorError\n$applyError")
        }
    }

  private def constructViaConstructor[T: Type](
      providers: List[Provider],
      boundVars: Map[Provider, Expr_??],
      validateOnly: Boolean
  ): Either[String, Expr[T]] =
    Type[T].primaryConstructor.filter(_.isAvailable(AtCallSite)) match {
      case None       => Left(s"Cannot find a public constructor for ${Type.prettyPrint[T]}")
      case Some(ctor) =>
        DirectStyle[Either[String, *]].scoped { runSafe =>
          val arguments = resolveArguments(ctor, providers, boundVars, validateOnly, runSafe)
          upcastResult[T](runSafe(applyArguments(ctor, arguments)))
        }
    }

  private def constructViaCompanionApply[T: Type](
      providers: List[Provider],
      boundVars: Map[Provider, Expr_??],
      validateOnly: Boolean
  ): Either[String, Expr[T]] =
    Type.companionObject[T] match {
      case None                => Left(s"Companion object for ${Type.prettyPrint[T]} has no apply methods.")
      case Some(companionExpr) =>
        import companionExpr.{Underlying as Companion, value as companion}
        val applyMethods = Class[Companion].method("apply").collect {
          case m: Method.OnInstance if m.isAvailable(AtCallSite) && returnsSubtypeOf[T](m) => m
        }
        applyMethods match {
          case Nil           => Left(s"Companion object for ${Type.prettyPrint[T]} has no apply methods constructing it.")
          case single :: Nil =>
            DirectStyle[Either[String, *]].scoped { runSafe =>
              val arguments = resolveArguments(single, providers, boundVars, validateOnly, runSafe)
              val applied = single.apply(companion.asInstanceOf[Expr[single.Instance]])
              upcastResult[T](runSafe(applyArguments(applied, arguments)))
            }
          case _ =>
            Left(s"Multiple matching apply methods in the companion object of ${Type.prettyPrint[T]} were found.")
        }
    }

  /** Resolve every value parameter of `method` to a provider's value, aborting the surrounding direct-style scope with
    * the first failure (mirroring macwire's per-param resolution + its error messages).
    */
  private def resolveArguments(
      method: Method,
      providers: List[Provider],
      boundVars: Map[Provider, Expr_??],
      validateOnly: Boolean,
      runSafe: DirectStyle.RunSafe[Either[String, *]]
  ): Map[String, Expr_??] =
    method.totalParameters.flatten.toList.map { case (name, parameter) =>
      name -> runSafe(resolveParameter(providers, boundVars, validateOnly)(parameter))
    }.toMap

  /** Resolve a single parameter: implicit parameters go through implicit search; everything else is resolved to the
    * single provider whose `resultType <:< paramType` (macwire-parity errors on none / multiple).
    */
  private def resolveParameter(
      providers: List[Provider],
      boundVars: Map[Provider, Expr_??],
      validateOnly: Boolean
  )(parameter: Parameter): Either[String, Expr_??] =
    if (parameter.isImplicit) {
      import parameter.tpe.Underlying as P
      Expr.summonImplicit[P].toOption match {
        case Some(found) => Right(found.as_??)
        case None        => Left(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
      }
    } else {
      import parameter.tpe.Underlying as P
      matching(providers, parameter.tpe) match {
        case Nil           => Left(s"Cannot find a value of type: [${Type.prettyPrint[P]}]")
        case single :: Nil => Right(valueOf(single, boundVars, validateOnly))
        case multiple      =>
          Left(s"Ambiguous instances of types [${multiple.map(_.describe).mkString(", ")}] for [${Type.prettyPrint[P]}]")
      }
    }

  /** Providers whose `resultType` conforms to `parameterType` (excluding bottom types). */
  private def matching(providers: List[Provider], parameterType: ??): List[Provider] = {
    import parameterType.Underlying as P
    providers.filter { provider =>
      val rt = provider.resultType
      import rt.Underlying as R
      Type[R] <:< Type[P] && !(Type[R] =:= Type.of[Nothing]) && !(Type[R] =:= Type.of[Null])
    }
  }

  /** The value a resolved provider contributes to a construction: instances splice their own expression; monadic
    * providers contribute the bound flatMap-lambda variable. In `validateOnly` mode a monadic provider without a bound
    * var returns a harmless placeholder (never used to build a tree).
    */
  private def valueOf(provider: Provider, boundVars: Map[Provider, Expr_??], validateOnly: Boolean): Expr_?? =
    provider match {
      case i: Instance => i.value
      case monadic     =>
        boundVars.get(monadic) match {
          case Some(bound)               => bound
          case None if validateOnly      =>
            // Placeholder during validation: its type is the provider's resultType; never spliced into a real tree.
            val rt = monadic.resultType
            import rt.Underlying as R
            Expr.quote(null.asInstanceOf[R]).as_??
          case None                      =>
            Environment.reportErrorAndAbort(s"Internal error: no bound value for provider [${monadic.describe}]")
        }
    }

  private def upcastResult[T: Type](built: Expr_??): Expr[T] = {
    import built.Underlying
    built.value.upcast[T]
  }

  private def returnsSubtypeOf[T: Type](m: Method): Boolean = m.knownReturning.exists { rt =>
    import rt.Underlying as R
    Type[R] <:< Type[T]
  }

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
}
