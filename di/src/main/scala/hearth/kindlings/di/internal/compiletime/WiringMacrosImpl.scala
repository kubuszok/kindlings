package hearth.kindlings.di
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.DirectStyle

/** Shared, macro-platform-agnostic implementation of the wiring macros.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `Method`, `enclosingScope`, ...).
  */
private[di] trait WiringMacrosImpl { this: MacroCommons =>

  /** macwire-style `wire[A]`: construct an `A` by resolving each parameter of its primary constructor (or, failing
    * that, of a single matching companion `apply`) from a value of a compatible type found in the enclosing lexical
    * scope. Implicit parameters are resolved by implicit search rather than from scope.
    */
  def wire[A: Type]: Expr[A] = {
    val scopes = collectScopes
    buildInstance[A](scopes, recursive = false) match {
      case Right(expr)  => expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  /** macwire-style `wireRec[A]`: like [[wire]], but any constructor/apply parameter that cannot be found in the
    * enclosing scope is itself constructed recursively (instead of failing), as long as it is "wireable" — i.e. not a
    * `java.*`/`scala.*` library type, mirroring macwire's `isWireable` guard.
    */
  def wireRec[A: Type]: Expr[A] = {
    val scopes = collectScopes
    buildInstance[A](scopes, recursive = true) match {
      case Right(expr)  => expr
      case Left(errors) => Environment.reportErrorAndAbort(errors)
    }
  }

  /** macwire-style `wireWith[RES](factory)`: resolve each parameter of the supplied factory function from the enclosing
    * scope, then apply the factory to the resolved values. The factory is any `FunctionN` value (a lambda or an
    * eta-expanded method/constructor reference); implicit parameters of the underlying method are assumed already
    * applied at the call site, exactly as in macwire.
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
          val arguments = resolveArguments(applied, scopes, recursive = false, runSafe)
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

  private def buildSet[A: Type](elements: List[Expr_??]): Expr[Set[A]] =
    elements.foldRight(Expr.quote(Set.empty[A]))((element, acc) => consSet[A](upcastTo[A](element), acc))
  private def consSet[A: Type](element: Expr[A], acc: Expr[Set[A]]): Expr[Set[A]] =
    Expr.quote(Expr.splice(acc) + Expr.splice(element))

  private def buildList[A: Type](elements: List[Expr_??]): Expr[List[A]] =
    elements.foldRight(Expr.quote(List.empty[A]))((element, acc) => consList[A](upcastTo[A](element), acc))
  private def consList[A: Type](element: Expr[A], acc: Expr[List[A]]): Expr[List[A]] =
    Expr.quote(Expr.splice(element) :: Expr.splice(acc))

  private def upcastTo[A: Type](candidate: Expr_??): Expr[A] = {
    import candidate.Underlying
    candidate.value.upcast[A]
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
  protected def collectScopes: List[List[Expr_??]] =
    enclosingScope.iterator.flatMap {
      case enc: Enclosure.Class  => scopesForMembers(enc.thisRef, enc.members)
      case enc: Enclosure.Object => scopesForMembers(Some(enc.thisRef), enc.members)
      case enc: Enclosure.Method => List(enc.localValues.map(_.ref))
      case _: Enclosure.Value    => Nil
      case _: Enclosure.Package  => Nil
    }.toList.filter(_.nonEmpty)

  /** Direct members of an enclosing instance form one scope; the public parameterless members of any `@Module`-typed
    * value among them form an immediately-wider scope.
    */
  private def scopesForMembers(self: Option[Expr_??], members: List[Method]): List[List[Expr_??]] =
    self.toList.flatMap { s =>
      val direct = membersAsValues(s, members)
      val moduleExpanded = direct.flatMap(expandIfModule)
      if (moduleExpanded.isEmpty) List(direct) else List(direct, moduleExpanded)
    }

  /** If `value`'s type is annotated with [[Module]] (or inherits such a type), expose its public parameterless members
    * as candidate values read off `value`.
    */
  private def expandIfModule(value: Expr_??): List[Expr_??] = {
    import value.Underlying as T
    if (isModuleType[T]) membersAsValues(value, Class[T].methods) else Nil
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

  /** Turn the accessible nullary members of an enclosing instance into candidate value expressions. */
  private def membersAsValues(self: Expr_??, members: List[Method]): List[Expr_??] = {
    import self.value as selfExpr
    members.collect {
      case m: Method.OnInstance if m.isNullary && m.knownReturning.isDefined && m.isAvailable(AtCallSite) =>
        m.apply(selfExpr.asInstanceOf[Expr[m.Instance]]) match {
          case r: Method.Result[?] =>
            import r.Returned
            r.build().toOption.map(_.as_??)
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
  protected def resolveParameter(scopes: List[List[Expr_??]], recursive: Boolean)(
      parameter: Parameter
  ): Either[String, Expr_??] =
    if (parameter.isImplicit) {
      import parameter.tpe.Underlying as P
      Expr.summonImplicit[P].toOption match {
        case Some(found) => Right(found.as_??)
        case None        => Left(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
      }
    } else if (recursive && matchingCandidates(scopes, parameter.tpe).isEmpty && isWireable(parameter.tpe)) {
      // Nothing of this type is in scope; in recursive mode, build it from scratch.
      import parameter.tpe.Underlying as P
      buildInstance[P](scopes, recursive = true).map(_.as_??)
    } else resolveSingle(scopes)(parameter.tpe)

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
  protected def resolveSingle(scopes: List[List[Expr_??]])(parameterType: ??): Either[String, Expr_??] = {
    import parameterType.Underlying as P
    def search(remaining: List[List[Expr_??]]): Either[String, Expr_??] = remaining match {
      case Nil           => Left(s"Cannot find a value of type: [${Type.prettyPrint[P]}]")
      case scope :: rest =>
        matchingIn(scope, parameterType) match {
          case Nil           => search(rest)
          case single :: Nil => Right(single)
          case multiple      =>
            Left(s"Found multiple values of type [${Type.prettyPrint[P]}]: [${multiple.map(describe).mkString(", ")}]")
        }
    }
    search(scopes)
  }

  /** Every candidate of every scope whose type conforms to `parameterType` (flattened, innermost first) — used by
    * set/list wiring.
    */
  protected def matchingCandidates(scopes: List[List[Expr_??]], parameterType: ??): List[Expr_??] =
    scopes.flatMap(matchingIn(_, parameterType))

  /** Candidates within a single scope whose type conforms to `parameterType` (excluding the bottom types
    * `Nothing`/`Null`).
    */
  private def matchingIn(scope: List[Expr_??], parameterType: ??): List[Expr_??] = {
    import parameterType.Underlying as P
    scope.filter { c =>
      import c.Underlying as C
      Type[C] <:< Type[P] && !(Type[C] =:= Type.of[Nothing]) && !(Type[C] =:= Type.of[Null])
    }
  }

  protected def describe(candidate: Expr_??): String = {
    import candidate.Underlying as C
    Type.prettyPrint[C]
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Construction
  // ---------------------------------------------------------------------------------------------------------------

  /** Build an `A`: prefer its accessible primary constructor; otherwise fall back to a single matching companion
    * `apply`. Errors from both attempts are combined to explain why wiring failed.
    */
  private def buildInstance[A: Type](scopes: List[List[Expr_??]], recursive: Boolean): Either[String, Expr[A]] =
    constructViaConstructor[A](scopes, recursive) match {
      case Right(expr)        => Right(expr)
      case Left(ctorError)    =>
        constructViaCompanionApply[A](scopes, recursive) match {
          case Right(expr)       => Right(expr)
          case Left(applyError)  => Left(s"$ctorError\n$applyError")
        }
    }

  private def constructViaConstructor[A: Type](scopes: List[List[Expr_??]], recursive: Boolean): Either[String, Expr[A]] =
    Type[A].primaryConstructor.filter(_.isAvailable(AtCallSite)) match {
      case None       => Left(s"Cannot find a public constructor for ${Type.prettyPrint[A]}")
      case Some(ctor) =>
        DirectStyle[Either[String, *]].scoped { runSafe =>
          val arguments = resolveArguments(ctor, scopes, recursive, runSafe)
          upcastResult[A](runSafe(applyArguments(ctor, arguments)))
        }
    }

  private def constructViaCompanionApply[A: Type](scopes: List[List[Expr_??]], recursive: Boolean): Either[String, Expr[A]] =
    Type.companionObject[A] match {
      case None                => Left(s"Companion object for ${Type.prettyPrint[A]} has no apply methods.")
      case Some(companionExpr) =>
        import companionExpr.{Underlying as Companion, value as companion}
        val applyMethods = Class[Companion].method("apply").collect {
          case m: Method.OnInstance if m.isAvailable(AtCallSite) && returnsSubtypeOf[A](m) => m
        }
        applyMethods match {
          case Nil           => Left(s"Companion object for ${Type.prettyPrint[A]} has no apply methods constructing it.")
          case single :: Nil =>
            DirectStyle[Either[String, *]].scoped { runSafe =>
              val arguments = resolveArguments(single, scopes, recursive, runSafe)
              val applied = single.apply(companion.asInstanceOf[Expr[single.Instance]])
              upcastResult[A](runSafe(applyArguments(applied, arguments)))
            }
          case _ =>
            Left(s"Multiple matching apply methods in the companion object of ${Type.prettyPrint[A]} were found.")
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

  /** Resolve every value parameter of `method` (implicit lookups included), aborting the surrounding direct-style
    * scope with the first failure.
    */
  private def resolveArguments(
      method: Method,
      scopes: List[List[Expr_??]],
      recursive: Boolean,
      runSafe: DirectStyle.RunSafe[Either[String, *]]
  ): Map[String, Expr_??] =
    method.totalParameters.flatten.toList.map { case (name, parameter) =>
      name -> runSafe(resolveParameter(scopes, recursive)(parameter))
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

    val block: Expr[A] = DirectStyle[ValDefs].scoped { runSafe =>
      val memo = scala.collection.mutable.LinkedHashMap.empty[String, Expr_??]

      def valNode[T: Type](built: Expr_??): Expr_?? = {
        import built.Underlying
        runSafe(ValDefs.createVal[T](built.value.upcast[T])).as_??
      }

      def buildViaMethod(t: ??, method: Method, breadcrumb: List[??]): Expr_?? = {
        val arguments: Map[String, Expr_??] = method.totalParameters.flatten.toList.map { case (name, parameter) =>
          val ref =
            if (parameter.isImplicit) summonOrAbort(parameter.tpe)
            else expand(parameter.tpe, breadcrumb :+ t)
          name -> ref
        }.toMap
        applyArguments(method, arguments) match {
          case Right(built) => built
          case Left(err)    => Environment.reportErrorAndAbort(err)
        }
      }

      def expand(t: ??, breadcrumb: List[??]): Expr_?? = {
        import t.Underlying as T
        memo.getOrElseUpdate(
          Type.fqcn[T], {
            verifyNotCyclic(t, breadcrumb)
            verifyNotPrimitive(t)
            findInstanceProvider(t, rawDeps) match {
              case Some((dep, i)) =>
                used += i
                import dep.Underlying
                dep.value.upcast[T].as_??
              case None =>
                findFactoryProvider(t, rawDeps) match {
                  case Some((dep, i, applyM)) =>
                    used += i
                    val applied = applyM.apply(dep.value.asInstanceOf[Expr[applyM.Instance]])
                    valNode[T](buildViaMethod(t, applied, breadcrumb))
                  case None =>
                    providerCallable[T] match {
                      case Some(method) => valNode[T](buildViaMethod(t, method, breadcrumb))
                      case None         =>
                        Environment.reportErrorAndAbort(
                          s"cannot find a provided dependency, public constructor or public apply method for: ${Type
                              .prettyPrint[T]}"
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
    block
  }

  /** Recover the precise static type of a single provided dependency expression. Both platforms hand the macro the
    * individual argument expressions (Scala 2 directly as `c.Expr[Any]*`, Scala 3 via `scala.quoted.Varargs`); their
    * static `Any` type is widened back to the real type through [[DestructuredExpr]].
    */
  def preciseExpr(e: Expr[Any]): Expr_?? = {
    implicit val anyType: Type[Any] = Type.of[Any]
    toTypedExpr(DestructuredExpr.parse(e))
  }

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
    new Class[D]().method("apply").collect {
      case m: Method.OnInstance if m.isAvailable(AtCallSite) => m
    }.sortBy(m => -m.totalParameters.flatten.size).headOption
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
    rawDeps.zipWithIndex.iterator.flatMap { case (dep, i) =>
      if (isFunctionExpr(dep)) factoryApply(dep).flatMap { applyM =>
        applyM.knownReturning.collect {
          case rt if { import rt.Underlying as R; Type[R] <:< Type[T] } => (dep, i, applyM)
        }
      }
      else None
    }.nextOption()
  }

  /** A callable [[Method]] producing `T`: its public primary constructor, or a single matching companion `apply`
    * (already applied to the companion module).
    */
  private def providerCallable[T: Type]: Option[Method] =
    Type[T].primaryConstructor.filter(_.isAvailable(AtCallSite)).orElse(companionAppliedMethod[T])

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
      case None        => Environment.reportErrorAndAbort(s"Cannot find an implicit value of type: [${Type.prettyPrint[P]}]")
    }
  }

  private def checkDuplicateDependencyTypes(rawDeps: List[Expr_??]): Unit = {
    val seen = scala.collection.mutable.ListBuffer.empty[??]
    rawDeps.foreach { dep =>
      import dep.Underlying as D
      val clash = seen.exists { s =>
        import s.Underlying as S
        Type[S] <:< Type[D] || Type[D] <:< Type[S]
      }
      if (clash) Environment.reportErrorAndAbort(s"duplicate type in dependencies list: ${Type.prettyPrint[D]}")
      val _ = seen += Type[D].as_??
    }
  }

  private def verifyNotCyclic(t: ??, breadcrumb: List[??]): Unit = {
    import t.Underlying as T
    if (breadcrumb.exists { b => import b.Underlying as B; Type[B] <:< Type[T] })
      Environment.reportErrorAndAbort(s"cyclic dependencies detected while wiring ${Type.prettyPrint[T]}")
  }

  private def verifyNotPrimitive(t: ??): Unit = {
    import t.Underlying as T
    val isPrimitive =
      Type[T] =:= Type.of[Int] || Type[T] =:= Type.of[Long] || Type[T] =:= Type.of[Byte] ||
        Type[T] =:= Type.of[Short] || Type[T] =:= Type.of[Char] || Type[T] =:= Type.of[Boolean] ||
        Type[T] =:= Type.of[Double] || Type[T] =:= Type.of[Float] || Type[T] =:= Type.of[String]
    if (isPrimitive)
      Environment.reportErrorAndAbort(s"cannot use a primitive type or String in autowiring: ${Type.prettyPrint[T]}")
  }

  private def verifyAllDependenciesUsed(rawDeps: List[Expr_??], used: Set[Int]): Unit = {
    val unused = rawDeps.zipWithIndex.collect { case (dep, i) if !used.contains(i) => describe(dep) }
    if (unused.nonEmpty) Environment.reportErrorAndAbort(s"unused dependencies: ${unused.mkString(", ")}")
  }
}
