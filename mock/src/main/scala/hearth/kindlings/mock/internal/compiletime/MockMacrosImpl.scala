package hearth.kindlings.mock
package internal.compiletime

import hearth.MacroCommons

/** Shared, macro-platform-agnostic implementation of the mocking macros.
  *
  * Uses Hearth's [[hearth.typed.Classes.AnonymousInstance]] to synthesize an anonymous subtype of the mocked type whose
  * every abstract member forwards into the runtime [[MockContext]] supplied at the call site.
  */
private[mock] trait MockMacrosImpl extends hearth.kindlings.macros.compiletime.GenerationLogging { this: MacroCommons =>

  protected val generationLoggingNamespace: String = "mock"
  protected def generationMarkerImported: Boolean = {
    implicit val tpe: Type[Mock.LogGeneration] = Type.of[Mock.LogGeneration]
    Expr.summonImplicit[Mock.LogGeneration].isDefined
  }

  def mockType[A: Type](ctx: Expr[MockContext]): Expr[A] =
    synthesizeMock[A](kind = "mock", ctx, (c, cm) => forwardingBody(c, cm))

  private def synthesizeMock[A: Type](
      kind: String,
      ctx: Expr[MockContext],
      bodyFor: (Expr[MockContext], ClassifiedMethod) => OverrideBody
  ): Expr[A] = {
    val trace = new Trace
    trace.scope(s"$kind[${Type.prettyPrint[A]}]") {
      AnonymousInstance.parse[A] match {
        case ClassViewResult.Incompatible(reason) =>
          Environment.reportErrorAndAbort(s"Cannot $kind ${Type.prettyPrint[A]}: $reason")
        case ClassViewResult.Compatible(ai) =>
          val members = ai.mustOverride
          trace.step(s"overriding ${members.size} member(s): ${members.map(_.method.name).mkString(", ")}")
          val overrides: Map[UntypedMethod, OverrideBody] =
            members.map(cm => cm.method.asUntyped -> bodyFor(ctx, cm)).toMap
          val (ctor, ctorArgs) = constructorAndArgs(ai.classParent)
          trace.step(ctor match {
            case Some(_) => s"invoking parent constructor with ${ctorArgs.size} seeded argument(s)"
            case None    => "trait parent — no constructor to invoke"
          })
          ai.construct(ctor, ctorArgs, overrides) match {
            case Right(instance) =>
              emitGenerationLog(s"Mock.$kind", trace, instance.prettyPrint)
              instance
            case Left(errors) =>
              Environment.reportErrorAndAbort(s"Cannot $kind ${Type.prettyPrint[A]}: ${errors.toVector.mkString("; ")}")
          }
      }
    }
  }

  /** `Mock.stub[A]`: like [[mockType]] but each abstract member forwards into [[MockContext.handleStub]] so unexpected
    * calls return a default (from a summoned [[Defaultable]], or `null`) instead of failing, and behaviour is preset
    * lazily via `when(...)` and checked post-hoc via `verify(...)`.
    */
  def stubType[A: Type](ctx: Expr[MockContext]): Expr[A] =
    synthesizeMock[A](kind = "stub", ctx, (c, cm) => stubBody(c, cm))

  /** When mocking a CLASS (not a trait), Hearth's `AnonymousInstance` exposes the parent's accessible constructors in
    * `classParent`; the synthesized subtype must invoke one. We pick the primary (first) constructor and supply a dummy
    * value for each of its parameters — a summoned `Defaultable` (or `null`), mirroring ScalaMock's
    * `Defaultable`-seeded `new T(...)`. The real superclass constructor IS run with these dummies (so ctor side effects
    * fire, as in ScalaMock). Trait parents have no constructor, so this yields `(None, empty)` and the original
    * behaviour.
    */
  private def constructorAndArgs(classParent: Option[(??, List[Method])]): (Option[Method], Map[String, Expr_??]) =
    classParent.flatMap(_._2.headOption) match {
      case Some(ctor) =>
        val args = ctor.totalParameters.flatten.toList.map { case (name, parameter) =>
          import parameter.tpe.Underlying as P
          name -> summonDefault[P].as_??
        }.toMap
        (Some(ctor), args)
      case None => (None, Map.empty)
    }

  /** Body of one stubbed method: forward to `ctx.handleStub(name, args, default)`, where `default` is the value of a
    * summoned `Defaultable[R]` (or `null` when none is available).
    */
  private def stubBody(ctx: Expr[MockContext], cm: ClassifiedMethod): OverrideBody = new OverrideBody {
    def apply(octx: OverrideContext): Expr_?? = {
      import octx.returnType.Underlying as R
      val methodName: Expr[String] = Expr(cm.method.name)
      val args: Expr[Vector[Any]] = octx.parameters.foldRight(Expr.quote(Vector.empty[Any]))(prependArg)
      stubCall[R](ctx, methodName, args, summonDefault[R]).as_??
    }
  }

  /** The default value for `R`: a summoned `Defaultable[R]` if one exists, otherwise `null` cast to `R`. */
  private def summonDefault[R: Type]: Expr[R] = {
    implicit val defaultableType: Type[Defaultable[R]] = Type.of[Defaultable[R]]
    Expr.summonImplicit[Defaultable[R]].toOption match {
      case Some(d) => Expr.quote(Expr.splice(d).default)
      case None    => Expr.quote(null.asInstanceOf[R])
    }
  }

  private def stubCall[R: Type](
      ctx: Expr[MockContext],
      methodName: Expr[String],
      args: Expr[Vector[Any]],
      default: Expr[R]
  ): Expr[R] =
    Expr.quote(
      Expr.splice(ctx).handleStub(Expr.splice(methodName), Expr.splice(args), Expr.splice(default)).asInstanceOf[R]
    )

  /** Faithful `(m.method _).expects(args...)` DSL. Given the eta-expanded method reference `f` and the expected
    * arguments as a `Seq[Any]`, register an expectation for that method on the [[MockContext]] that is implicitly in
    * scope at the call site (the same context the mock was created with). Mirrors ScalaMock's `expects`, but routes
    * into our name-keyed runtime rather than per-method `MockFunction` fields.
    */
  def expectsType(f: Expr[Any], args: Expr[Seq[Any]]): Expr[CallHandler] =
    withMockContext[CallHandler]("expects") { ctx =>
      val (name, arity) = extractMethodRef(f)
      Expr.quote(Expr.splice(ctx).expectingArity(Expr.splice(Expr(name)), Expr.splice(Expr(arity)), Expr.splice(args)))
    }

  /** Faithful `(s.method _).when(args...)` DSL — preset a stub's behaviour for the referenced method. */
  def whenType(f: Expr[Any], args: Expr[Seq[Any]]): Expr[CallHandler] =
    withMockContext[CallHandler]("when") { ctx =>
      val (name, arity) = extractMethodRef(f)
      Expr.quote(Expr.splice(ctx).whenArity(Expr.splice(Expr(name)), Expr.splice(Expr(arity)), Expr.splice(args)))
    }

  /** Faithful `(s.method _).verify(args...)` DSL — post-hoc assert how often the referenced method was called. */
  def verifyType(f: Expr[Any], args: Expr[Seq[Any]]): Expr[VerifyTarget] =
    withMockContext[VerifyTarget]("verify") { ctx =>
      val (name, _) = extractMethodRef(f)
      Expr.quote(Expr.splice(ctx).verifySeq(Expr.splice(Expr(name)), Expr.splice(args)))
    }

  /** Summon the implicit [[MockContext]] in scope at the call site and hand it to `build`; aborts with a helpful
    * message if none is found. Shared by the faithful `expects`/`when`/`verify` DSL macros.
    */
  private def withMockContext[R](dslName: String)(build: Expr[MockContext] => Expr[R]): Expr[R] = {
    implicit val ctxType: Type[MockContext] = Type.of[MockContext]
    Expr.summonImplicit[MockContext].toOption match {
      case Some(ctx) => build(ctx)
      case None      =>
        Environment.reportErrorAndAbort(
          s"No implicit MockContext is in scope for `.$dslName`. Create the mock/stub with an implicit MockContext " +
            "(e.g. `implicit val ctx: MockContext = new MockContext`) and call it in that scope."
        )
    }
  }

  /** Extract the referenced method's name from an eta-expanded reference `m.method _` (a lambda whose body is the call,
    * or the call itself). The outermost [[hearth.typed.Exprs.DestructuredExpr.MethodCall]] in the parsed tree is the
    * referenced method.
    */
  private def extractMethodRef(f: Expr[Any]): (String, Int) = {
    implicit val anyType: Type[Any] = Type.of[Any]
    val parsed = DestructuredExpr.parse[Any](f)
    parsed.collect { case mc: DestructuredExpr.MethodCall => mc }.headOption match {
      case Some(mc) => (mc.method.name, mc.method.arity)
      case None     =>
        Environment.reportErrorAndAbort(
          "`.expects` must be applied to a method reference of the form `(m.method _)`; " +
            s"could not find a method call in ${parsed.plainPrint}."
        )
    }
  }

  /** Build the body of one overridden method: pack the arguments into a `Seq[Any]` and forward to
    * `ctx.handle(methodName, args)`, casting the (erased) result back to the method's return type.
    */
  private def forwardingBody(ctx: Expr[MockContext], cm: ClassifiedMethod): OverrideBody = new OverrideBody {
    def apply(octx: OverrideContext): Expr_?? = {
      import octx.returnType.Underlying as R
      val methodName: Expr[String] = Expr(cm.method.name)

      val args: Expr[Vector[Any]] =
        octx.parameters.foldRight(Expr.quote(Vector.empty[Any]))(prependArg)

      // A `this.type` (fluent) method cannot have a value synthesized for it — the only inhabitant of the synthesized
      // subtype's `this.type` is `this`. `octx.returnsThisType` (Hearth 0.3.1-49) is the signal `returnType` can't give
      // (it widens to the parent). So we still route through the context (recording + strict expectation match) but
      // return `octx.self` (the mock itself), WITHOUT casting it to the widened `returnType`.
      if (octx.returnsThisType) selfReturnBody(ctx, methodName, args, octx.self)
      else forwardCall[R](ctx, methodName, args, summonDefault[R]).as_??
    }
  }

  /** Body for a `this.type` return: record the call (so strict expectations still match/verify), then return `this`.
    * `self` is already typed as the synthesized subtype's `this.type`, so it is returned as-is (no cast).
    */
  private def selfReturnBody(
      ctx: Expr[MockContext],
      methodName: Expr[String],
      args: Expr[Vector[Any]],
      self: Expr_??
  ): Expr_?? = {
    import self.Underlying as S
    selfReturnBodyImpl[S](ctx, methodName, args, self.value).as_??
  }
  private def selfReturnBodyImpl[S: Type](
      ctx: Expr[MockContext],
      methodName: Expr[String],
      args: Expr[Vector[Any]],
      self: Expr[S]
  ): Expr[S] =
    Expr.quote {
      val _ = Expr.splice(ctx).handle(Expr.splice(methodName), Expr.splice(args), ())
      Expr.splice(self)
    }

  /** Prepend one argument expression to the accumulated `Vector[Any]`. A helper with a regular type parameter so the
    * spliced argument's (path-dependent) type does not leak into the generated tree on Scala 2. The spliced reference
    * forces a by-name parameter to its value (it is bound to an `Any` local before use).
    */
  private def prependArg(parameter: Expr_??, acc: Expr[Vector[Any]]): Expr[Vector[Any]] = {
    import parameter.Underlying as P
    prepend[P](parameter.value, acc)
  }
  private def prepend[P: Type](parameter: Expr[P], acc: Expr[Vector[Any]]): Expr[Vector[Any]] =
    // Bind to `Any` first: this forces a by-name parameter to its value exactly once (so the runtime never receives a
    // `Function0` thunk that would `ClassCastException` on Scala 3), and ascribing to `Any` — rather than the param's
    // own (possibly singleton/path-dependent) type — keeps the generated tree valid on both platforms.
    Expr.quote {
      val forced: Any = Expr.splice(parameter)
      forced +: Expr.splice(acc)
    }

  /** Build `ctx.handle(name, args).asInstanceOf[R]`. A helper with a regular type parameter `R` so the path-dependent
    * return type (`overrideContext.returnType.Underlying`) does not leak into the generated tree on Scala 2 (see the
    * cross-compilation pitfalls skill).
    */
  private def forwardCall[R: Type](
      ctx: Expr[MockContext],
      methodName: Expr[String],
      args: Expr[Vector[Any]],
      default: Expr[R]
  ): Expr[R] =
    Expr.quote(
      Expr.splice(ctx).handle(Expr.splice(methodName), Expr.splice(args), Expr.splice(default)).asInstanceOf[R]
    )
}
