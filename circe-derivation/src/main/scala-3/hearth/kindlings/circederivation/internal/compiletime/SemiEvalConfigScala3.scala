package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3

/** Hearth's `semiEval` follows immutable local `val`s (including the proxies Scala 3 binds the non-inline arguments of
  * an inline method to, also inside an argument and in `Inlined` bindings), unwraps named arguments and refuses `var`s
  * and members of classes compiled together with the macro call. One case is still refused here: the default argument
  * of another (inline) method (`KindlingsEncoder.derive[A](discriminator = Some("type"))` builds
  * `Configuration(derive$default$1[A], ...)`). Evaluating it broke the compilation of the enclosing class
  * (`CirceScala3Spec is already defined`), see `docs/research/hearth-gap-semieval-named-args.md`. And Hearth does not
  * follow lazy local vals (a local `given Configuration = ...`), which is done here.
  */
trait SemiEvalConfigScala3 extends SealedHierarchyFlattening { this: MacroCommonsScala3 =>

  override protected def semiEvalConfig(configExpr: Expr[Configuration]): Option[Configuration] = {
    import quotes.reflect.*
    // The references Hearth follows: immutable local vals, replaced by their right-hand sides (only to inspect the
    // result, which is never spliced).
    def followLocalVals(depth: Int): TreeMap = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case ident: Ident
            if depth > 0 && ident.symbol.isValDef && !ident.symbol.owner.isClassDef &&
              !ident.symbol.flags.is(Flags.Mutable) && !ident.symbol.flags.is(Flags.Lazy) &&
              !ident.symbol.flags.is(Flags.Param) =>
          scala.util.Try(ident.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs } match {
            case Some(rhs) => followLocalVals(depth - 1).transformTerm(rhs)(owner)
            case None      => ident
          }
        case _ => super.transformTerm(tree)(owner)
      }
    }
    val configurationModule = Symbol.requiredModule("hearth.kindlings.circederivation.Configuration")
    def callsOtherDefaultGetter(term: Term): Boolean =
      new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match {
            case ref: Ref
                if ref.symbol.name.contains("$default$") &&
                  ref.symbol.owner != configurationModule.moduleClass =>
              true
            case _ => foldOverTree(found, tree)(owner)
          })
      }.foldTree(false, term)(Symbol.spliceOwner)
    def evaluate(expr: Expr[Configuration]): Option[Configuration] = {
      val followed = scala.util.Try(followLocalVals(depth = 5).transformTerm(expr.asTerm)(Symbol.spliceOwner))
      if followed.toOption.forall(callsOtherDefaultGetter) then None
      else super.semiEvalConfig(expr)
    }
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _)      => unwrap(inner)
      case other                => other
    }
    // Hearth does not follow lazy local vals, and a local `given Configuration = Configuration(...)` is one: follow a
    // reference to an immutable local val (lazy included) to its right-hand side.
    def loop(expr: Expr[Configuration], depth: Int): Option[Configuration] =
      evaluate(expr).orElse {
        unwrap(expr.asTerm) match {
          case ident: Ident if depth > 0 && !ident.symbol.owner.isClassDef && !ident.symbol.flags.is(Flags.Mutable) =>
            scala.util.Try(ident.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs }.flatMap { rhs =>
              loop(rhs.asExpr.asInstanceOf[Expr[Configuration]], depth - 1)
            }
          case _ => None
        }
      }
    loop(configExpr, depth = 3)
  }
}
