package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3

/** Hearth's `semiEval` cannot resolve references to local `val`s, and Scala 3 binds every non-inline argument of an
  * inline method (`derived[A](using Configuration.default.withDefaults)`) to such a local proxy `val`: without this
  * only stable module references (e.g. `Configuration.default`) would be evaluated at compile time. Follow immutable
  * local `val`s (never `var`s) to their right-hand side. Same as jsoniter-derivation's `CodecMacros.semiEvalConfig`,
  * see `docs/research/hearth-gap-semieval-named-args.md`.
  */
trait SemiEvalConfigScala3 extends SealedHierarchyFlattening { this: MacroCommonsScala3 =>

  override protected def semiEvalConfig(configExpr: Expr[Configuration]): Option[Configuration] = {
    import quotes.reflect.*
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _)      => unwrap(inner)
      case other                => other
    }
    // `semiEval` reads `var`s by reflection, from classes compiled earlier (e.g. by a previous incremental run): the
    // value it sees is not the runtime value. Configurations referencing a `var` are only known at runtime.
    def readsVar(term: Term): Boolean =
      new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match {
            case ref: Ref if ref.symbol.flags.is(Flags.Mutable) => true
            case _                                              => foldOverTree(found, tree)(owner)
          })
      }.foldTree(false, term)(Symbol.spliceOwner)
    // `semiEval` does not look through named arguments (`Configuration(useDefaults = true)` keeps `NamedArg` wrappers
    // around the arguments, already in positional order): strip them. See docs/research/hearth-gap-semieval-named-args.md.
    val stripNamedArgs: TreeMap = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case NamedArg(_, arg) => transformTerm(arg)(owner)
        case _                => super.transformTerm(tree)(owner)
      }
    }
    // The arguments of a configuration built inside an inline method (`KindlingsEncoder.deriveEnum(renaming.kebabCase)`
    // builds `Configuration(transformConstructorNames = <proxy>, ...)`) are references to the local proxy `val`s Scala 3
    // binds the inline method's arguments to: replace such references to (immutable) proxy `val`s by their
    // right-hand sides. The result is only evaluated, never spliced: a block of such `val`s is replaced by its result
    // expression, and other blocks (e.g. lambdas) are kept as they are (copying them would check their owners).
    def inlineLocalVals(depth: Int): TreeMap = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match {
        case Block(stats, expr) if stats.forall { case _: ValDef => true; case _ => false } =>
          transformTerm(expr)(owner)
        case _: Block => tree
        case ident: Ident
            if depth > 0 && ident.symbol.isValDef && ident.symbol.name.contains("$proxy") &&
              !ident.symbol.owner.isClassDef &&
              !ident.symbol.flags.is(Flags.Mutable) && !ident.symbol.flags.is(Flags.Lazy) =>
          scala.util.Try(ident.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs } match {
            case Some(rhs) => inlineLocalVals(depth - 1).transformTerm(rhs)(owner)
            case None      => ident
          }
        case _ => super.transformTerm(tree)(owner)
      }
    }
    // Lambdas (or other blocks) of the call site, and the default arguments of other (inline) methods, are not evaluated
    // (evaluating the latter breaks the compilation of the enclosing class).
    val configurationModule = Symbol.requiredModule("hearth.kindlings.circederivation.Configuration")
    def notEvaluable(term: Term): Boolean =
      new TreeAccumulator[Boolean] {
        def foldTree(found: Boolean, tree: Tree)(owner: Symbol): Boolean =
          found || (tree match {
            case _: Block => true
            case ref: Ref
                if ref.symbol.name.contains("$default$") &&
                  ref.symbol.owner != configurationModule.moduleClass =>
              true
            case _ => foldOverTree(found, tree)(owner)
          })
      }.foldTree(false, term)(Symbol.spliceOwner)
    def evaluate(expr: Expr[Configuration]): Option[Configuration] =
      super
        .semiEvalConfig(expr)
        .orElse {
          val stripped = stripNamedArgs.transformTerm(expr.asTerm)(Symbol.spliceOwner)
          if stripped eq expr.asTerm then None
          else super.semiEvalConfig(stripped.asExpr.asInstanceOf[Expr[Configuration]])
        }
        .orElse {
          scala.util
            .Try {
              val inlined = inlineLocalVals(depth = 3).transformTerm(unwrap(expr.asTerm))(Symbol.spliceOwner)
              val stripped = stripNamedArgs.transformTerm(inlined)(Symbol.spliceOwner)
              if readsVar(stripped) || notEvaluable(stripped) then None
              else super.semiEvalConfig(stripped.asExpr.asInstanceOf[Expr[Configuration]])
            }
            .toOption
            .flatten
        }
    def loop(expr: Expr[Configuration], depth: Int): Option[Configuration] =
      (if readsVar(expr.asTerm) then None else evaluate(expr)).orElse {
        unwrap(expr.asTerm) match {
          case ident: Ident if depth > 0 && !ident.symbol.flags.is(Flags.Mutable) =>
            scala.util.Try(ident.symbol.tree).toOption.collect { case ValDef(_, _, Some(rhs)) => rhs }.flatMap { rhs =>
              loop(rhs.asExpr.asInstanceOf[Expr[Configuration]], depth - 1)
            }
          case _ => None
        }
      }
    loop(configExpr, depth = 3)
  }
}
