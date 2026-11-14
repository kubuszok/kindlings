package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala3

/** Hearth's `semiEval` follows immutable local `val`s (including the proxies Scala 3 binds the non-inline arguments of
  * an inline method to), unwraps named arguments, and refuses `var`s and members of classes compiled together with the
  * macro call. It skips lazy local vals by design ("its right-hand side may refer to itself"), and a local
  * `given Configuration = Configuration(...)` is one: such a reference is followed to its right-hand side here.
  */
trait SemiEvalConfigScala3 extends SealedHierarchyFlattening { this: MacroCommonsScala3 =>

  override protected def semiEvalConfig(configExpr: Expr[Configuration]): Option[Configuration] = {
    import quotes.reflect.*
    def unwrap(t: Term): Term = t match {
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _)      => unwrap(inner)
      case other                => other
    }
    def loop(expr: Expr[Configuration], depth: Int): Option[Configuration] =
      super.semiEvalConfig(expr).orElse {
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
