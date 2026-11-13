package hearth.kindlings.circederivation
package internal.compiletime

import hearth.MacroCommonsScala2

/** `semiEval` reads `var`s by reflection, from classes compiled earlier (e.g. by a previous incremental run): the value
  * it sees is not the runtime value. Configurations referencing a `var` are only known at runtime. Same as
  * jsoniter-derivation's `CodecMacros.semiEvalConfig`, see `docs/research/hearth-gap-semieval-named-args.md`.
  */
trait SemiEvalConfigScala2 extends SealedHierarchyFlattening { this: MacroCommonsScala2 =>

  override protected def semiEvalConfig(configExpr: Expr[Configuration]): Option[Configuration] = {
    import c.universe.*
    val readsVar = configExpr.tree.exists { t =>
      val sym = t.symbol
      sym != null && sym != NoSymbol && sym.isTerm && {
        val term = sym.asTerm
        term.isVar || (term.isMethod && term.asMethod.isGetter && term.asMethod.setter != NoSymbol)
      }
    }
    if (readsVar) None else super.semiEvalConfig(configExpr)
  }
}
