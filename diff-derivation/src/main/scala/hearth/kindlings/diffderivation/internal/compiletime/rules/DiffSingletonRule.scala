package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation._

trait DiffSingletonRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  object DiffSingletonRule extends DiffDerivationRule("Diff as singleton") {
    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
          val sn = Expr(Type.shortName[A])
          val pn = Expr(Type.prettyPrint[A])
          val fn = Expr(Type.plainPrint[A])
          MIO.pure(Rule.matched(Expr.quote {
            DiffResult.Identical(Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn),
              Expr.splice(sn))
          }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
