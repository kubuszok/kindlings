package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*
import hearth.kindlings.diffderivation.internal.runtime.*

trait DiffBuiltInRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused")
  object DiffBuiltInRule extends DiffDerivationRule("built-in Diff for primitives/String") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = MIO {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val ST: Type[String] = DiffTypes.StringType
      val pn = Expr(Type.prettyPrint[A])
      val fn = Expr(Type.plainPrint[A])
      val sn = Expr(Type.shortName[A])

      if (Type[A] <:< DiffTypes.StringType) {
        Rule.matched(Expr.quote {
          DiffRuntime.diffString(
            Expr.splice(pn),
            Expr.splice(fn),
            Expr.splice(sn),
            Expr.splice(sn),
            Expr.splice(dctx.left.upcast[String]),
            Expr.splice(dctx.right.upcast[String])
          )
        })
      } else if (
        Type[A] <:< DiffTypes.BooleanType || Type[A] <:< DiffTypes.ByteType ||
        Type[A] <:< DiffTypes.ShortType || Type[A] <:< DiffTypes.IntType ||
        Type[A] <:< DiffTypes.LongType || Type[A] <:< DiffTypes.FloatType ||
        Type[A] <:< DiffTypes.DoubleType || Type[A] <:< DiffTypes.CharType ||
        Type[A] <:< DiffTypes.BigDecimalType || Type[A] <:< DiffTypes.BigIntType
      ) {
        Rule.matched(Expr.quote {
          val l = Expr.splice(dctx.left)
          val r = Expr.splice(dctx.right)
          if (l == r)
            DiffResult.Identical(Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn), l.toString)
          else
            DiffResult
              .ValueChanged(Expr.splice(pn), Expr.splice(fn), Expr.splice(sn), Expr.splice(sn), l.toString, r.toString)
        })
      } else {
        Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }
}
