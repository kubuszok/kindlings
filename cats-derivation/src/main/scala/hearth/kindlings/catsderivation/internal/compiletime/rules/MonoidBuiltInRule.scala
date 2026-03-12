package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait MonoidBuiltInRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object MonoidBuiltInRule extends MonoidDerivationRule("built-in Monoid for primitives") {

    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] =
      Log.info(s"Checking built-in Monoid for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< SemigroupTypes.Byte)
          Rule.matched(mkNumericMonoid[A](Expr(0.toByte).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.Short)
          Rule.matched(mkNumericMonoid[A](Expr(0.toShort).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.Int)
          Rule.matched(mkNumericMonoid[A](Expr(0).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.Long)
          Rule.matched(mkNumericMonoid[A](Expr(0L).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.Float)
          Rule.matched(mkNumericMonoid[A](Expr(0.0f).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.Double)
          Rule.matched(mkNumericMonoid[A](Expr(0.0d).asInstanceOf[Expr[A]]))
        else if (Type[A] <:< SemigroupTypes.String)
          Rule.matched(mkNumericMonoid[A](Expr("").asInstanceOf[Expr[A]]))
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in Monoid type")
      }

    private def mkNumericMonoid[A: MonoidCtx](emptyVal: Expr[A]): MonoidDerivationResult[A] = {
      val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) => {
        // Use a fresh SemigroupCtx with its own cache so combine defs are self-contained
        val sgCtx = SemigroupCtx.from(x, y, moidctx.derivedType)
        for {
          result <- deriveSemigroupRecursively[A](using sgCtx)
          cache <- sgCtx.cache.get
        } yield cache.toValDefs.use(_ => result)
      }
      MonoidDerivationResult(emptyVal, combine)
    }
  }
}
