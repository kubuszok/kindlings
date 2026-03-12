package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupBuiltInRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object SemigroupBuiltInRule extends SemigroupDerivationRule("built-in Semigroup for primitives") {

    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val ByteType: Type[Byte] = SemigroupTypes.Byte
      implicit val ShortType: Type[Short] = SemigroupTypes.Short
      implicit val IntType: Type[Int] = SemigroupTypes.Int
      implicit val LongType: Type[Long] = SemigroupTypes.Long
      implicit val FloatType: Type[Float] = SemigroupTypes.Float
      implicit val DoubleType: Type[Double] = SemigroupTypes.Double
      implicit val StringType: Type[String] = SemigroupTypes.String
      Log.info(s"Checking built-in Semigroup for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< SemigroupTypes.Byte)
          Rule.matched(
            Expr
              .quote(
                (Expr.splice(sgctx.x.upcast[Byte]) + Expr.splice(sgctx.y.upcast[Byte])).toByte
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.Short)
          Rule.matched(
            Expr
              .quote(
                (Expr.splice(sgctx.x.upcast[Short]) + Expr.splice(sgctx.y.upcast[Short])).toShort
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.Int)
          Rule.matched(
            Expr
              .quote(
                Expr.splice(sgctx.x.upcast[Int]) + Expr.splice(sgctx.y.upcast[Int])
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.Long)
          Rule.matched(
            Expr
              .quote(
                Expr.splice(sgctx.x.upcast[Long]) + Expr.splice(sgctx.y.upcast[Long])
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.Float)
          Rule.matched(
            Expr
              .quote(
                Expr.splice(sgctx.x.upcast[Float]) + Expr.splice(sgctx.y.upcast[Float])
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.Double)
          Rule.matched(
            Expr
              .quote(
                Expr.splice(sgctx.x.upcast[Double]) + Expr.splice(sgctx.y.upcast[Double])
              )
              .asInstanceOf[Expr[A]]
          )
        else if (Type[A] <:< SemigroupTypes.String)
          Rule.matched(
            Expr
              .quote(
                Expr.splice(sgctx.x.upcast[String]) + Expr.splice(sgctx.y.upcast[String])
              )
              .asInstanceOf[Expr[A]]
          )
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in Semigroup type")
      }
    }
  }
}
