package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait OrderBuiltInRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object OrderBuiltInRule extends OrderDerivationRule("built-in Order for primitives") {

    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val IntType: Type[Int] = OrderTypes.Int
      implicit val BooleanType: Type[Boolean] = OrderTypes.Boolean
      implicit val ByteType: Type[Byte] = OrderTypes.Byte
      implicit val ShortType: Type[Short] = OrderTypes.Short
      implicit val LongType: Type[Long] = OrderTypes.Long
      implicit val FloatType: Type[Float] = OrderTypes.Float
      implicit val DoubleType: Type[Double] = OrderTypes.Double
      implicit val CharType: Type[Char] = OrderTypes.Char
      implicit val StringType: Type[String] = OrderTypes.String
      Log.info(s"Checking built-in Order for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< OrderTypes.Boolean)
          Rule.matched(
            Expr.quote(
              java.lang.Boolean.compare(
                Expr.splice(octx.x.upcast[Boolean]),
                Expr.splice(octx.y.upcast[Boolean])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Byte)
          Rule.matched(
            Expr.quote(
              java.lang.Byte.compare(
                Expr.splice(octx.x.upcast[Byte]),
                Expr.splice(octx.y.upcast[Byte])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Short)
          Rule.matched(
            Expr.quote(
              java.lang.Short.compare(
                Expr.splice(octx.x.upcast[Short]),
                Expr.splice(octx.y.upcast[Short])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Int)
          Rule.matched(
            Expr.quote(
              java.lang.Integer.compare(
                Expr.splice(octx.x.upcast[Int]),
                Expr.splice(octx.y.upcast[Int])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Long)
          Rule.matched(
            Expr.quote(
              java.lang.Long.compare(
                Expr.splice(octx.x.upcast[Long]),
                Expr.splice(octx.y.upcast[Long])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Float)
          Rule.matched(
            Expr.quote(
              java.lang.Float.compare(
                Expr.splice(octx.x.upcast[Float]),
                Expr.splice(octx.y.upcast[Float])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Double)
          Rule.matched(
            Expr.quote(
              java.lang.Double.compare(
                Expr.splice(octx.x.upcast[Double]),
                Expr.splice(octx.y.upcast[Double])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.Char)
          Rule.matched(
            Expr.quote(
              java.lang.Character.compare(
                Expr.splice(octx.x.upcast[Char]),
                Expr.splice(octx.y.upcast[Char])
              )
            )
          )
        else if (Type[A] <:< OrderTypes.String)
          Rule.matched(
            Expr.quote(
              Expr.splice(octx.x.upcast[String]).compareTo(Expr.splice(octx.y.upcast[String]))
            )
          )
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }
}
