package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait GroupBuiltInRuleImpl {
  this: GroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object GroupBuiltInRule extends GroupDerivationRule("built-in Group for numeric primitives") {

    def apply[A: GroupCtx]: MIO[Rule.Applicability[GroupDerivationResult[A]]] = {
      implicit val ByteType: Type[Byte] = SemigroupTypes.Byte
      implicit val ShortType: Type[Short] = SemigroupTypes.Short
      implicit val IntType: Type[Int] = SemigroupTypes.Int
      implicit val LongType: Type[Long] = SemigroupTypes.Long
      implicit val FloatType: Type[Float] = SemigroupTypes.Float
      implicit val DoubleType: Type[Double] = SemigroupTypes.Double

      Log.info(s"Checking built-in Group for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< SemigroupTypes.Byte)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0.toByte).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote((-Expr.splice(a.upcast[Byte])).toByte).asInstanceOf[Expr[A]]
            )
          )
        else if (Type[A] <:< SemigroupTypes.Short)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0.toShort).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote((-Expr.splice(a.upcast[Short])).toShort).asInstanceOf[Expr[A]]
            )
          )
        else if (Type[A] <:< SemigroupTypes.Int)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote(-Expr.splice(a.upcast[Int])).asInstanceOf[Expr[A]]
            )
          )
        else if (Type[A] <:< SemigroupTypes.Long)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0L).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote(-Expr.splice(a.upcast[Long])).asInstanceOf[Expr[A]]
            )
          )
        else if (Type[A] <:< SemigroupTypes.Float)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0.0f).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote(-Expr.splice(a.upcast[Float])).asInstanceOf[Expr[A]]
            )
          )
        else if (Type[A] <:< SemigroupTypes.Double)
          Rule.matched(
            mkNumericGroup[A](
              Expr(0.0d).asInstanceOf[Expr[A]],
              (a: Expr[A]) => Expr.quote(-Expr.splice(a.upcast[Double])).asInstanceOf[Expr[A]]
            )
          )
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in Group type (String has no Group)")
      }
    }

    private def mkNumericGroup[A: GroupCtx](
        emptyVal: Expr[A],
        negate: Expr[A] => Expr[A]
    ): GroupDerivationResult[A] = {
      val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) => {
        val sgCtx = SemigroupCtx.from(x, y, gctx.derivedType)
        for {
          result <- deriveSemigroupRecursively[A](using sgCtx)
          cache <- sgCtx.cache.get
        } yield cache.toValDefs.use(_ => result)
      }
      val inverse: Expr[A] => MIO[Expr[A]] = a => MIO.pure(negate(a))
      GroupDerivationResult(emptyVal, combine, inverse)
    }
  }
}
