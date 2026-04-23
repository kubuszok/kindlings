package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkBuiltInRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShrinkBuiltInRule extends ShrinkDerivationRule("use ScalaCheck built-in Shrink") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] = {
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

      val isBuiltIn =
        Type[A] =:= Type.of[Boolean] ||
          Type[A] =:= Type.of[Byte] ||
          Type[A] =:= Type.of[Short] ||
          Type[A] =:= Type.of[Int] ||
          Type[A] =:= Type.of[Long] ||
          Type[A] =:= Type.of[Float] ||
          Type[A] =:= Type.of[Double] ||
          Type[A] =:= Type.of[Char] ||
          Type[A] =:= Type.of[String] ||
          Type[A] =:= Type.of[Unit] ||
          Type[A] =:= Type.of[BigInt] ||
          Type[A] =:= Type.of[BigDecimal]

      if (isBuiltIn) {
        ShrinkTypes.Shrink[A].summonExpr.toEither match {
          case Right(shrinkExpr) =>
            Log.info(s"Using ScalaCheck built-in Shrink for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(shrinkExpr))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No Shrink[${Type[A].prettyPrint}]: $reason"))
        }
      } else {
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type"))
      }
    }
  }
}
