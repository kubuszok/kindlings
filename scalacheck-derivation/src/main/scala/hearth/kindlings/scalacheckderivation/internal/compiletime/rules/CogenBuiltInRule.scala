package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenBuiltInRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object CogenBuiltInRule extends CogenDerivationRule("use ScalaCheck built-in Cogen") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] = {
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

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
        CogenTypes.Cogen[A].summonExpr.toEither match {
          case Right(cogenExpr) =>
            Log.info(s"Using ScalaCheck built-in Cogen for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(cogenExpr))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No Cogen[${Type[A].prettyPrint}]: $reason"))
        }
      } else {
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type"))
      }
    }
  }
}
