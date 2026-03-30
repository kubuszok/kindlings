package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryBuiltInRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryBuiltInRule extends ArbitraryDerivationRule("use ScalaCheck built-in Arbitrary") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      implicit val ArbitraryA: Type[Arbitrary[A]] = ArbitraryTypes.Arbitrary[A]

      // Check if it's a type with built-in ScalaCheck support by checking each type explicitly
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
        // For built-in types, summon the implicit Arbitrary instance
        ArbitraryTypes.Arbitrary[A].summonExpr.toEither match {
          case Right(arbExpr) =>
            Log.info(s"Using ScalaCheck built-in Arbitrary for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(Expr.quote {
                Expr.splice(arbExpr).arbitrary
              }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No Arbitrary[${Type[A].prettyPrint}]: $reason"))
        }
      } else {
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type"))
      }
    }
  }
}
