package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Cogen, Gen}

trait CogenHandleAsFunctionRuleImpl {
  this: CogenMacrosImpl & ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  /** `FunctionN` (0-22) and `PartialFunction`: the `Arbitrary`s for the arguments and the `Cogen` for the result are
    * derived within the same expansion (instead of ScalaCheck's `Cogen.functionN`/`cogenPartialFunction`, which need
    * them as implicits), so e.g. `MyCaseClass => Int` or `Int => MyCaseClass` need no instance in scope.
    */
  @scala.annotation.nowarn("msg=is never used")
  object CogenHandleAsFunctionRule extends CogenDerivationRule("handle as function when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] = {
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]
      implicit val CogenAny: Type[Cogen[Any]] = Type.of[Cogen[Any]]
      implicit val GenAny: Type[Gen[Any]] = Type.of[Gen[Any]]

      functionArity[A] match {
        case Some(arity) =>
          val typeArgs: List[??] = Type.typeArguments[A]
          Log.info(s"Handling ${Type[A].prettyPrint} as Function$arity") >>
            (for {
              argGens <- traverseTypes(typeArgs.init)(arg => deriveArbitraryForCogen(arg))
              resultCogen <- {
                val result: ?? = typeArgs.last
                import result.Underlying as Z
                deriveCogenRecursively[Z](using cogenctx.nest[Z])
                  .map(cogen => Expr.quote(Expr.splice(cogen).asInstanceOf[Cogen[Any]]))
              }
            } yield {
              val gens = listExpr(argGens)
              val arityExpr = Expr(arity)
              Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                  .cogenFunction[A](Expr.splice(arityExpr), Expr.splice(gens), Expr.splice(resultCogen))
              })
            })
        case None if isPartialFunctionType[A] =>
          val typeArgs: List[??] = Type.typeArguments[A]
          val in: ?? = typeArgs.head
          val out: ?? = typeArgs(1)
          import in.Underlying as In
          import out.Underlying as Out
          implicit val OptionOut: Type[Option[Out]] = Type.of[Option[Out]]
          Log.info(s"Handling ${Type[A].prettyPrint} as PartialFunction") >>
            (for {
              argGen <- deriveArbitraryRecursively[In](using ArbitraryCtx(Type[In], cogenctx.cache, derivedType = None))
              resultCogen <- deriveCogenRecursively[Option[Out]](using cogenctx.nest[Option[Out]])
            } yield Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                .cogenPartialFunction[In, Out](Expr.splice(argGen), Expr.splice(resultCogen))
                .asInstanceOf[Cogen[A]]
            }))
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a function"))
      }
    }

    /** The argument's `Arbitrary` is derived by the Arbitrary rules, sharing this expansion's cache. `derivedType =
      * None`: the type being derived is a `Cogen`, so any implicit `Arbitrary` is a legitimate candidate.
      */
    private def deriveArbitraryForCogen[A: CogenCtx](arg: ??): MIO[Expr[Gen[Any]]] = {
      import arg.Underlying as T
      implicit val GenAny: Type[Gen[Any]] = Type.of[Gen[Any]]
      deriveArbitraryRecursively[T](using ArbitraryCtx(Type[T], cogenctx.cache, derivedType = None))
        .map(gen => Expr.quote(Expr.splice(gen).asInstanceOf[Gen[Any]]))
    }
  }
}
