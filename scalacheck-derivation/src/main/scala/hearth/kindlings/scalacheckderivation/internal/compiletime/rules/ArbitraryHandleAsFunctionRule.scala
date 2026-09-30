package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Cogen, Gen}

trait ArbitraryHandleAsFunctionRuleImpl {
  this: ArbitraryMacrosImpl & CogenMacrosImpl & MacroCommons & StdExtensions =>

  /** `FunctionN` (0-22) and `PartialFunction`: the `Cogen`s for the arguments and the `Arbitrary` for the result are
    * derived within the same expansion (instead of ScalaCheck's `arbFunctionN`/`arbPartialFunction`, which need them as
    * implicits), so e.g. `Int => MyCaseClass` or `MyCaseClass => Int` need no instance in scope.
    */
  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryHandleAsFunctionRule extends ArbitraryDerivationRule("handle as function when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]
      implicit val CogenAny: Type[Cogen[Any]] = Type.of[Cogen[Any]]
      implicit val GenAny: Type[Gen[Any]] = Type.of[Gen[Any]]

      functionArity[A] match {
        case Some(arity) =>
          val typeArgs: List[??] = Type.typeArguments[A]
          Log.info(s"Handling ${Type[A].prettyPrint} as Function$arity") >>
            (for {
              argCogens <- traverseTypes(typeArgs.init)(arg => deriveCogenForArbitrary(arg))
              resultGen <- {
                val result: ?? = typeArgs.last
                import result.Underlying as Z
                deriveArbitraryRecursively[Z](using arbctx.nest[Z])
                  .map(gen => Expr.quote(Expr.splice(gen).asInstanceOf[Gen[Any]]))
              }
            } yield {
              val cogens = listExpr(argCogens)
              val arityExpr = Expr(arity)
              Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                  .genFunction[A](Expr.splice(arityExpr), Expr.splice(cogens), Expr.splice(resultGen))
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
              argCogen <- deriveCogenRecursively[In](using CogenCtx(Type[In], arbctx.cache, derivedType = None))
              resultGen <- deriveArbitraryRecursively[Option[Out]](using arbctx.nest[Option[Out]])
            } yield Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                .genPartialFunction[In, Out](Expr.splice(argCogen), Expr.splice(resultGen))
                .asInstanceOf[Gen[A]]
            }))
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a function"))
      }
    }

    /** The argument's `Cogen` is derived by the Cogen rules, sharing this expansion's cache (so recursive types reached
      * through both type classes reuse the same cached definitions). `derivedType = None`: the type being derived is an
      * `Arbitrary`, so any implicit `Cogen` - even for the same type - is a legitimate candidate.
      */
    private def deriveCogenForArbitrary[A: ArbitraryCtx](arg: ??): MIO[Expr[Cogen[Any]]] = {
      import arg.Underlying as T
      implicit val CogenAny: Type[Cogen[Any]] = Type.of[Cogen[Any]]
      deriveCogenRecursively[T](using CogenCtx(Type[T], arbctx.cache, derivedType = None))
        .map(cogen => Expr.quote(Expr.splice(cogen).asInstanceOf[Cogen[Any]]))
    }
  }

  /** `Future` and `Gen`: like ScalaCheck's `arbFuture`/`arbGen`, but with the value's `Arbitrary` derived. */
  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryHandleAsFutureOrGenRule extends ArbitraryDerivationRule("handle as Future or Gen when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]
      val isFuture = isFutureType[A]
      if (isFuture || isGenType[A]) {
        val inner: ?? = Type.typeArguments[A].head
        import inner.Underlying as T
        Log.info(s"Handling ${Type[A].prettyPrint} as ${if (isFuture) "Future" else "Gen"}") >>
          deriveArbitraryRecursively[T](using arbctx.nest[T]).map { gen =>
            if (isFuture)
              Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                  .genFuture[T](Expr.splice(gen))
                  .asInstanceOf[Gen[A]]
              })
            else
              Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                  .genGen[T](Expr.splice(gen))
                  .asInstanceOf[Gen[A]]
              })
          }
      } else MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is neither a Future nor a Gen"))
    }
  }
}
