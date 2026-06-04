package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsEnumRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  object ShrinkHandleAsEnumRule extends ShrinkDerivationRule("handle as enum when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      Enum.parse[A].toEither match {
        case Right(enumData) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as enum") >>
            deriveEnumShrink[A](enumData).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumShrink[A: ShrinkCtx](enumData: Enum[A]): MIO[Expr[Shrink[A]]] = {
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

      val childrenList = enumData.directChildren.toList

      childrenList match {
        case Nil =>
          MIO.fail(new RuntimeException(s"Enum ${Type[A].prettyPrint} has no cases"))
        case children =>
          // Derive Shrink for each enum case
          NonEmptyList
            .fromList(children)
            .get
            .parTraverse { case (_, enumChild) =>
              import enumChild.Underlying as CaseType
              Log.namedScope(s"Deriving Shrink for enum case ${CaseType.prettyPrint}") {
                deriveShrinkRecursively[CaseType](using shrinkctx.nest[CaseType]).map { caseShrink =>
                  Expr.quote(Expr.splice(caseShrink).asInstanceOf[Shrink[A]])
                }
              }
            }
            .map { caseShrinks =>
              // Build a list of Shrink[A] for runtime dispatch
              val shrinksListExpr: Expr[List[Shrink[A]]] =
                caseShrinks.toList.foldRight(Expr.quote(List.empty[Shrink[A]])) { (shrinkExpr, acc) =>
                  Expr.quote(Expr.splice(shrinkExpr) :: Expr.splice(acc))
                }

              // At runtime, dispatch to the correct Shrink based on the value's actual class
              Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
                  .shrinkEnum(Expr.splice(shrinksListExpr))
              }
            }
      }
    }
  }
}
