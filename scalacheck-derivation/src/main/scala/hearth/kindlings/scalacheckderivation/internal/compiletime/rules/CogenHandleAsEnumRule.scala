package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsEnumRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  object CogenHandleAsEnumRule extends CogenDerivationRule("handle as enum when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      Enum.parse[A].toEither match {
        case Right(enumData) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as enum") >>
            deriveEnumCogen[A](enumData).map(Rule.matched(_))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumCogen[A: CogenCtx](enumData: Enum[A]): MIO[Expr[Cogen[A]]] = {
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

      val childrenList = enumData.directChildren.toList

      childrenList match {
        case Nil =>
          MIO.fail(new RuntimeException(s"Enum ${Type[A].prettyPrint} has no cases"))
        case children =>
          // Derive Cogen for each enum case
          NonEmptyList
            .fromList(children)
            .get
            .parTraverse { case (_, enumChild) =>
              import enumChild.Underlying as CaseType
              Log.namedScope(s"Deriving Cogen for enum case ${CaseType.prettyPrint}") {
                deriveCogenRecursively[CaseType](using cogenctx.nest[CaseType]).map { caseCogen =>
                  Expr.quote(Expr.splice(caseCogen).asInstanceOf[Cogen[A]])
                }
              }
            }
            .map { caseCogens =>
              // Build a list of Cogen[A] for runtime dispatch
              val cogensListExpr: Expr[List[Cogen[A]]] =
                caseCogens.toList.foldRight(Expr.quote(List.empty[Cogen[A]])) { (cogenExpr, acc) =>
                  Expr.quote(Expr.splice(cogenExpr) :: Expr.splice(acc))
                }

              // At runtime, dispatch to the correct Cogen based on the value's actual class
              Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                  .cogenEnum(Expr.splice(cogensListExpr))
              }
            }
      }
    }
  }
}
