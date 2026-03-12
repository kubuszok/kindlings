package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EmptyEnumRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  object EmptyEnumRule extends EmptyDerivationRule("Empty as enum") {

    @scala.annotation.nowarn("msg=is never used|is unchecked")
    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          val children = enumm.exhaustiveChildren match {
            case Some(m) => m.toList.map(_._2)
            case None    =>
              return MIO.pure(Rule.yielded(s"${Type[A].prettyPrint}: no children found"))
          }

          val emptyVariants: List[Expr[A]] = children.flatMap { (child: ??<:[A]) =>
            val childType = child.Underlying
            implicit val ct: Type[child.Underlying] = childType
            implicit val EmptyChild: Type[alleycats.Empty[child.Underlying]] = EmptyTypes.Empty[child.Underlying]
            if (ectx.derivedType.exists(_.Underlying =:= childType)) None
            else
              EmptyTypes.Empty[child.Underlying].summonExprIgnoring().toOption.map { instanceExpr =>
                Expr.quote(Expr.splice(instanceExpr).empty.asInstanceOf[A])
              }
          }

          emptyVariants match {
            case List(singleEmpty) =>
              MIO.pure(Rule.matched(singleEmpty))
            case Nil =>
              MIO.pure(
                Rule.yielded(
                  s"${Type[A].prettyPrint}: no variant has an Empty instance"
                )
              )
            case _ =>
              MIO.fail(EmptyDerivationError.MultipleEmptyVariants(Type[A].prettyPrint))
          }
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
