package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EmptyEnumRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  object EmptyEnumRule extends EmptyDerivationRule("Empty as enum") {

    /** The sealed trait's empty value is the one of the single subtype with an implicit `Empty`; without any, like
      * kittens, the one of the single subtype that forms an `Empty` - a case object (or parameterless enum case) or a
      * case class whose fields all form one. (Preferring implicit instances keeps accepting a superset of what kittens
      * accepts.)
      */
    @scala.annotation.nowarn("msg=is never used|is unchecked|Non local returns")
    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Enum.parse[A].toEither match {
        case Right(enumm) =>
          val children = enumm.exhaustiveChildren match {
            case Some(m) =>
              m.toList.map(_._2).filterNot(child => ectx.derivedType.exists(_.Underlying =:= child.Underlying))
            case None =>
              return MIO.pure(Rule.yielded(s"${Type[A].prettyPrint}: no children found"))
          }

          val withImplicit: List[Expr[A]] = children.flatMap { (child: ??<:[A]) =>
            implicit val ct: Type[child.Underlying] = child.Underlying
            implicit val EmptyChild: Type[alleycats.Empty[child.Underlying]] = EmptyTypes.Empty[child.Underlying]
            EmptyTypes.Empty[child.Underlying].summonExprIgnoring().toOption.map { instanceExpr =>
              Expr.quote(Expr.splice(instanceExpr).empty.asInstanceOf[A])
            }
          }

          if (withImplicit.nonEmpty) selectSingle[A](withImplicit.map(MIO.pure(_)))
          else
            selectSingle[A](
              children.filter(child => formsEmpty(child.Underlying.as_??, Set.empty, 0)).map { (child: ??<:[A]) =>
                implicit val ct: Type[child.Underlying] = child.Underlying
                SingletonValue.unapply(child.Underlying) match {
                  case Some(sv) =>
                    val singletonExpr = sv.singletonExpr
                    MIO.pure(Expr.quote(Expr.splice(singletonExpr).asInstanceOf[A]))
                  case None =>
                    deriveEmptyRecursively[child.Underlying](using ectx.nestType[child.Underlying]).map { childEmpty =>
                      Expr.quote(Expr.splice(childEmpty).asInstanceOf[A])
                    }
                }
              }
            )
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }

    private def selectSingle[A: EmptyCtx](variants: List[MIO[Expr[A]]]): MIO[Rule.Applicability[Expr[A]]] =
      variants match {
        case List(single) => single.map(Rule.matched(_))
        case Nil          => MIO.pure(Rule.yielded(s"${Type[A].prettyPrint}: no variant has an Empty instance"))
        case _            => MIO.fail(EmptyDerivationError.MultipleEmptyVariants(Type[A].prettyPrint))
      }

    /** Whether `tpe` forms an `Empty` (checked without deriving, so nothing is logged or cached for the subtypes that
      * are not selected): an implicit instance, a built-in type, a singleton, a case class whose fields all form one,
      * or a sealed hierarchy where exactly one subtype does.
      */
    private def formsEmpty(tpe: ??, visited: Set[String], depth: Int): Boolean = {
      import tpe.Underlying as T

      val key = Type[T].plainPrint
      if (depth > 8 || visited(key)) false
      else {
        val seen = visited + key
        EmptyTypes.Empty[T].summonExprIgnoring().toOption.isDefined ||
        isBuiltInEmpty[T] ||
        SingletonValue.unapply(Type[T]).isDefined ||
        (CaseClass.parse[T].toOption match {
          case Some(caseClass) =>
            caseClass.primaryConstructor.totalParameters.flatten.forall { case (_, param) =>
              formsEmpty(param.tpe, seen, depth + 1)
            }
          case None =>
            Enum
              .parse[T]
              .toOption
              .exists(
                _.directChildren.toList.count { case (_, child) =>
                  formsEmpty(child.Underlying.as_??, seen, depth + 1)
                } == 1
              )
        })
      }
    }

    private def isBuiltInEmpty[T: Type]: Boolean =
      Type[T] <:< EmptyTypes.Boolean || Type[T] <:< EmptyTypes.Byte || Type[T] <:< EmptyTypes.Short ||
        Type[T] <:< EmptyTypes.Int || Type[T] <:< EmptyTypes.Long || Type[T] <:< EmptyTypes.Float ||
        Type[T] <:< EmptyTypes.Double || Type[T] <:< EmptyTypes.Char || Type[T] <:< EmptyTypes.String
  }
}
