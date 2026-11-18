package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait FastShowPrettyHandleAsNamedTupleRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsNamedTupleRule extends DerivationRule("handle as named tuple when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ctx.setHelper[A] { (sb, config, level, value) =>
                deriveNamedTupleFields[A](namedTuple.primaryConstructor)(using
                  ctx.nestInCache(sb, value, config, level)
                )
              }
              result <- ctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveNamedTupleFields[A: DerivationCtx](
        constructor: Method
    ): MIO[Expr[StringBuilder]] = {
      implicit val IntType: Type[Int] = Types.Int
      implicit val ProductType: Type[Product] = Types.Product

      val fields = constructor.totalParameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fieldName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(ctx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(
                s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Type[Field].prettyPrint}"
              ) {
                deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
                  (fieldName, fieldResult)
                }
              }
            }
            // Rendered without a type name prefix: "(\n  name = ...,\n  age = ...\n)"
            .map(toAppend => renderFieldsFlat[A]("(\n", toAppend))
        case None =>
          MIO.pure {
            Expr.quote {
              Expr.splice(ctx.sb).append("()")
            }
          }
      }
    }

  }
}
