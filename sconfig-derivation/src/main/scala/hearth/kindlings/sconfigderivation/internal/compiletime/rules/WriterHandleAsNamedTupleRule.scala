package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait WriterHandleAsNamedTupleRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsNamedTupleRule extends WriterDerivationRule("handle as named tuple when possible") {

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- wctx.setHelper[A] { (value, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using wctx.nestInCache(value, config))
              }
              result <- wctx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(wctx.value, wctx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeNamedTupleFields[A: WriterCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[ConfigValue]] = {
      implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
      implicit val StringT: Type[String] = WTypes.String
      implicit val ProductType: Type[Product] = WTypes.Product
      implicit val IntType: Type[Int] = WTypes.Int

      val fields = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(wctx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(s"Writing named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveWriterRecursively[Field](using wctx.nest(fieldExpr)).map { fieldValueExpr =>
                  (fName, fieldValueExpr)
                }
              }
            }
            .map { fieldData =>
              fieldData.toList.foldRight(Expr.quote(List.empty[(String, ConfigValue)])) {
                case ((fName, fieldValueExpr), acc) =>
                  Expr.quote {
                    (
                      Expr.splice(wctx.config).transformMemberNames(Expr.splice(Expr(fName))),
                      Expr.splice(fieldValueExpr)
                    ) ::
                      Expr.splice(acc)
                  }
              }
            }
            .map { fieldsListExpr =>
              Expr.quote {
                SConfigDerivationUtils.writeFields(Expr.splice(fieldsListExpr))
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            SConfigDerivationUtils.writeFields(Nil)
          })
      }
    }
  }
}
