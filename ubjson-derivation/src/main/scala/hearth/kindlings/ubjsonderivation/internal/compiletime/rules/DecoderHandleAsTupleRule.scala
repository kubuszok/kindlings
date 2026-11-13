package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader

/** Tuples are arrays, as in jsoniter-scala (`[1,"a"]` is `(1, "a")`). */
trait DecoderHandleAsTupleRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsTupleRule extends DecoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      if (!Type[A].isTuple || Type[A].isNamedTuple)
        MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
      else
        CaseClass.parse[A].toEither match {
          case Left(reason) => MIO.pure(Rule.yielded(reason))
          case Right(cc)    =>
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
            // Arguments are evaluated left to right, so the elements are read in order.
            cc.construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                import field.tpe.Underlying as E
                deriveDecoderRecursively[E](using dctx.nest[E](dctx.reader))
              }
            }).flatMap {
              case Some(constructed) =>
                MIO.pure(Rule.matched(Expr.quote {
                  val reader = Expr.splice(dctx.reader)
                  reader.readArrayStart()
                  val tuple = Expr.splice(constructed)
                  if (!reader.isArrayEnd()) reader.decodeError("expected ']'")
                  tuple
                }))
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }
        }
  }
}
