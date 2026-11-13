package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

/** Tuples are JSON arrays, as in jsoniter-scala (`(1, "a")` is `[1,"a"]`). */
trait EncoderHandleAsTupleRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsTupleRule extends EncoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      tupleElementTypes[A] match {
        case None           => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        case Some(elements) =>
          implicit val UnitT: Type[Unit] = CTypes.Unit
          implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
          implicit val ProductT: Type[Product] = CTypes.Product
          // `TupleN` fields are read with their accessors, generic tuples (Scala 3) with `productElement`.
          // (Hearth's `caseFieldValuesAt` finds no fields of `Tuple22`, hence the size check.)
          val accessors = CaseClass.parse[A].toOption.map(_.caseFieldValuesAt(ectx.value).toList.map(_._2))
          val elementExprs: List[Expr_??] = accessors match {
            case Some(fields) if fields.size == elements.size => fields
            case _                                            =>
              elements.zipWithIndex.map { case (element, index) =>
                import element.Underlying as E
                Expr
                  .quote {
                    Expr.splice(ectx.value).asInstanceOf[Product].productElement(Expr.splice(Expr(index))).asInstanceOf[E]
                  }
                  .as_??
              }
          }
          elementExprs
            .parTraverse { element =>
              import element.{Underlying as E, value as elementExpr}
              deriveEncoderRecursively[E](using ectx.nest(elementExpr))
            }
            .map { encoded =>
              val body = encoded.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, e) =>
                Expr.quote {
                  Expr.splice(acc)
                  Expr.splice(e)
                }
              }
              Rule.matched(Expr.quote {
                Expr.splice(ectx.writer).writeArrayStart()
                Expr.splice(body)
                Expr.splice(ectx.writer).writeArrayEnd()
              })
            }
      }
  }
}
