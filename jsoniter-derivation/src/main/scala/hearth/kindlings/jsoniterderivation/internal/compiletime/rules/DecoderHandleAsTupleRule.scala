package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

/** Tuples are JSON arrays, as in jsoniter-scala: `[1,"a"]` decodes as `(1, "a")`, a missing element fails with
  * `expected ','`, an extra one with `expected ']'`, and JSON `null` decodes as `null`.
  */
trait DecoderHandleAsTupleRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsTupleRule extends DecoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      tupleElementTypes[A] match {
        case None           => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
        case Some(elements) =>
          implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
          val reader = dctx.reader
          elements.zipWithIndex
            .parTraverse { case (element, index) =>
              import element.Underlying as E
              deriveDecoderRecursively[E](using dctx.nest[E](reader)).map { decoded =>
                // Constructor arguments are evaluated left to right, so the reads happen in order.
                val read: Expr[E] =
                  if (index == 0) decoded
                  else
                    Expr.quote {
                      if (!Expr.splice(reader).isNextToken(','.toByte)) Expr.splice(reader).commaError()
                      Expr.splice(decoded)
                    }
                read.as_??
              }
            }
            .flatMap { reads =>
              val constructed: MIO[Expr[A]] = CaseClass.parse[A].toOption match {
                case Some(cc) =>
                  val names = cc.primaryConstructor.totalParameters.flatten.toList.map(_._1)
                  foldInstanceFree(cc.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => names.zip(reads).toMap
                  ) match {
                    case Right(expr) => MIO.pure(expr.value.asInstanceOf[Expr[A]])
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                case None =>
                  constructGenericTuple[A](reads) match {
                    case Some(expr) => MIO.pure(expr)
                    case None       =>
                      val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                      Log.error(err.message) >> MIO.fail(err)
                  }
              }
              constructed.map { construct =>
                Rule.matched(Expr.quote {
                  if (Expr.splice(reader).isNextToken('['.toByte)) {
                    val tuple = Expr.splice(construct)
                    if (!Expr.splice(reader).isNextToken(']'.toByte)) Expr.splice(reader).arrayEndError()
                    tuple
                  } else Expr.splice(reader).readNullOrTokenError(null.asInstanceOf[A], '['.toByte)
                })
              }
            }
      }
  }
}
