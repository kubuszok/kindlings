package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsNamedTupleRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsNamedTupleRule extends EncoderDerivationRule("handle as named tuple when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ectx.setHelper[A] { (value, writer, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using ectx.nestInCache(value, writer, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.writer, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            // A named tuple behind a recursive type alias: encoded as the named tuple the alias stands for.
            unfoldRecursiveAlias[A].filter(_.Underlying.isNamedTuple) match {
              case Some(unfolded) =>
                import unfolded.Underlying as Unfolded
                deriveEncoderRecursively[Unfolded](using
                  ectx.nest[Unfolded](Expr.quote(Expr.splice(ectx.value).asInstanceOf[Unfolded]))
                ).map(Rule.matched(_))
              case None => MIO.pure(Rule.yielded(reason))
            }
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeNamedTupleFields[A: EncoderCtx](
        constructor: Method
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val ProductType: Type[Product] = CTypes.Product
      implicit val IntType: Type[Int] = CTypes.Int
      implicit val AnyT: Type[Any] = CTypes.Any

      val fields = constructor.totalParameters.flatten.toList

      val fieldsEnc = NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(ectx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(s"Encoding named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr).copy(stringified = Some(false))).map {
                  fieldEnc =>
                    val write: Expr[Unit] = Expr.quote {
                      Expr
                        .splice(ectx.writer)
                        .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                      Expr.splice(fieldEnc)
                    }
                    // As in jsoniter-scala, `transientNone`/`transientEmpty`/`transientNull` apply to the fields of
                    // named tuples as to those of case classes.
                    applySkipConditions(
                      write,
                      buildFieldSkipConditions[Field](fieldExpr, None, ectx.config, Map.empty)
                    )
                }
              }
            }
            .map { fieldWrites =>
              fieldWrites.toList
                .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, field) =>
                  Expr.quote {
                    Expr.splice(acc)
                    Expr.splice(field)
                  }
                }
            }
        case None =>
          MIO.pure(Expr.quote(()): Expr[Unit])
      }

      fieldsEnc.map { innerExpr =>
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(innerExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
    }
  }

}
