package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader

trait DecoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

            LambdaBuilder
              .of1[UBJsonReader]("itemReader")
              .traverse { itemReaderExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val decodeFnVal = Expr.splice(decodeFn)
                  val reader = Expr.splice(dctx.reader)
                  val maxInserts = Expr.splice(dctx.config).setMaxInsertNumber
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  reader.readArrayStart()
                  var count = 0
                  while (!reader.isArrayEnd()) {
                    count += 1
                    if (count > maxInserts)
                      reader.decodeError("too many collection items (max: " + maxInserts + ")")
                    collBuilder += decodeFnVal(reader)
                  }
                  collBuilder
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(buildResultExpr.asInstanceOf[Expr[A]])

                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(err)    => Expr.splice(dctx.reader).decodeError(err)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(errs)   =>
                          Expr.splice(dctx.reader).decodeError(errs.mkString(", "))
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(err)    => throw err
                      }
                    })

                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(errs)   =>
                          val iter = errs.iterator
                          if (iter.hasNext) throw iter.next()
                          else Expr.splice(dctx.reader).decodeError("collection construction failed")
                      }
                    })
                }
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }

}
