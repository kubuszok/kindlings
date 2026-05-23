package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait AvroDecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val AnyT: Type[Any] = DecTypes.Any

            LambdaBuilder
              .of1[Any]("itemRaw")
              .traverse { itemRawExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemRawExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                val buildStep = isCollection.value.build

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  val decodeFnVal = Expr.splice(decodeFn)
                  val rawCollection = Expr.splice(dctx.avroValue).asInstanceOf[java.util.Collection[Any]]
                  val iter = rawCollection.iterator()
                  while (iter.hasNext)
                    collBuilder += decodeFnVal(iter.next())
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
                        case Left(err)    => throw new org.apache.avro.AvroRuntimeException(err)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(errs)   =>
                          throw new org.apache.avro.AvroRuntimeException(errs.mkString("\n"))
                      }
                    })

                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(err)    =>
                          throw new org.apache.avro.AvroRuntimeException(err.getMessage, err)
                      }
                    })

                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      Expr.splice(eitherExpr) match {
                        case Right(value) => value
                        case Left(errs)   =>
                          throw new org.apache.avro.AvroRuntimeException(errs.map(_.getMessage).mkString("\n"))
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
