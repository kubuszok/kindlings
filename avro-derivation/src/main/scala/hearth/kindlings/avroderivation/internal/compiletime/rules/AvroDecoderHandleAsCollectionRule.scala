package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait AvroDecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection or map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of a separate map rule that re-parsed `IsCollection` for every non-map field.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                AvroDecoderHandleAsMapRule.decodeMapEntries[A, Item](isMapOf)
              case None =>
                handleCollection[A, Item](isCollection.value)
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def handleCollection[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isCollection.CtorResult
      implicit val AnyT: Type[Any] = DecTypes.Any

      LambdaBuilder
        .of1[Any]("itemRaw")
        .traverse { itemRawExpr =>
          deriveDecoderRecursively[Item](using dctx.nest[Item](itemRawExpr))
        }
        .map { builder =>
          val decodeFn = builder.build[Item]
          val factoryExpr = isCollection.factory
          val buildStep = isCollection.build

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
    }
  }
}
