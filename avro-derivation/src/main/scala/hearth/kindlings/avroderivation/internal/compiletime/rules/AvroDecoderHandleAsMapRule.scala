package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait AvroDecoderHandleAsMapRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object AvroDecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Any]("valueRaw")
          .traverse { valueRawExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueRawExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Value]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val decodeFnVal = Expr.splice(decodeFn)
              val rawMap = Expr.splice(dctx.avroValue).asInstanceOf[java.util.Map[Any, Any]]
              val iter = rawMap.entrySet().iterator()
              while (iter.hasNext) {
                val entry = iter.next()
                val key = entry.getKey.toString
                val value = decodeFnVal(entry.getValue)
                mapBuilder += Expr.splice(isMap.pair(Expr.quote(key.asInstanceOf[Key]), Expr.quote(value)))
              }
              mapBuilder
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
}
