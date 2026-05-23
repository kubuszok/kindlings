package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader

trait DecoderHandleAsMapRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

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
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val decodeFnVal = Expr.splice(decodeFn)
              val reader = Expr.splice(dctx.reader)
              val maxInserts = Expr.splice(dctx.config).mapMaxInsertNumber
              val mapBuilder =
                Expr.splice(factoryExpr).newBuilder
              reader.readObjectStart()
              var count = 0
              while (!reader.isObjectEnd()) {
                count += 1
                if (count > maxInserts)
                  reader.decodeError("too many map entries (max: " + maxInserts + ")")
                val key = reader.readFieldName()
                mapBuilder += Expr.splice(
                  isMap.pair(Expr.quote(key.asInstanceOf[Key]), Expr.quote(decodeFnVal(reader)))
                )
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
                    case Left(err)    => Expr.splice(dctx.reader).decodeError(err)
                  }
                })

              case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                Rule.matched(Expr.quote {
                  Expr.splice(eitherExpr) match {
                    case Right(value) => value
                    case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString(", "))
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
                      else Expr.splice(dctx.reader).decodeError("map construction failed")
                  }
                })
            }
          }
      } else {
        // Non-String keys — parse from string
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val keyDecoder: Expr[String => Key] =
              if (Key =:= Type.of[Int]) Expr.quote((s: String) => s.toInt.asInstanceOf[Key])
              else if (Key =:= Type.of[Long]) Expr.quote((s: String) => s.toLong.asInstanceOf[Key])
              else if (Key =:= Type.of[Double]) Expr.quote((s: String) => s.toDouble.asInstanceOf[Key])
              else Expr.quote((s: String) => s.asInstanceOf[Key])

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val decodeFnVal = Expr.splice(decodeFn)
              val keyDecoderVal = Expr.splice(keyDecoder)
              val reader = Expr.splice(dctx.reader)
              val maxInserts = Expr.splice(dctx.config).mapMaxInsertNumber
              val mapBuilder =
                Expr.splice(factoryExpr).newBuilder
              reader.readObjectStart()
              var count = 0
              while (!reader.isObjectEnd()) {
                count += 1
                if (count > maxInserts)
                  reader.decodeError("too many map entries (max: " + maxInserts + ")")
                val key = keyDecoderVal(reader.readFieldName())
                mapBuilder += Expr.splice(isMap.pair(Expr.quote(key), Expr.quote(decodeFnVal(reader))))
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
                    case Left(err)    => Expr.splice(dctx.reader).decodeError(err)
                  }
                })

              case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                Rule.matched(Expr.quote {
                  Expr.splice(eitherExpr) match {
                    case Right(value) => value
                    case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString(", "))
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
                      else Expr.splice(dctx.reader).decodeError("map construction failed")
                  }
                })
            }
          }
      }
    }
  }

}
