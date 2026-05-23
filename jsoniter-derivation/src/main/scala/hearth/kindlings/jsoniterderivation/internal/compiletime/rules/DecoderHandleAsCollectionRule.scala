package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            if (dctx.evaluatedConfig.isDefined) decodeCollectionInline[A, Item](isCollection.value)
            else decodeCollectionViaHelper[A, Item](isCollection.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeCollectionInline[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      implicit val IntT: Type[Int] = CTypes.Int
      val maxInserts = dctx.evaluatedConfig.get.setMaxInsertNumber
      import isCollection.CtorResult
      deriveDecoderRecursively[Item](using dctx.nest[Item](dctx.reader)).map { decodeItemExpr =>
        val factoryExpr = isCollection.factory
        val buildStep = isCollection.build

        val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
          val reader = Expr.splice(dctx.reader)
          val collBuilder = Expr.splice(factoryExpr).newBuilder
          if (!reader.isNextToken('['.toByte)) reader.decodeError("expected '['")
          if (!reader.isNextToken(']'.toByte)) {
            reader.rollbackToken()
            var count: Int = 1
            collBuilder += Expr.splice(decodeItemExpr)
            while (reader.isNextToken(','.toByte)) {
              count += 1
              if (count > Expr.splice(Expr(maxInserts)))
                reader.decodeError("too many collection items (max: " + Expr.splice(Expr(maxInserts)) + ")")
              collBuilder += Expr.splice(decodeItemExpr)
            }
            if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
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
                case Left(err)    => throw new IllegalArgumentException(err)
              }
            })

          case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
            val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
            Rule.matched(Expr.quote {
              Expr.splice(eitherExpr) match {
                case Right(value) => value
                case Left(errs)   => throw new IllegalArgumentException(errs.mkString(", "))
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
                  else throw new IllegalArgumentException("collection construction failed")
              }
            })
        }
      }
    }

    private def decodeCollectionViaHelper[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      import isCollection.CtorResult
      LambdaBuilder
        .of1[JsonReader]("itemReader")
        .traverse { itemReaderExpr =>
          deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
        }
        .map { builder =>
          val decodeFn = builder.build[Item]
          val factoryExpr = isCollection.factory
          val buildStep = isCollection.build

          val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
            val decodeFnVal = Expr.splice(decodeFn)
            val reader = Expr.splice(dctx.reader)
            val maxInserts = Expr.splice(dctx.config).setMaxInsertNumber
            val collBuilder = Expr.splice(factoryExpr).newBuilder
            if (!reader.isNextToken('['.toByte)) reader.decodeError("expected '['")
            if (!reader.isNextToken(']'.toByte)) {
              reader.rollbackToken()
              var count: Int = 1
              collBuilder += decodeFnVal(reader)
              while (reader.isNextToken(','.toByte)) {
                count += 1
                if (count > maxInserts)
                  reader.decodeError("too many collection items (max: " + maxInserts + ")")
                collBuilder += decodeFnVal(reader)
              }
              if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
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
                  case Left(err)    => throw new IllegalArgumentException(err)
                }
              })

            case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
              val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
              Rule.matched(Expr.quote {
                Expr.splice(eitherExpr) match {
                  case Right(value) => value
                  case Left(errs)   => throw new IllegalArgumentException(errs.mkString(", "))
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
                    else throw new IllegalArgumentException("collection construction failed")
                }
              })
          }
        }
    }
  }

}
