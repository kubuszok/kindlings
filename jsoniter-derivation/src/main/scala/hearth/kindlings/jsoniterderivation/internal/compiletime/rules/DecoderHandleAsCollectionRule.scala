package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

trait DecoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection or map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of the old map-rule (`IsMap.parse` = `IsCollection.parse` + cast) followed by a separate
            // collection rule that parsed `IsCollection` again — every non-map field paid for the parse twice.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                DecoderHandleAsMapRule.decodeMapEntries[A, Item](isMapOf)
              case None =>
                implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
                if (dctx.evaluatedConfig.isDefined) decodeCollectionInline[A, Item](isCollection.value)
                else decodeCollectionViaHelper[A, Item](isCollection.value)
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }

    /** As in jsoniter-scala, `setMaxInsertNumber` limits only sets (not other collections, nor bit sets). */
    @scala.annotation.nowarn("msg=is never used")
    private def isSet[A: Type, Item: Type]: Boolean = Type[A] <:< Type.of[scala.collection.Set[Item]]

    /** As in jsoniter-scala, a JSON `null` decodes as an empty collection (the codec's `nullValue`), for collections
      * that have a plain empty instance (not `::` or non-empty collections built with a smart constructor).
      */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def nullAsEmpty[A: Type, Item](isCollection: IsCollectionOf[A, Item]): Boolean =
      !(Type[A] <:< Type.of[scala.collection.immutable.::[Any]]) && (isCollection.build match {
        case _: CtorLikeOf.PlainValue[?, ?] => true
        case _                              => false
      })

    @scala.annotation.nowarn("msg=is never used")
    private def decodeCollectionInline[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val IntT: Type[Int] = CTypes.Int
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      val maxInserts = if (isSet[A, Item]) dctx.evaluatedConfig.get.setMaxInsertNumber else Int.MaxValue
      val acceptsNull = nullAsEmpty[A, Item](isCollection)
      import isCollection.CtorResult
      deriveDecoderRecursively[Item](using dctx.nest[Item](dctx.reader)).map { decodeItemExpr =>
        val factoryExpr = isCollection.factory
        val buildStep = isCollection.build

        val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
          val reader = Expr.splice(dctx.reader)
          val collBuilder = Expr.splice(factoryExpr).newBuilder
          if (
            JsoniterDerivationUtils.openArray(reader, Expr.splice(Expr(acceptsNull))) && !reader.isNextToken(']'.toByte)
          ) {
            reader.rollbackToken()
            // as jsoniter-scala: checked after every insert (so a limit of 0 rejects the first element)
            var count: Int = 1
            collBuilder += Expr.splice(decodeItemExpr)
            if (count > Expr.splice(Expr(maxInserts))) reader.decodeError("too many set inserts")
            while (reader.isNextToken(','.toByte)) {
              count += 1
              collBuilder += Expr.splice(decodeItemExpr)
              if (count > Expr.splice(Expr(maxInserts))) reader.decodeError("too many set inserts")
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
                case Left(err)    => Expr.splice(dctx.reader).decodeError(err.getMessage)
              }
            })

          case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
            val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
            Rule.matched(Expr.quote {
              Expr.splice(eitherExpr) match {
                case Right(value) => value
                case Left(errs)   =>
                  val iter = errs.iterator
                  if (iter.hasNext) Expr.splice(dctx.reader).decodeError(iter.next().getMessage)
                  else Expr.splice(dctx.reader).decodeError("collection construction failed")
              }
            })
        }
      }
    }

    private def decodeCollectionViaHelper[A: DecoderCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    )(implicit JsonReaderT: Type[JsonReader]): MIO[Rule.Applicability[Expr[A]]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val IntT: Type[Int] = CTypes.Int
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
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

          val limited = isSet[A, Item]
          val acceptsNull = nullAsEmpty[A, Item](isCollection)
          val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
            val decodeFnVal = Expr.splice(decodeFn)
            val reader = Expr.splice(dctx.reader)
            val maxInserts =
              if (Expr.splice(Expr(limited))) Expr.splice(dctx.config).setMaxInsertNumber else Int.MaxValue
            val collBuilder = Expr.splice(factoryExpr).newBuilder
            if (
              JsoniterDerivationUtils.openArray(reader, Expr.splice(Expr(acceptsNull))) &&
              !reader.isNextToken(']'.toByte)
            ) {
              reader.rollbackToken()
              var count: Int = 1
              collBuilder += decodeFnVal(reader)
              if (count > maxInserts) reader.decodeError("too many set inserts")
              while (reader.isNextToken(','.toByte)) {
                count += 1
                collBuilder += decodeFnVal(reader)
                if (count > maxInserts) reader.decodeError("too many set inserts")
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
                  case Left(err)    => Expr.splice(dctx.reader).decodeError(err.getMessage)
                }
              })

            case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
              val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
              Rule.matched(Expr.quote {
                Expr.splice(eitherExpr) match {
                  case Right(value) => value
                  case Left(errs)   =>
                    val iter = errs.iterator
                    if (iter.hasNext) Expr.splice(dctx.reader).decodeError(iter.next().getMessage)
                    else Expr.splice(dctx.reader).decodeError("collection construction failed")
                }
              })
          }
        }
    }
  }

}
