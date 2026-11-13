package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Turns a collection/map builder expression (that may throw `CollectionBuildException`) into a result honouring
    * `dctx.failFast`, for every kind of collection constructor.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def wrapBuildResult[A: DecoderCtx](
      buildStep: CtorLikeOf[?, ?],
      buildResultExpr: Expr[Any]
  ): Rule.Applicability[Expr[Either[DecodingFailure, A]]] = {
    implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
    implicit val AnyT: Type[Any] = DTypes.Any
    val converted: Expr[Any] = buildStep match {
      case _: CtorLikeOf.PlainValue[?, ?] =>
        val value = buildResultExpr.asInstanceOf[Expr[A]]
        Expr.quote(CirceDerivationUtils.okResult(Expr.splice(dctx.failFast), Expr.splice(value)))
      case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
        Expr.quote {
          Expr.splice(eitherExpr) match {
            case Right(value) => CirceDerivationUtils.okResult(Expr.splice(dctx.failFast), value)
            case Left(err)    =>
              CirceDerivationUtils
                .failResult(Expr.splice(dctx.failFast), DecodingFailure(err, Expr.splice(dctx.cursor).history))
          }
        }
      case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
        Expr.quote {
          Expr.splice(eitherExpr) match {
            case Right(value) => CirceDerivationUtils.okResult(Expr.splice(dctx.failFast), value)
            case Left(errs)   =>
              CirceDerivationUtils.failResult(
                Expr.splice(dctx.failFast),
                DecodingFailure(errs.mkString("\n"), Expr.splice(dctx.cursor).history)
              )
          }
        }
      case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
        Expr.quote {
          Expr.splice(eitherExpr) match {
            case Right(value) => CirceDerivationUtils.okResult(Expr.splice(dctx.failFast), value)
            case Left(err)    =>
              CirceDerivationUtils.failResult(
                Expr.splice(dctx.failFast),
                DecodingFailure(err.getMessage, Expr.splice(dctx.cursor).history)
              )
          }
        }
      case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
        Expr.quote {
          Expr.splice(eitherExpr) match {
            case Right(value) => CirceDerivationUtils.okResult(Expr.splice(dctx.failFast), value)
            case Left(errs)   =>
              CirceDerivationUtils.failResult(
                Expr.splice(dctx.failFast),
                DecodingFailure(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.cursor).history)
              )
          }
        }
    }
    Rule.matched(Expr.quote {
      CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
        try Expr.splice(converted)
        catch {
          case e: CirceDerivationUtils.CollectionBuildException =>
            CirceDerivationUtils.collectionFailure(Expr.splice(dctx.failFast), e)
        }
      )
    })
  }

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection or map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
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
                import isCollection.value.CtorResult
                implicit val HCursorT: Type[HCursor] = DTypes.HCursor
                implicit val EitherDFItem: Type[Either[DecodingFailure, Item]] = DTypes.DecoderResult[Item]
                implicit val AnyT: Type[Any] = DTypes.Any

                LambdaBuilder
                  .of1[HCursor]("itemCursor")
                  .traverse { itemCursorExpr =>
                    deriveDecoderRecursively[Item](using dctx.nest[Item](itemCursorExpr)).map(_.upcast[Any])
                  }
                  .map { builder =>
                    val decodeFn = builder.build[Any]
                    val factoryExpr = isCollection.value.factory

                    // Like circe's SeqDecoder: fail-fast stops at the first element error, accumulating collects the
                    // errors of every element. Elements are decoded through the array cursor so that their errors keep
                    // the cursor history (e.g. `.members[1].age`).
                    val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                      val cursor = Expr.splice(dctx.cursor)
                      val failFast = Expr.splice(dctx.failFast)
                      val decode = Expr.splice(decodeFn)
                      val collBuilder = Expr.splice(factoryExpr).newBuilder
                      cursor.values match {
                        case None =>
                          throw new CirceDerivationUtils.CollectionBuildException(
                            CirceDerivationUtils.wrongTypeFailure("array", cursor)
                          )
                        case Some(jsonValues) =>
                          val errors = new CirceDerivationUtils.FailureCollector
                          var itemCursor: io.circe.ACursor = if (jsonValues.isEmpty) null else cursor.downArray
                          while (itemCursor ne null) {
                            val item = decode(itemCursor.asInstanceOf[HCursor])
                            if (errors.add(failFast, item)) collBuilder += CirceDerivationUtils.resultValue[Item](item)
                            val next = itemCursor.right
                            itemCursor = if (next.succeeded) next else null
                          }
                          errors.throwIfFailed()
                      }
                      collBuilder
                    }
                    wrapBuildResult[A](
                      isCollection.value.build,
                      isCollection.value.build.ctor(readLoop).asInstanceOf[Expr[Any]]
                    )
                  }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }
  }
}
