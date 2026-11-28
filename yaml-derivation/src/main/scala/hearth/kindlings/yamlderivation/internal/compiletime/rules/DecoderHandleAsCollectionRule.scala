package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node}

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection or map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
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
                implicit val NodeT: Type[Node] = DTypes.Node
                implicit val EitherCEItem: Type[Either[ConstructError, Item]] = DTypes.DecoderResult[Item]
                implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

                LambdaBuilder
                  .of1[Node]("itemNode")
                  .traverse { itemNodeExpr =>
                    deriveDecoderRecursively[Item](using dctx.nest[Item](itemNodeExpr))
                  }
                  .map { builder =>
                    val decodeFn = builder.build[Either[ConstructError, Item]]
                    val factoryExpr = isCollection.value.factory
                    val buildStep = isCollection.value.build
                    val classNameExpr: Expr[String] = Expr(runtimeClassName[A])

                    implicit val BuilderT: Type[scala.collection.mutable.Builder[Item, CtorResult]] =
                      Type.of[scala.collection.mutable.Builder[Item, CtorResult]]
                    // fail fast: the first failing item ends the decoding
                    def readLoopFailFast: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                      val collBuilder = Expr.splice(factoryExpr).newBuilder
                      val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
                      // anything but a sequence (a null included) fails as scala-yaml's collection decoders
                      val iter = YamlDerivationUtils
                        .sequenceNodes(Expr.splice(dctx.node), Expr.splice(classNameExpr))
                        .iterator
                      while (iter.hasNext)
                        decoder.construct(iter.next())(LoadSettings.empty) match {
                          case Right(item) => collBuilder += item
                          case Left(err)   =>
                            throw new YamlDerivationUtils.CollectionBuildException(err)
                        }
                      collBuilder
                    }
                    // accumulating: every item is decoded, each failure is recorded under its index
                    def readLoopAccumulating: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                      val collBuilder = Expr.splice(factoryExpr).newBuilder
                      val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
                      val iter = YamlDerivationUtils
                        .sequenceNodes(Expr.splice(dctx.node), Expr.splice(classNameExpr))
                        .iterator
                      var failures: YamlDerivationUtils.FailureAccumulator = null
                      var index = 0
                      while (iter.hasNext) {
                        decoder.construct(iter.next())(LoadSettings.empty) match {
                          case Right(item) => collBuilder += item
                          case Left(err)   => failures = YamlDerivationUtils.addItemFailure(failures, index, err)
                        }
                        index += 1
                      }
                      if (failures != null) throw new YamlDerivationUtils.CollectionBuildException(failures.toError)
                      collBuilder
                    }
                    val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] =
                      byErrorMode[scala.collection.mutable.Builder[Item, CtorResult]](
                        readLoopFailFast,
                        readLoopAccumulating
                      )
                    val buildResultExpr = buildStep.ctor(readLoop)

                    buildStep match {
                      case _: CtorLikeOf.PlainValue[?, ?] =>
                        Rule.matched(Expr.quote {
                          try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                          catch {
                            case e: YamlDerivationUtils.CollectionBuildException =>
                              Left(e.error)
                          }
                        })

                      case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                        Rule.matched(Expr.quote {
                          try
                            Expr.splice(eitherExpr) match {
                              case Right(value) => Right(value)
                              case Left(err)    => Left(ConstructError.from(err, Expr.splice(dctx.node)))
                            }
                          catch {
                            case e: YamlDerivationUtils.CollectionBuildException =>
                              Left(e.error)
                          }
                        })

                      case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                        Rule.matched(Expr.quote {
                          try
                            Expr.splice(eitherExpr) match {
                              case Right(value) => Right(value)
                              case Left(errs)   =>
                                Left(ConstructError.from(errs.mkString("\n"), Expr.splice(dctx.node)))
                            }
                          catch {
                            case e: YamlDerivationUtils.CollectionBuildException =>
                              Left(e.error)
                          }
                        })

                      case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                        Rule.matched(Expr.quote {
                          try
                            Expr.splice(eitherExpr) match {
                              case Right(value) => Right(value)
                              case Left(err)    =>
                                Left(ConstructError.from(err.getMessage, Expr.splice(dctx.node)))
                            }
                          catch {
                            case e: YamlDerivationUtils.CollectionBuildException =>
                              Left(e.error)
                          }
                        })

                      case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                        val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                        Rule.matched(Expr.quote {
                          try
                            Expr.splice(eitherExpr) match {
                              case Right(value) => Right(value)
                              case Left(errs)   =>
                                Left(
                                  ConstructError.from(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.node))
                                )
                            }
                          catch {
                            case e: YamlDerivationUtils.CollectionBuildException =>
                              Left(e.error)
                          }
                        })
                    }
                  }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }
  }
}
