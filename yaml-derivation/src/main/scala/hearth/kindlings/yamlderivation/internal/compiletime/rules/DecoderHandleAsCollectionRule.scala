package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node}
import org.virtuslab.yaml.Node.SequenceNode

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
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

                val readLoop: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
                  val collBuilder = Expr.splice(factoryExpr).newBuilder
                  val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
                  Expr.splice(dctx.node) match {
                    case SequenceNode(nodes, _) =>
                      val iter = nodes.iterator
                      while (iter.hasNext)
                        decoder.construct(iter.next())(LoadSettings.empty) match {
                          case Right(item) => collBuilder += item
                          case Left(err)   =>
                            throw new YamlDerivationUtils.CollectionBuildException(err)
                        }
                    case other =>
                      throw new YamlDerivationUtils.CollectionBuildException(
                        ConstructError.from(s"Expected sequence node but got ${other.getClass.getSimpleName}", other)
                      )
                  }
                  collBuilder
                }
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

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
