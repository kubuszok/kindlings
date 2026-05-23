package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node}
import org.virtuslab.yaml.Node.{MappingNode, ScalarNode}

trait DecoderHandleAsMapRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
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
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEValue: Type[Either[ConstructError, Value]] = DTypes.DecoderResult[Value]
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Node]("valueNode")
          .traverse { valueNodeExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueNodeExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConstructError, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
              Expr.splice(dctx.node) match {
                case MappingNode(mappings, _) =>
                  val iter = mappings.iterator
                  while (iter.hasNext) {
                    val (keyNode, valueNode) = iter.next()
                    val key = keyNode match {
                      case ScalarNode(value, _) => value
                      case other                =>
                        throw new YamlDerivationUtils.CollectionBuildException(
                          ConstructError.from(
                            s"Expected scalar key but got ${other.getClass.getSimpleName}",
                            other
                          )
                        )
                    }
                    decoder.construct(valueNode)(LoadSettings.empty) match {
                      case Right(v) =>
                        mapBuilder += Expr.splice(
                          isMap.pair(Expr.quote(key.asInstanceOf[Key]), Expr.quote(v))
                        )
                      case Left(err) =>
                        throw new YamlDerivationUtils.CollectionBuildException(err)
                    }
                  }
                case other =>
                  throw new YamlDerivationUtils.CollectionBuildException(
                    ConstructError.from(
                      s"Expected mapping node but got ${other.getClass.getSimpleName}",
                      other
                    )
                  )
              }
              mapBuilder
            }
            val buildResultExpr = buildStep.ctor(readLoop)

            buildStep match {
              case _: CtorLikeOf.PlainValue[?, ?] =>
                Rule.matched(Expr.quote {
                  try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                  catch {
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
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
                    case e: YamlDerivationUtils.CollectionBuildException => Left(e.error)
                  }
                })
            }
          }
      }
    }
  }
}
