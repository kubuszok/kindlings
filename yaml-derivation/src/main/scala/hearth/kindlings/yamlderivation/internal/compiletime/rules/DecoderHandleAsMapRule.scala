package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, LoadSettings, Node}

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

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    @scala.annotation.nowarn("msg=is never used")
    private[rules] def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val EitherCEValue: Type[Either[ConstructError, Value]] = DTypes.DecoderResult[Value]
      implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]

      implicit val EitherCEKey: Type[Either[ConstructError, Key]] = DTypes.DecoderResult[Key]
      implicit val KeyFnT: Type[Node => Either[ConstructError, Key]] = Type.of[Node => Either[ConstructError, Key]]

      // `String` keys are read as scala-yaml's `YamlDecoder.forString` reads them; keys of any other type are decoded
      // with the decoder derived for them (as `YamlDecoder.forMap[K, V]` uses the key's instance).
      val keyDecodeFnMIO: MIO[Option[Expr[Node => Either[ConstructError, Key]]]] =
        if (Key <:< Type[String]) MIO.pure(None)
        else
          LambdaBuilder
            .of1[Node]("keyNode")
            .traverse { keyNodeExpr =>
              deriveDecoderRecursively[Key](using dctx.nest[Key](keyNodeExpr))
            }
            .map(builder => Some(builder.build[Either[ConstructError, Key]]))

      keyDecodeFnMIO.flatMap { keyDecodeFn =>
        LambdaBuilder
          .of1[Node]("valueNode")
          .traverse { valueNodeExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueNodeExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[ConstructError, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build
            val classNameExpr: Expr[String] = Expr(runtimeClassName[A])

            def readLoopWith(
                keyOf: Expr[Node] => Expr[Key]
            ): Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              val decoder = YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
              // anything but a mapping (a null included) fails as scala-yaml's `YamlDecoder.forMap`
              val iter = YamlDerivationUtils
                .mappingEntries(Expr.splice(dctx.node), Expr.splice(classNameExpr))
                .iterator
              while (iter.hasNext) {
                val entry = iter.next()
                val keyNode = entry._1
                val valueNode = entry._2
                val key = Expr.splice(keyOf(Expr.quote(keyNode)))
                decoder.construct(valueNode)(LoadSettings.empty) match {
                  case Right(v) =>
                    mapBuilder += Expr.splice(isMap.pair(Expr.quote(key), Expr.quote(v)))
                  case Left(err) =>
                    throw new YamlDerivationUtils.CollectionBuildException(err)
                }
              }
              mapBuilder
            }
            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = keyDecodeFn match {
              case None =>
                readLoopWith(keyNode =>
                  Expr.quote(YamlDerivationUtils.stringKey(Expr.splice(keyNode)).asInstanceOf[Key])
                )
              case Some(fn) =>
                ValDefs
                  .createVal[Node => Either[ConstructError, Key]](fn, FreshName.FromPrefix("decodeKey"))
                  .use { decodeKey =>
                    readLoopWith(keyNode =>
                      Expr.quote(YamlDerivationUtils.decodeKeyWith(Expr.splice(keyNode), Expr.splice(decodeKey)))
                    )
                  }
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
