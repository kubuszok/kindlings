package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsMapRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
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
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val EitherDFValue: Type[Either[DecodingFailure, Value]] = DTypes.DecoderResult[Value]
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]

      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[HCursor]("valueCursor")
          .traverse { valueCursorExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[DecodingFailure, Value]]
            val factoryExpr = isMap.factory
            val buildStep = isMap.build

            val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
              val cursor = Expr.splice(dctx.cursor)
              val decoder = CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
              val mapBuilder = Expr.splice(factoryExpr).newBuilder
              cursor.keys match {
                case None =>
                  throw new CirceDerivationUtils.CollectionBuildException(
                    DecodingFailure("Expected JSON object", cursor.history)
                  )
                case Some(keys) =>
                  val iter = keys.iterator
                  while (iter.hasNext) {
                    val key = iter.next()
                    cursor.downField(key).as(decoder) match {
                      case Right(v) =>
                        mapBuilder += Expr.splice(
                          isMap.pair(Expr.quote(key.asInstanceOf[Key]), Expr.quote(v))
                        )
                      case Left(err) =>
                        throw new CirceDerivationUtils.CollectionBuildException(err)
                    }
                  }
              }
              mapBuilder
            }
            val buildResultExpr = buildStep.ctor(readLoop)

            buildStep match {
              case _: CtorLikeOf.PlainValue[?, ?] =>
                Rule.matched(Expr.quote {
                  try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                  catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                })
              case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(err)    => Left(DecodingFailure(err, Expr.splice(dctx.cursor).history))
                    }
                  catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                })
              case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(errs)   => Left(DecodingFailure(errs.mkString("\n"), Expr.splice(dctx.cursor).history))
                    }
                  catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                })
              case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(err)    => Left(DecodingFailure(err.getMessage, Expr.splice(dctx.cursor).history))
                    }
                  catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                })
              case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                Rule.matched(Expr.quote {
                  try
                    Expr.splice(eitherExpr) match {
                      case Right(value) => Right(value)
                      case Left(errs)   =>
                        Left(DecodingFailure(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.cursor).history))
                    }
                  catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                })
            }
          }
      } else {
        deriveKeyDecoder[Key].flatMap {
          case Some(keyDecoderLambda) =>
            LambdaBuilder
              .of1[HCursor]("valueCursor")
              .traverse { valueCursorExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[DecodingFailure, Value]]
                val factoryExpr = isMap.factory
                val buildStep = isMap.build

                val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
                  val cursor = Expr.splice(dctx.cursor)
                  val decoder = CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn))
                  val keyDecoder = Expr.splice(keyDecoderLambda)
                  val mapBuilder = Expr.splice(factoryExpr).newBuilder
                  cursor.keys match {
                    case None =>
                      throw new CirceDerivationUtils.CollectionBuildException(
                        DecodingFailure("Expected JSON object", cursor.history)
                      )
                    case Some(keys) =>
                      val iter = keys.iterator
                      while (iter.hasNext) {
                        val keyStr = iter.next()
                        keyDecoder(keyStr) match {
                          case Right(key) =>
                            cursor.downField(keyStr).as(decoder) match {
                              case Right(v) =>
                                mapBuilder += Expr.splice(isMap.pair(Expr.quote(key), Expr.quote(v)))
                              case Left(err) =>
                                throw new CirceDerivationUtils.CollectionBuildException(err)
                            }
                          case Left(err) =>
                            throw new CirceDerivationUtils.CollectionBuildException(err)
                        }
                      }
                  }
                  mapBuilder
                }
                val buildResultExpr = buildStep.ctor(readLoop)

                buildStep match {
                  case _: CtorLikeOf.PlainValue[?, ?] =>
                    Rule.matched(Expr.quote {
                      try Right(Expr.splice(buildResultExpr.asInstanceOf[Expr[A]]))
                      catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                    })
                  case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[String, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    => Left(DecodingFailure(err, Expr.splice(dctx.cursor).history))
                        }
                      catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                    })
                  case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[String], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(DecodingFailure(errs.mkString("\n"), Expr.splice(dctx.cursor).history))
                        }
                      catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                    })
                  case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Throwable, A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(err)    => Left(DecodingFailure(err.getMessage, Expr.splice(dctx.cursor).history))
                        }
                      catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                    })
                  case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
                    val eitherExpr = buildResultExpr.asInstanceOf[Expr[Either[Iterable[Throwable], A]]]
                    Rule.matched(Expr.quote {
                      try
                        Expr.splice(eitherExpr) match {
                          case Right(value) => Right(value)
                          case Left(errs)   =>
                            Left(
                              DecodingFailure(errs.map(_.getMessage).mkString("\n"), Expr.splice(dctx.cursor).history)
                            )
                        }
                      catch { case e: CirceDerivationUtils.CollectionBuildException => Left(e.failure) }
                    })
                }
              }
          case None =>
            MIO.pure(
              Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived")
            )
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyDecoder[K: Type](implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[String => Either[DecodingFailure, K]]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val EitherDFK: Type[Either[DecodingFailure, K]] = DTypes.DecoderResult[K]

      Log.info(s"Attempting to derive key decoder for ${Type[K].prettyPrint}") >> {
        val builtIn: Option[Expr[String => Either[DecodingFailure, K]]] =
          if (Type[K] =:= Type.of[Int])
            Some(Expr.quote { (s: String) =>
              CirceDerivationUtils.decodeKeyInt(s).asInstanceOf[Either[DecodingFailure, K]]
            })
          else if (Type[K] =:= Type.of[Long])
            Some(Expr.quote { (s: String) =>
              CirceDerivationUtils.decodeKeyLong(s).asInstanceOf[Either[DecodingFailure, K]]
            })
          else if (Type[K] =:= Type.of[Double])
            Some(Expr.quote { (s: String) =>
              CirceDerivationUtils.decodeKeyDouble(s).asInstanceOf[Either[DecodingFailure, K]]
            })
          else if (Type[K] =:= Type.of[Short])
            Some(Expr.quote { (s: String) =>
              CirceDerivationUtils.decodeKeyShort(s).asInstanceOf[Either[DecodingFailure, K]]
            })
          else if (Type[K] =:= Type.of[Byte])
            Some(Expr.quote { (s: String) =>
              CirceDerivationUtils.decodeKeyByte(s).asInstanceOf[Either[DecodingFailure, K]]
            })
          else None

        builtIn.map(fn => MIO.pure(Some(fn): Option[Expr[String => Either[DecodingFailure, K]]])).getOrElse {
          DTypes.KeyDecoder[K].summonExprIgnoring().toEither match {
            case Right(keyDecoderExpr) =>
              Log.info(s"Found implicit KeyDecoder[${Type[K].prettyPrint}]") >>
                MIO.pure(
                  Some(Expr.quote { (s: String) =>
                    Expr.splice(keyDecoderExpr).apply(s) match {
                      case Some(k) => Right(k): Either[DecodingFailure, K]
                      case None    =>
                        Left(
                          DecodingFailure("Failed to decode map key: " + s, Nil)
                        ): Either[DecodingFailure, K]
                    }
                  }): Option[Expr[String => Either[DecodingFailure, K]]]
                )
            case Left(_) =>
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyDecoder[Inner].map {
                    case Some(innerKeyDecoder) =>
                      Some(buildValueTypeKeyDecoder[K, Inner](isValueType.value, innerKeyDecoder))
                    case None => None
                  }
                case _ =>
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        NonEmptyList.fromList(childrenList) match {
                          case Some(children) =>
                            children
                              .parTraverse { case (childName, child) =>
                                import child.Underlying as ChildType
                                SingletonValue.unapply(Type[ChildType]) match {
                                  case Some(sv) =>
                                    MIO.pure((childName, sv.singletonExpr.asInstanceOf[Expr[K]]))
                                  case None =>
                                    CaseClass.parse[ChildType].toOption match {
                                      case Some(cc) =>
                                        cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                                          def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                                            MIO.fail(new RuntimeException("Unexpected parameter in enum singleton"))
                                        }).flatMap {
                                          case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[K]]))
                                          case None       =>
                                            MIO.fail(new RuntimeException(s"Cannot construct enum case $childName"))
                                        }
                                      case None =>
                                        MIO.fail(new RuntimeException(s"Cannot construct enum case $childName"))
                                    }
                                }
                              }
                              .flatMap { casesNel =>
                                val lookupMapExpr: Expr[Map[String, K]] = casesNel.toList.foldRight(
                                  Expr.quote(Map.empty[String, K])
                                ) { case ((caseName, caseExpr), acc) =>
                                  Expr.quote {
                                    Expr.splice(acc) + (
                                      Expr.splice(ctx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                                        -> Expr.splice(caseExpr)
                                    )
                                  }
                                }
                                MIO.pure(
                                  Some(Expr.quote { (s: String) =>
                                    CirceDerivationUtils.decodeEnumKey[K](s, Expr.splice(lookupMapExpr))
                                  }): Option[Expr[String => Either[DecodingFailure, K]]]
                                )
                              }
                          case None => MIO.pure(None)
                        }
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
    }

    private def buildValueTypeKeyDecoder[K: Type, Inner: Type](
        isValueType: IsValueTypeOf[K, Inner],
        innerKeyDecoder: Expr[String => Either[DecodingFailure, Inner]]
    ): Expr[String => Either[DecodingFailure, K]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherDFK: Type[Either[DecodingFailure, K]] = DTypes.DecoderResult[K]
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherDFInner: Type[Either[DecodingFailure, Inner]] = DTypes.DecoderResult[Inner]

      isValueType.wrap match {
        case _: CtorLikeOf.PlainValue[?, ?] =>
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).map { (inner: Inner) =>
              Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[K]])
            }
          }
        case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).flatMap { (inner: Inner) =>
              Expr
                .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, K]]])
                .left
                .map((msg: String) => io.circe.DecodingFailure(msg, Nil))
            }
          }
        case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).flatMap { (inner: Inner) =>
              Expr
                .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[String], K]]])
                .left
                .map((errs: Iterable[String]) => io.circe.DecodingFailure(errs.mkString("\n"), Nil))
            }
          }
        case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).flatMap { (inner: Inner) =>
              Expr
                .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Throwable, K]]])
                .left
                .map((err: Throwable) => io.circe.DecodingFailure(err.getMessage, Nil))
            }
          }
        case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).flatMap { (inner: Inner) =>
              Expr
                .splice(
                  isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[Iterable[Throwable], K]]]
                )
                .left
                .map((errs: Iterable[Throwable]) =>
                  io.circe.DecodingFailure(errs.map(_.getMessage).mkString("\n"), Nil)
                )
            }
          }
      }
    }
  }
}
