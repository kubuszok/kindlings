package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

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

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    private[rules] def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = CTypes.String
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      val nullAsEmptyMap = DecoderHandleAsCollectionRule.nullAsEmpty[A, Pair](isMap)
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig

      // String keys use the inlined fast path unless a user-provided JsonKeyCodec[String] exists.
      if (Key =:= Type[String] && CTypes.JsonKeyCodec[Key].summonExprIgnoring().toEither.isLeft) {
        val mapAsArrayStaticallyFalse = dctx.evaluatedConfig.exists(!_.mapAsArray)
        val mapAsArrayStaticallyTrue = dctx.evaluatedConfig.exists(_.mapAsArray)
        if (mapAsArrayStaticallyFalse)
          decodeStringKeyedMapInline[A, Pair, Key, Value](isMap)
        else if (mapAsArrayStaticallyTrue)
          decodeStringKeyedMapAsArrayInline[A, Pair, Key, Value](isMap)
        else
          decodeStringKeyedMapViaHelper[A, Pair, Key, Value](isMap)
      } else {
        // Non-String keys — try to derive key decoding for object mode
        deriveKeyDecoding[Key].flatMap {
          case Some(keyDecoderLambda) =>
            // Derive value decoder
            LambdaBuilder
              .of1[JsonReader]("valueReader")
              .traverse { valueReaderExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
              }
              .flatMap { valueBuilder =>
                val decodeFn = valueBuilder.build[Value]
                // Derive key-as-value decoder for mapAsArray mode
                LambdaBuilder
                  .of1[JsonReader]("keyValueReader")
                  .traverse { keyReaderExpr =>
                    deriveDecoderRecursively[Key](using dctx.nest[Key](keyReaderExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueDecodeFn = keyValueBuilder.build[Key]
                    val factoryExpr = isMap.factory
                    val buildStep = isMap.build

                    val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
                      val decodeFnVal = Expr.splice(decodeFn)
                      val keyDecodeFnVal = Expr.splice(keyValueDecodeFn)
                      val keyDecoderVal = Expr.splice(keyDecoderLambda)
                      val reader = Expr.splice(dctx.reader)
                      val maxInserts = Expr.splice(dctx.config).mapMaxInsertNumber
                      val mapBuilder = Expr.splice(factoryExpr).newBuilder
                      if (Expr.splice(dctx.config).mapAsArray) {
                        if (
                          JsoniterDerivationUtils.openArray(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader
                            .isNextToken(']'.toByte)
                        ) {
                          reader.rollbackToken()
                          var count = 0
                          var continueLoop = true
                          while (continueLoop) {
                            count += 1
                            if (count > maxInserts)
                              reader.decodeError("too many map inserts")
                            JsoniterDerivationUtils.openPair(reader)
                            val key = keyDecodeFnVal(reader)
                            JsoniterDerivationUtils.pairSeparator(reader)
                            val value = decodeFnVal(reader)
                            JsoniterDerivationUtils.closePair(reader)
                            mapBuilder += Expr.splice(
                              isMap.pair(
                                Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                                Expr.quote(value).asInstanceOf[Expr[isMap.Value]]
                              )
                            )
                            continueLoop = reader.isNextToken(','.toByte)
                          }
                          if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
                        }
                      } else {
                        if (
                          JsoniterDerivationUtils.openObject(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader
                            .isNextToken('}'.toByte)
                        ) {
                          reader.rollbackToken()
                          var count = 0
                          var continueLoop = true
                          while (continueLoop) {
                            count += 1
                            if (count > maxInserts)
                              reader.decodeError("too many map inserts")
                            val key = keyDecoderVal(reader)
                            val value = decodeFnVal(reader)
                            mapBuilder += Expr.splice(
                              isMap.pair(
                                Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                                Expr.quote(value).asInstanceOf[Expr[isMap.Value]]
                              )
                            )
                            continueLoop = reader.isNextToken(','.toByte)
                          }
                          if (!reader.isCurrentToken('}'.toByte)) reader.decodeError("expected '}' or ','")
                        }
                      }
                      mapBuilder
                    }
                    val buildResultExpr = buildStep.ctor(readLoop)

                    buildStep match {
                      case _: CtorLikeOf.PlainValue[?, ?] =>
                        Rule.matched(Expr.quote {
                          import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
                          if (Expr.splice(dctx.config).mapAsArray) {
                            JsoniterDerivationUtils
                              .readMapAsArray[Key, Value, A](
                                Expr.splice(dctx.reader),
                                Expr.splice(keyValueDecodeFn),
                                Expr.splice(decodeFn),
                                Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                                Expr.splice(dctx.config).mapMaxInsertNumber
                              )
                              .asInstanceOf[A]
                          } else {
                            JsoniterDerivationUtils
                              .readMapWithKeyDecoder[Key, Value, A](
                                Expr.splice(dctx.reader),
                                Expr.splice(keyDecoderLambda),
                                Expr.splice(decodeFn),
                                Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                                Expr.splice(dctx.config).mapMaxInsertNumber
                              )
                              .asInstanceOf[A]
                          }
                        })
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
                            case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
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
                              else Expr.splice(dctx.reader).decodeError("map construction failed")
                          }
                        })
                    }
                  }
              }
          case None if dctx.evaluatedConfig.exists(!_.mapAsArray) =>
            val err = CodecDerivationError.UnsupportedMapKey(Key.prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
          case None =>
            // No key decoder — mapAsArray-only support (the config is only known at runtime)
            LambdaBuilder
              .of1[JsonReader]("valueReader")
              .traverse { valueReaderExpr =>
                deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
              }
              .flatMap { valueBuilder =>
                val decodeFn = valueBuilder.build[Value]
                LambdaBuilder
                  .of1[JsonReader]("keyValueReader")
                  .traverse { keyReaderExpr =>
                    deriveDecoderRecursively[Key](using dctx.nest[Key](keyReaderExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueDecodeFn = keyValueBuilder.build[Key]
                    val factoryExpr = isMap.factory
                    val buildStep = isMap.build

                    val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
                      val decodeFnVal = Expr.splice(decodeFn)
                      val keyDecodeFnVal = Expr.splice(keyValueDecodeFn)
                      val reader = Expr.splice(dctx.reader)
                      val maxInserts = Expr.splice(dctx.config).mapMaxInsertNumber
                      val mapBuilder = Expr.splice(factoryExpr).newBuilder
                      if (Expr.splice(dctx.config).mapAsArray) {
                        if (
                          JsoniterDerivationUtils.openArray(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader
                            .isNextToken(']'.toByte)
                        ) {
                          reader.rollbackToken()
                          var count = 0
                          var continueLoop = true
                          while (continueLoop) {
                            count += 1
                            if (count > maxInserts)
                              reader.decodeError("too many map inserts")
                            JsoniterDerivationUtils.openPair(reader)
                            val key = keyDecodeFnVal(reader)
                            JsoniterDerivationUtils.pairSeparator(reader)
                            val value = decodeFnVal(reader)
                            JsoniterDerivationUtils.closePair(reader)
                            mapBuilder += Expr.splice(
                              isMap.pair(
                                Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                                Expr.quote(value).asInstanceOf[Expr[isMap.Value]]
                              )
                            )
                            continueLoop = reader.isNextToken(','.toByte)
                          }
                          if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
                        }
                      } else {
                        throw new IllegalArgumentException(
                          "Map key type " + Expr.splice(Expr(Key.prettyPrint)) +
                            " cannot be used as JSON object key. Use mapAsArray config option."
                        )
                      }
                      mapBuilder
                    }
                    val buildResultExpr = buildStep.ctor(readLoop)

                    buildStep match {
                      case _: CtorLikeOf.PlainValue[?, ?] =>
                        Rule.matched(Expr.quote {
                          import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
                          if (Expr.splice(dctx.config).mapAsArray) {
                            JsoniterDerivationUtils
                              .readMapAsArray[Key, Value, A](
                                Expr.splice(dctx.reader),
                                Expr.splice(keyValueDecodeFn),
                                Expr.splice(decodeFn),
                                Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                                Expr.splice(dctx.config).mapMaxInsertNumber
                              )
                              .asInstanceOf[A]
                          } else {
                            throw new IllegalArgumentException(
                              "Map key type " + Expr.splice(Expr(Key.prettyPrint)) +
                                " cannot be used as JSON object key. Use mapAsArray config option."
                            )
                          }
                        })
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
                            case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
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
                              else Expr.splice(dctx.reader).decodeError("map construction failed")
                          }
                        })
                    }
                  }
              }
              .recoverWith { _ =>
                MIO.pure(
                  Rule.yielded(
                    s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived"
                  )
                )
              }
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeStringKeyedMapInline[A: DecoderCtx, Pair: Type, Key: Type, Value: Type](
        isMap: IsMapOf[A, Pair]
    )(implicit
        StringT: Type[String],
        JsonReaderT: Type[JsonReader],
        JsoniterConfigT: Type[JsoniterConfig]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      val nullAsEmptyMap = DecoderHandleAsCollectionRule.nullAsEmpty[A, Pair](isMap)
      @scala.annotation.nowarn("msg=is never used")
      implicit val IntT: Type[Int] = CTypes.Int
      import isMap.CtorResult
      val maxInserts = dctx.evaluatedConfig.get.mapMaxInsertNumber
      deriveDecoderRecursively[Value](using dctx.nest[Value](dctx.reader)).map { decodeValueExpr =>
        val factoryExpr = isMap.factory
        val buildStep = isMap.build

        val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
          val reader = Expr.splice(dctx.reader)
          val mapBuilder = Expr.splice(factoryExpr).newBuilder
          if (
            JsoniterDerivationUtils
              .openObject(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader.isNextToken('}'.toByte)
          ) {
            reader.rollbackToken()
            var count: Int = 1
            val key = reader.readKeyAsString()
            mapBuilder += Expr.splice(
              isMap.pair(
                Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                decodeValueExpr.asInstanceOf[Expr[isMap.Value]]
              )
            )
            while (reader.isNextToken(','.toByte)) {
              count += 1
              if (count > Expr.splice(Expr(maxInserts)))
                reader.decodeError("too many map inserts")
              val k = reader.readKeyAsString()
              mapBuilder += Expr.splice(
                isMap.pair(
                  Expr.quote(k).asInstanceOf[Expr[isMap.Key]],
                  decodeValueExpr.asInstanceOf[Expr[isMap.Value]]
                )
              )
            }
            if (!reader.isCurrentToken('}'.toByte)) reader.decodeError("expected '}' or ','")
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
                case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
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
                  else Expr.splice(dctx.reader).decodeError("map construction failed")
              }
            })
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeStringKeyedMapAsArrayInline[A: DecoderCtx, Pair: Type, Key: Type, Value: Type](
        isMap: IsMapOf[A, Pair]
    )(implicit
        StringT: Type[String],
        JsonReaderT: Type[JsonReader],
        JsoniterConfigT: Type[JsoniterConfig]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      val nullAsEmptyMap = DecoderHandleAsCollectionRule.nullAsEmpty[A, Pair](isMap)
      @scala.annotation.nowarn("msg=is never used")
      implicit val IntT: Type[Int] = CTypes.Int
      import isMap.CtorResult
      val maxInserts = dctx.evaluatedConfig.get.mapMaxInsertNumber
      deriveDecoderRecursively[Value](using dctx.nest[Value](dctx.reader)).map { decodeValueExpr =>
        val factoryExpr = isMap.factory
        val buildStep = isMap.build

        val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
          val reader = Expr.splice(dctx.reader)
          val mapBuilder = Expr.splice(factoryExpr).newBuilder
          if (
            JsoniterDerivationUtils
              .openArray(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader.isNextToken(']'.toByte)
          ) {
            reader.rollbackToken()
            var count: Int = 1
            JsoniterDerivationUtils.openPair(reader)
            val key = reader.readString(null)
            JsoniterDerivationUtils.pairSeparator(reader)
            mapBuilder += Expr.splice(
              isMap.pair(
                Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                decodeValueExpr.asInstanceOf[Expr[isMap.Value]]
              )
            )
            JsoniterDerivationUtils.closePair(reader)
            while (reader.isNextToken(','.toByte)) {
              count += 1
              if (count > Expr.splice(Expr(maxInserts)))
                reader.decodeError("too many map inserts")
              JsoniterDerivationUtils.openPair(reader)
              val k = reader.readString(null)
              JsoniterDerivationUtils.pairSeparator(reader)
              mapBuilder += Expr.splice(
                isMap.pair(
                  Expr.quote(k).asInstanceOf[Expr[isMap.Key]],
                  decodeValueExpr.asInstanceOf[Expr[isMap.Value]]
                )
              )
              JsoniterDerivationUtils.closePair(reader)
            }
            if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
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
                case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
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
                  else Expr.splice(dctx.reader).decodeError("map construction failed")
              }
            })
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeStringKeyedMapViaHelper[A: DecoderCtx, Pair: Type, Key: Type, Value: Type](
        isMap: IsMapOf[A, Pair]
    )(implicit
        StringT: Type[String],
        JsonReaderT: Type[JsonReader],
        JsoniterConfigT: Type[JsoniterConfig]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      val nullAsEmptyMap = DecoderHandleAsCollectionRule.nullAsEmpty[A, Pair](isMap)
      import isMap.CtorResult
      LambdaBuilder
        .of1[JsonReader]("valueReader")
        .traverse { valueReaderExpr =>
          deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
        }
        .flatMap { valueBuilder =>
          val decodeFn = valueBuilder.build[Value]
          LambdaBuilder
            .of1[JsonReader]("keyValueReader")
            .traverse { keyReaderExpr =>
              MIO.pure(Expr.quote {
                Expr.splice(keyReaderExpr).readString(null).asInstanceOf[Key]
              })
            }
            .map { keyValueBuilder =>
              val keyValueDecodeFn = keyValueBuilder.build[Key]
              val factoryExpr = isMap.factory
              val buildStep = isMap.build

              val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
                val decodeFnVal = Expr.splice(decodeFn)
                val keyDecodeFnVal = Expr.splice(keyValueDecodeFn)
                val reader = Expr.splice(dctx.reader)
                val maxInserts = Expr.splice(dctx.config).mapMaxInsertNumber
                val mapBuilder = Expr.splice(factoryExpr).newBuilder
                if (Expr.splice(dctx.config).mapAsArray) {
                  if (
                    JsoniterDerivationUtils
                      .openArray(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader.isNextToken(']'.toByte)
                  ) {
                    reader.rollbackToken()
                    var count = 0
                    var continueLoop = true
                    while (continueLoop) {
                      count += 1
                      if (count > maxInserts) reader.decodeError("too many map inserts")
                      JsoniterDerivationUtils.openPair(reader)
                      val key = keyDecodeFnVal(reader)
                      JsoniterDerivationUtils.pairSeparator(reader)
                      val value = decodeFnVal(reader)
                      JsoniterDerivationUtils.closePair(reader)
                      mapBuilder += Expr.splice(
                        isMap.pair(
                          Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                          Expr.quote(value).asInstanceOf[Expr[isMap.Value]]
                        )
                      )
                      continueLoop = reader.isNextToken(','.toByte)
                    }
                    if (!reader.isCurrentToken(']'.toByte)) reader.decodeError("expected ']' or ','")
                  }
                } else {
                  if (
                    JsoniterDerivationUtils.openObject(reader, Expr.splice(Expr(nullAsEmptyMap))) && !reader
                      .isNextToken('}'.toByte)
                  ) {
                    reader.rollbackToken()
                    var count = 0
                    var continueLoop = true
                    while (continueLoop) {
                      count += 1
                      if (count > maxInserts) reader.decodeError("too many map inserts")
                      val key = reader.readKeyAsString()
                      mapBuilder += Expr.splice(
                        isMap.pair(
                          Expr.quote(key).asInstanceOf[Expr[isMap.Key]],
                          Expr.quote(decodeFnVal(reader)).asInstanceOf[Expr[isMap.Value]]
                        )
                      )
                      continueLoop = reader.isNextToken(','.toByte)
                    }
                    if (!reader.isCurrentToken('}'.toByte)) reader.decodeError("expected '}' or ','")
                  }
                }
                mapBuilder
              }
              val buildResultExpr = buildStep.ctor(readLoop)

              buildStep match {
                case _: CtorLikeOf.PlainValue[?, ?] =>
                  Rule.matched(Expr.quote {
                    import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
                    if (Expr.splice(dctx.config).mapAsArray) {
                      JsoniterDerivationUtils
                        .readMapAsArray[Key, Value, A](
                          Expr.splice(dctx.reader),
                          Expr.splice(keyValueDecodeFn),
                          Expr.splice(decodeFn),
                          Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                          Expr.splice(dctx.config).mapMaxInsertNumber
                        )
                        .asInstanceOf[A]
                    } else {
                      JsoniterDerivationUtils
                        .readMap[Value, A](
                          Expr.splice(dctx.reader),
                          Expr.splice(decodeFn),
                          Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), A]],
                          Expr.splice(dctx.config).mapMaxInsertNumber
                        )
                        .asInstanceOf[A]
                    }
                  })
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
                      case Left(errs)   => Expr.splice(dctx.reader).decodeError(errs.mkString("\n"))
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
                        else Expr.splice(dctx.reader).decodeError("map construction failed")
                    }
                  })
              }
            }
        }
    }

    /** Try to derive a JsonReader => K function for map key decoding. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def deriveKeyDecoding[K: Type](implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[JsonReader => K]]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val StringT: Type[String] = CTypes.String

      Log.info(s"Attempting to derive key decoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types with native readKeyAs* overloads
        val builtIn: Option[MIO[Option[Expr[JsonReader => K]]]] = {
          def makeKeyDecoder(body: Expr[JsonReader] => Expr[K]): MIO[Option[Expr[JsonReader => K]]] =
            LambdaBuilder
              .of1[JsonReader]("reader")
              .traverse { readerExpr =>
                MIO.pure(body(readerExpr))
              }
              .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])

          val literal = literalKey[K]
          if (literal.isDefined) {
            val (keyString, keyValue) = literal.get
            val error = "expected key: \"" + keyString + "\""
            Some(makeKeyDecoder { r =>
              Expr.quote {
                if (Expr.splice(r).readKeyAsString() != Expr.splice(Expr(keyString)))
                  Expr.splice(r).decodeError(Expr.splice(Expr(error)))
                Expr.splice(keyValue)
              }
            })
          } else if (Type[K] =:= Type.of[String])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsString().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Int])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsInt().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Byte])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsByte().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Char])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsChar().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.JavaCharacter)
            Some(
              makeKeyDecoder(r =>
                Expr.quote(java.lang.Character.valueOf(Expr.splice(r).readKeyAsChar()).asInstanceOf[K])
              )
            )
          else if (Type[K] =:= CTypes.UUID)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsUUID().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.Duration)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsDuration().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.Instant)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsInstant().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.LocalDate)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsLocalDate().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.LocalDateTime)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsLocalDateTime().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.LocalTime)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsLocalTime().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.MonthDay)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsMonthDay().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.OffsetDateTime)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsOffsetDateTime().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.OffsetTime)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsOffsetTime().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.Period)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsPeriod().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.Year)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsYear().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.YearMonth)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsYearMonth().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.ZonedDateTime)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsZonedDateTime().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.ZoneId)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsZoneId().asInstanceOf[K])))
          else if (Type[K] =:= CTypes.ZoneOffset)
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsZoneOffset().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Long])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsLong().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Double])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsDouble().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Float])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsFloat().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Short])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsShort().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[Boolean])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBoolean().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[BigDecimal])
            Some(makeKeyDecoder { r =>
              Expr.quote {
                val config = Expr.splice(ctx.config)
                Expr
                  .splice(r)
                  .readKeyAsBigDecimal(
                    JsoniterDerivationUtils.mathContext(config.bigDecimalPrecision),
                    config.bigDecimalScaleLimit,
                    config.bigDecimalDigitsLimit
                  )
                  .asInstanceOf[K]
              }
            })
          else if (Type[K] =:= Type.of[BigInt])
            Some(makeKeyDecoder { r =>
              Expr.quote(Expr.splice(r).readKeyAsBigInt(Expr.splice(ctx.config).bigIntDigitsLimit).asInstanceOf[K])
            })
          // Java boxed primitives — decode key as primitive, then box
          else if (Type[K] =:= CTypes.JavaByte)
            Some(
              makeKeyDecoder(r => Expr.quote(java.lang.Byte.valueOf(Expr.splice(r).readKeyAsByte()).asInstanceOf[K]))
            )
          else if (Type[K] =:= CTypes.JavaShort)
            Some(
              makeKeyDecoder(r => Expr.quote(java.lang.Short.valueOf(Expr.splice(r).readKeyAsShort()).asInstanceOf[K]))
            )
          else if (Type[K] =:= CTypes.JavaInteger)
            Some(
              makeKeyDecoder(r => Expr.quote(java.lang.Integer.valueOf(Expr.splice(r).readKeyAsInt()).asInstanceOf[K]))
            )
          else if (Type[K] =:= CTypes.JavaLong)
            Some(
              makeKeyDecoder(r => Expr.quote(java.lang.Long.valueOf(Expr.splice(r).readKeyAsLong()).asInstanceOf[K]))
            )
          else if (Type[K] =:= CTypes.JavaFloat)
            Some(
              makeKeyDecoder(r => Expr.quote(java.lang.Float.valueOf(Expr.splice(r).readKeyAsFloat()).asInstanceOf[K]))
            )
          else if (Type[K] =:= CTypes.JavaDouble)
            Some(
              makeKeyDecoder(r =>
                Expr.quote(java.lang.Double.valueOf(Expr.splice(r).readKeyAsDouble()).asInstanceOf[K])
              )
            )
          else if (Type[K] =:= CTypes.JavaBoolean)
            Some(
              makeKeyDecoder(r =>
                Expr.quote(java.lang.Boolean.valueOf(Expr.splice(r).readKeyAsBoolean()).asInstanceOf[K])
              )
            )
          else
            None
        }

        // As in jsoniter-scala, a user-provided JsonKeyCodec takes precedence over the built-in key types.
        val hasUserKeyCodec = CTypes.JsonKeyCodec[K].summonExprIgnoring().toEither.isRight
        (if (hasUserKeyCodec) None else builtIn).getOrElse {
          // 2. Try summoning user-provided JsonKeyCodec[K]
          CTypes.JsonKeyCodec[K].summonExprIgnoring().toEither match {
            case Right(keyCodecExpr) =>
              Log.info(s"Found implicit JsonKeyCodec[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[JsonReader]("reader")
                  .traverse { readerExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyCodecExpr).decodeKey(Expr.splice(readerExpr))
                    })
                  }
                  .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
            case Left(_) =>
              // 3. Value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  // Build wrap lambda outside quotes
                  // For EitherStringOrValue wraps, handle Either and throw on Left
                  @scala.annotation.nowarn("msg=is never used")
                  def buildWrapLambda() =
                    LambdaBuilder
                      .of1[Inner]("inner")
                      .traverse { innerExpr =>
                        isValueType.value.wrap match {
                          case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                            val wrapResult =
                              isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, K]]]
                            MIO.pure(Expr.quote {
                              Expr.splice(wrapResult) match {
                                case scala.Right(v)  => v
                                case scala.Left(msg) => Expr.splice(ctx.reader).decodeError(msg)
                              }
                            })
                          case _ =>
                            MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[K]])
                        }
                      }
                  buildWrapLambda()
                    .flatMap { wrapBuilder =>
                      val wrapLambda = wrapBuilder.build[K]
                      deriveKeyDecoding[Inner].flatMap {
                        case Some(innerKeyDecoder) =>
                          LambdaBuilder
                            .of1[JsonReader]("reader")
                            .traverse { readerExpr =>
                              MIO.pure(Expr.quote {
                                Expr
                                  .splice(wrapLambda)
                                  .apply(Expr.splice(innerKeyDecoder).apply(Expr.splice(readerExpr)))
                              })
                            }
                            .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
                        case None => MIO.pure(None)
                      }
                    }
                case _
                    if !ctx.evaluatedConfig
                      .exists(!_.inlineOneValueClasses) && oneValueClassField[K](ctx.evaluatedConfig).isDefined =>
                  // 3b. inlineOneValueClasses: a one-value class key is its field's key (as jsoniter-scala)
                  val (product, fName, param) = oneValueClassField[K](ctx.evaluatedConfig).get
                  import param.tpe.Underlying as Inner
                  deriveKeyDecoding[Inner].flatMap {
                    case Some(innerKeyDecoder) =>
                      LambdaBuilder
                        .of1[JsonReader]("reader")
                        .traverse { readerExpr =>
                          val inner: Expr[Inner] =
                            Expr.quote(Expr.splice(innerKeyDecoder).apply(Expr.splice(readerExpr)))
                          constructOneValueClass[K](product, fName, inner.as_??) match {
                            case Right(constructed) => MIO.pure(constructed)
                            case Left(error)        =>
                              MIO.fail(
                                CodecDerivationError.CannotConstructType(Type[K].prettyPrint, false, Some(error))
                              )
                          }
                        }
                        .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Enum (all case objects) — read key as string, match against case names
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || isJavaEnumType[K] || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        NonEmptyList.fromList(childrenList) match {
                          case None           => MIO.pure(None)
                          case Some(children) =>
                            // Build a Map[String, K] lookup expression from singletons, then use it
                            // inside the lambda to dispatch readKeyAsString() results.
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
                                            MIO.fail(
                                              CodecDerivationError
                                                .UnexpectedParameterInSingleton(
                                                  childName,
                                                  "Unexpected parameter in enum singleton"
                                                )
                                            )
                                        }).flatMap {
                                          case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[K]]))
                                          case None       =>
                                            val err =
                                              CodecDerivationError.CannotConstructType(childName, isSingleton = true)
                                            Log.error(err.message) >> MIO.fail(err)
                                        }
                                      case None =>
                                        val err =
                                          CodecDerivationError.CannotConstructType(childName, isSingleton = true)
                                        Log.error(err.message) >> MIO.fail(err)
                                    }
                                }
                              }
                              .flatMap { casesNel =>
                                implicit val IntT: Type[Int] = CTypes.Int
                                // The same names as enum values (see enumLeafNameExpr); Enumeration ids with
                                // useScalaEnumValueId, as in jsoniter-scala.
                                val cases: List[(Expr[K], Expr[String])] = casesNel.toList.map {
                                  case (name, caseExpr) =>
                                    val child = childrenList.find(_._1 == name).get._2
                                    import child.Underlying as ChildType
                                    (caseExpr, enumLeafNameExpr[K, ChildType](name, ctx.config))
                                }
                                LambdaBuilder
                                  .of1[JsonReader]("reader")
                                  .traverse { readerExpr =>
                                    val byName: Expr[K] = Expr.quote {
                                      val keyStr = Expr.splice(readerExpr).readKeyAsString()
                                      Expr.splice {
                                        cases.foldRight(
                                          Expr.quote(Expr.splice(readerExpr).enumValueError(keyStr): K)
                                        ) { case ((value, caseName), elseExpr) =>
                                          Expr.quote {
                                            if (keyStr == Expr.splice(caseName)) Expr.splice(value)
                                            else Expr.splice(elseExpr)
                                          }
                                        }
                                      }
                                    }
                                    MIO.pure(
                                      if (Type[K].isEnumeration) Expr.quote {
                                        if (Expr.splice(ctx.config).useScalaEnumValueId) {
                                          val id = Expr.splice(readerExpr).readKeyAsInt()
                                          Expr.splice {
                                            cases.foldRight(
                                              Expr.quote(
                                                Expr.splice(readerExpr).decodeError("illegal enum value " + id): K
                                              )
                                            ) { case ((value, _), elseExpr) =>
                                              Expr.quote {
                                                if (
                                                  id == hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
                                                    .scalaEnumValueId(Expr.splice(value))
                                                ) Expr.splice(value)
                                                else Expr.splice(elseExpr)
                                              }
                                            }
                                          }
                                        } else Expr.splice(byName)
                                      }
                                      else byName
                                    )
                                  }
                                  .map(builder => Some(builder.build[K]): Option[Expr[JsonReader => K]])
                              }
                        }
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
    }
  }

}
