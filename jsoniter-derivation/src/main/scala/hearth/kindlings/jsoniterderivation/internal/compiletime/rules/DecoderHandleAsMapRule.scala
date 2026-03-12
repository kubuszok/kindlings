package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

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

    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig

      if (Key <:< Type[String]) {
        // String keys — derive value decoder, plus key-as-value decoder for mapAsArray
        LambdaBuilder
          .of1[JsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .flatMap { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            // Derive key-as-value decoder for String (for mapAsArray mode)
            LambdaBuilder
              .of1[JsonReader]("keyValueReader")
              .traverse { keyReaderExpr =>
                // String keys: read as string value
                MIO.pure(Expr.quote {
                  Expr.splice(keyReaderExpr).readString(null).asInstanceOf[Key]
                })
              }
              .map { keyValueBuilder =>
                val keyValueDecodeFn = keyValueBuilder.build[Key]
                val factoryExpr = isMap.factory
                Rule.matched(Expr.quote {
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
              }
          }
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
                    Rule.matched(Expr.quote {
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
                  }
              }
          case None =>
            // No key decoder — try value-level decoding for mapAsArray-only support
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
                    Rule.matched(Expr.quote {
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

          if (Type[K] =:= Type.of[Int])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsInt().asInstanceOf[K])))
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
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBigDecimal().asInstanceOf[K])))
          else if (Type[K] =:= Type.of[BigInt])
            Some(makeKeyDecoder(r => Expr.quote(Expr.splice(r).readKeyAsBigInt().asInstanceOf[K])))
          else
            None
        }

        builtIn.getOrElse {
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
                                case scala.Left(msg) => throw new IllegalArgumentException(msg)
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
                case _ =>
                  // 4. Enum (all case objects) — read key as string, match against case names
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
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
                                // Build Map[String, K] expression
                                val mapEntries: List[Expr[(String, K)]] = casesNel.toList.map { case (name, caseExpr) =>
                                  Expr.quote {
                                    (Expr.splice(Expr(name)), Expr.splice(caseExpr))
                                  }
                                }
                                val lookupMapExpr: Expr[Map[String, K]] = mapEntries.foldLeft(
                                  Expr.quote(Map.empty[String, K])
                                ) { (accExpr, entryExpr) =>
                                  Expr.quote(Expr.splice(accExpr) + Expr.splice(entryExpr))
                                }
                                LambdaBuilder
                                  .of1[JsonReader]("reader")
                                  .traverse { readerExpr =>
                                    MIO.pure(Expr.quote {
                                      val keyStr = Expr.splice(readerExpr).readKeyAsString()
                                      Expr
                                        .splice(lookupMapExpr)
                                        .getOrElse(
                                          keyStr,
                                          Expr.splice(readerExpr).decodeError("unknown enum key: " + keyStr): K
                                        )
                                    })
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
