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

    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val EitherDFValue: Type[Either[DecodingFailure, Value]] = DTypes.DecoderResult[Value]

      if (Key <:< Type[String]) {
        // String keys — use existing fast path
        LambdaBuilder
          .of1[HCursor]("valueCursor")
          .traverse { valueCursorExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr))
          }
          .map { builder =>
            val decodeFn = builder.build[Either[DecodingFailure, Value]]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              CirceDerivationUtils
                .decodeMapWith(
                  Expr.splice(dctx.cursor),
                  CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                  Expr
                    .splice(factoryExpr)
                    .asInstanceOf[scala.collection.Factory[(String, Value), A]]
                )
                .asInstanceOf[Either[DecodingFailure, A]]
            })
          }
      } else {
        // Non-String keys — try to derive a key decoder
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
                Rule.matched(Expr.quote {
                  CirceDerivationUtils
                    .decodeMapWithKeyDecoder[Key, Value, A](
                      Expr.splice(dctx.cursor),
                      Expr.splice(keyDecoderLambda),
                      CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr
                        .splice(factoryExpr)
                        .asInstanceOf[scala.collection.Factory[(Key, Value), A]]
                    )
                    .asInstanceOf[Either[DecodingFailure, A]]
                })
              }
          case None =>
            MIO.pure(
              Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived")
            )
        }
      }
    }

    /** Try to derive a String => Either[DecodingFailure, K] function for map keys. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyDecoder[K: Type](implicit
        ctx: DecoderCtx[?]
    ): MIO[Option[Expr[String => Either[DecodingFailure, K]]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val EitherDFK: Type[Either[DecodingFailure, K]] = DTypes.DecoderResult[K]

      Log.info(s"Attempting to derive key decoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types — inline parsing via runtime helpers
        val builtIn: Option[MIO[Option[Expr[String => Either[DecodingFailure, K]]]]] = {
          def makeBuiltInKeyDecoder(
              body: Expr[String] => Expr[Either[DecodingFailure, K]]
          ): MIO[Option[Expr[String => Either[DecodingFailure, K]]]] =
            LambdaBuilder
              .of1[String]("keyStr")
              .traverse { keyStrExpr =>
                MIO.pure(body(keyStrExpr))
              }
              .map { builder =>
                Some(builder.build[Either[DecodingFailure, K]]): Option[
                  Expr[String => Either[DecodingFailure, K]]
                ]
              }

          if (Type[K] =:= Type.of[Int])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyInt(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Long])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyLong(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Double])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(
                  CirceDerivationUtils.decodeKeyDouble(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]]
                )
              )
            )
          else if (Type[K] =:= Type.of[Short])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyShort(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else if (Type[K] =:= Type.of[Byte])
            Some(
              makeBuiltInKeyDecoder(s =>
                Expr.quote(CirceDerivationUtils.decodeKeyByte(Expr.splice(s)).asInstanceOf[Either[DecodingFailure, K]])
              )
            )
          else
            None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided KeyDecoder[K]
          DTypes.KeyDecoder[K].summonExprIgnoring().toEither match {
            case Right(keyDecoderExpr) =>
              Log.info(s"Found implicit KeyDecoder[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[String]("keyStr")
                  .traverse { keyStrExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyDecoderExpr).apply(Expr.splice(keyStrExpr)) match {
                        case Some(k) => Right(k): Either[DecodingFailure, K]
                        case None    =>
                          Left(
                            DecodingFailure(
                              "Failed to decode map key: " + Expr.splice(keyStrExpr),
                              Nil
                            )
                          ): Either[DecodingFailure, K]
                      }
                    })
                  }
                  .map { builder =>
                    Some(builder.build[Either[DecodingFailure, K]]): Option[
                      Expr[String => Either[DecodingFailure, K]]
                    ]
                  }
            case Left(_) =>
              // 3. Try value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyDecoder[Inner].flatMap {
                    case Some(innerKeyDecoder) =>
                      isValueType.value.wrap match {
                        case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
                          // Wrap returns Either[String, K] — convert Left(String) to Left(DecodingFailure)
                          // EitherDFK implicit is already in scope from above
                          LambdaBuilder
                            .of1[Inner]("inner")
                            .traverse { innerExpr =>
                              val wrapResult =
                                isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[Either[String, K]]]
                              MIO.pure(Expr.quote {
                                Expr.splice(wrapResult).left.map { (msg: String) =>
                                  io.circe.DecodingFailure(msg, Nil)
                                }
                              })
                            }
                            .flatMap { wrapBuilder =>
                              val wrapLambda = wrapBuilder.build[Either[DecodingFailure, K]]
                              LambdaBuilder
                                .of1[String]("keyStr")
                                .traverse { keyStrExpr =>
                                  MIO.pure(Expr.quote {
                                    Expr
                                      .splice(innerKeyDecoder)
                                      .apply(Expr.splice(keyStrExpr))
                                      .flatMap(Expr.splice(wrapLambda))
                                  })
                                }
                                .map { builder =>
                                  Some(builder.build[Either[DecodingFailure, K]]): Option[
                                    Expr[String => Either[DecodingFailure, K]]
                                  ]
                                }
                            }
                        case _ =>
                          // PlainValue — original behavior
                          LambdaBuilder
                            .of1[Inner]("inner")
                            .traverse { innerExpr =>
                              MIO.pure(isValueType.value.wrap.apply(innerExpr).asInstanceOf[Expr[K]])
                            }
                            .flatMap { wrapBuilder =>
                              val wrapLambda = wrapBuilder.build[K]
                              LambdaBuilder
                                .of1[String]("keyStr")
                                .traverse { keyStrExpr =>
                                  MIO.pure(Expr.quote {
                                    Expr
                                      .splice(innerKeyDecoder)
                                      .apply(Expr.splice(keyStrExpr))
                                      .map(Expr.splice(wrapLambda))
                                  })
                                }
                                .map { builder =>
                                  Some(builder.build[Either[DecodingFailure, K]]): Option[
                                    Expr[String => Either[DecodingFailure, K]]
                                  ]
                                }
                            }
                      }
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Try enum (all case objects) — build lookup Map[String, K] and use runtime helper
                  // Uses runtime dispatch to avoid Scala 3 staging issues with singleton expressions in LambdaBuilder
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
                            // Build singleton expressions for each child
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
                                // Build a Map[String, K] expression: Map(config.transformConstructorNames("Name") -> singleton, ...)
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
                                // Build the key decoder lambda using the runtime helper
                                LambdaBuilder
                                  .of1[String]("keyStr")
                                  .traverse { keyStrExpr =>
                                    MIO.pure(Expr.quote {
                                      CirceDerivationUtils.decodeEnumKey[K](
                                        Expr.splice(keyStrExpr),
                                        Expr.splice(lookupMapExpr)
                                      )
                                    })
                                  }
                                  .map { builder =>
                                    Some(builder.build[Either[DecodingFailure, K]]): Option[
                                      Expr[String => Either[DecodingFailure, K]]
                                    ]
                                  }
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
  }
}
