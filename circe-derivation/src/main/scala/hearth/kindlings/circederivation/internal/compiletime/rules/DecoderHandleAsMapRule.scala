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
        // 1. Built-in types — inline parsing via runtime helpers.
        //
        // Per project rule 5, [[LambdaBuilder]] is reserved for lambdas passed to collection /
        // Optional iteration helpers. The functions returned here are pre-built once during
        // macro expansion and spliced as plain `Expr[String => Either[DecodingFailure, K]]`
        // values into a runtime helper, so we use direct cross-quotes function literals
        // (`Expr.quote { (s: String) => ... }`) instead.
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
          // 2. Try summoning user-provided KeyDecoder[K] — wrap with a direct cross-quotes lambda.
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
              // 3. Try value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyDecoder[Inner].map {
                    case Some(innerKeyDecoder) =>
                      Some(buildValueTypeKeyDecoder[K, Inner](isValueType.value, innerKeyDecoder))
                    case None => None
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
                                // Build the key decoder as a direct cross-quotes lambda over
                                // the runtime helper.
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

    /** Build a String => Either[DecodingFailure, K] function that delegates to an inner key decoder and then wraps the
      * inner value into K via the provided value type's `wrap`.
      *
      * Extracted as a helper because the `wrap` closure captures path-dependent state from the [[IsValueTypeOf]]
      * instance, which is not safe to reference inside [[Expr.quote]].
      */
    private def buildValueTypeKeyDecoder[K: Type, Inner: Type](
        isValueType: IsValueTypeOf[K, Inner],
        innerKeyDecoder: Expr[String => Either[DecodingFailure, Inner]]
    ): Expr[String => Either[DecodingFailure, K]] = {
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherDFK: Type[Either[DecodingFailure, K]] = DTypes.DecoderResult[K]
      @scala.annotation.nowarn("msg=is never used")
      implicit val EitherDFInner: Type[Either[DecodingFailure, Inner]] = DTypes.DecoderResult[Inner]

      isValueType.wrap match {
        case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
          // wrap returns Either[String, K] — convert Left(String) to Left(DecodingFailure)
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).flatMap { (inner: Inner) =>
              Expr
                .splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[Either[String, K]]])
                .left
                .map((msg: String) => io.circe.DecodingFailure(msg, Nil))
            }
          }
        case _ =>
          // PlainValue — wrap is total
          Expr.quote { (s: String) =>
            Expr.splice(innerKeyDecoder).apply(s).map { (inner: Inner) =>
              Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[K]])
            }
          }
      }
    }
  }
}
