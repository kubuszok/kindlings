package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsMapRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val JsonT: Type[Json] = Types.Json
    implicit val StringT: Type[String] = Types.String
    implicit val StringJsonPairT: Type[(String, Json)] = Type.of[(String, Json)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Json]]] = {
      import isMap.{Key, Value}
      if (Key <:< Type[String]) {
        // String keys — use existing fast path
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueJson))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Json)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              CirceDerivationUtils.jsonFromMappedPairs[Pair](
                Expr.splice(iterableExpr),
                Expr.splice(pairLambda)
              )
            })
          }
      } else {
        // Non-String keys — try to derive a key-to-String function
        deriveKeyEncoder[Key].flatMap {
          case Some(keyEncoderLambda) =>
            LambdaBuilder
              .of1[Pair]("pair")
              .traverse { pairExpr =>
                val keyExpr = isMap.key(pairExpr)
                val valueExpr = isMap.value(pairExpr)
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
                  Expr.quote {
                    (Expr.splice(keyEncoderLambda).apply(Expr.splice(keyExpr)), Expr.splice(valueJson))
                  }
                }
              }
              .map { builder =>
                val pairLambda = builder.build[(String, Json)]
                val iterableExpr = isMap.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.jsonFromMappedPairs[Pair](
                    Expr.splice(iterableExpr),
                    Expr.splice(pairLambda)
                  )
                })
              }
          case None =>
            MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key encoder could be derived"))
        }
      }
    }

    /** Try to derive a K => String function for map keys. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyEncoder[K: Type](implicit ctx: EncoderCtx[?]): MIO[Option[Expr[K => String]]] =
      Log.info(s"Attempting to derive key encoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types — inline key.toString (same as circe's KeyEncoder[Int] etc.)
        val builtIn: Option[MIO[Option[Expr[K => String]]]] = {
          def makeToStringKeyEncoder: MIO[Option[Expr[K => String]]] =
            LambdaBuilder
              .of1[K]("key")
              .traverse { keyExpr =>
                MIO.pure(Expr.quote(Expr.splice(keyExpr).toString))
              }
              .map(builder => Some(builder.build[String]): Option[Expr[K => String]])

          if (Type[K] =:= Type.of[Int]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Long]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Double]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Short]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Byte]) Some(makeToStringKeyEncoder)
          else None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided KeyEncoder[K]
          Types.KeyEncoder[K].summonExprIgnoring().toEither match {
            case Right(keyEncoderExpr) =>
              Log.info(s"Found implicit KeyEncoder[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of1[K]("key")
                  .traverse { keyExpr =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyEncoderExpr).apply(Expr.splice(keyExpr))
                    })
                  }
                  .map { builder =>
                    Some(builder.build[String]): Option[Expr[K => String]]
                  }
            case Left(_) =>
              // 3. Try value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyEncoder[Inner].flatMap {
                    case Some(innerKeyEncoder) =>
                      LambdaBuilder
                        .of1[K]("key")
                        .traverse { keyExpr =>
                          val unwrapped = isValueType.value.unwrap(keyExpr)
                          MIO.pure(Expr.quote {
                            Expr.splice(innerKeyEncoder).apply(Expr.splice(unwrapped))
                          })
                        }
                        .map { builder =>
                          Some(builder.build[String]): Option[Expr[K => String]]
                        }
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Try enum (all case objects) — use toString for key
                  // Uses .toString to avoid Scala 3 staging issues with singleton expressions in LambdaBuilder
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        LambdaBuilder
                          .of1[K]("key")
                          .traverse { keyExpr =>
                            MIO.pure(Expr.quote {
                              Expr.splice(ctx.config).transformConstructorNames(Expr.splice(keyExpr).toString)
                            })
                          }
                          .map { builder =>
                            Some(builder.build[String]): Option[Expr[K => String]]
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
