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

    /** Try to derive a K => String function for map keys. Returns None if derivation fails.
      *
      * Per project rule 5, [[LambdaBuilder]] is reserved for lambdas passed to collection / Optional iteration helpers.
      * The functions returned here are pre-built once during macro expansion and spliced as plain `Expr[K => String]`
      * values into a runtime helper, so we use direct cross-quotes function literals (`Expr.quote { (k: K) => ... }`)
      * instead.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def deriveKeyEncoder[K: Type](implicit ctx: EncoderCtx[?]): MIO[Option[Expr[K => String]]] =
      Log.info(s"Attempting to derive key encoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types — inline key.toString (same as circe's KeyEncoder[Int] etc.)
        val builtIn: Option[Expr[K => String]] = {
          def makeToStringKeyEncoder: Expr[K => String] =
            Expr.quote((k: K) => k.toString)

          if (Type[K] =:= Type.of[Int]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Long]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Double]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Short]) Some(makeToStringKeyEncoder)
          else if (Type[K] =:= Type.of[Byte]) Some(makeToStringKeyEncoder)
          else None
        }

        builtIn match {
          case Some(fn) => MIO.pure(Some(fn))
          case None     =>
            // 2. Try summoning user-provided KeyEncoder[K] — splice it directly,
            //    no LambdaBuilder needed.
            Types.KeyEncoder[K].summonExprIgnoring().toEither match {
              case Right(keyEncoderExpr) =>
                Log.info(s"Found implicit KeyEncoder[${Type[K].prettyPrint}]") >>
                  MIO.pure(Some(Expr.quote { (k: K) =>
                    Expr.splice(keyEncoderExpr).apply(k)
                  }))
              case Left(_) =>
                // 3. Try value type — unwrap to inner, recurse
                Type[K] match {
                  case IsValueType(isValueType) =>
                    import isValueType.Underlying as Inner
                    deriveKeyEncoder[Inner].map {
                      case Some(innerKeyEncoder) =>
                        // We need a `K => String` that unwraps to `Inner` then applies the
                        // inner encoder. The unwrap helper is path-dependent on `isValueType`,
                        // so we route through a local helper closing over its `value`.
                        Some(buildValueTypeKeyEncoder[K, Inner](isValueType.value, innerKeyEncoder))
                      case None => None
                    }
                  case _ =>
                    // 4. Try enum (all case objects) — use toString for key
                    Enum.parse[K].toOption match {
                      case Some(enumm) =>
                        val childrenList = enumm.directChildren.toList
                        val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                          case (_, child) =>
                            SingletonValue.unapply(child.Underlying).isDefined
                        }
                        if (allCaseObjects) {
                          val configExpr = ctx.config
                          MIO.pure(Some(Expr.quote { (k: K) =>
                            Expr.splice(configExpr).transformConstructorNames(k.toString)
                          }))
                        } else MIO.pure(None)
                      case None => MIO.pure(None)
                    }
                }
            }
        }
      }

    /** Build a K => String function that unwraps a value type to its inner representation and applies an inner encoder.
      * Extracted as a helper because the unwrap closure captures path-dependent state from the [[IsValueType]]
      * instance.
      */
    private def buildValueTypeKeyEncoder[K: Type, Inner: Type](
        isValueType: IsValueTypeOf[K, Inner],
        innerKeyEncoder: Expr[Inner => String]
    ): Expr[K => String] =
      Expr.quote { (k: K) =>
        Expr.splice(innerKeyEncoder).apply(Expr.splice(isValueType.unwrap(Expr.quote(k))))
      }
  }
}
