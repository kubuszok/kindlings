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
    implicit val StringJsonPairT: Type[(String, Json)] = Types.StringJsonPair

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

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    private[rules] def deriveMapEntries[A: EncoderCtx, Pair: Type](
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
        // Non-String keys — try to derive a key-to-String function. The pair lambda returns the key itself (not the
        // encoded key): splicing the key-encoding function literal inside the LambdaBuilder body would nest a lambda
        // created with a different owner (Scala 3 "Block contains definition with different owners").
        implicit val KeyJsonPairT: Type[(Key, Json)] = Type.of[(Key, Json)]
        deriveKeyEncoder[Key].flatMap {
          case Some(keyEncoderLambda) =>
            LambdaBuilder
              .of1[Pair]("pair")
              .traverse { pairExpr =>
                val keyExpr = isMap.key(pairExpr)
                val valueExpr = isMap.value(pairExpr)
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueJson =>
                  Expr.quote {
                    (Expr.splice(keyExpr), Expr.splice(valueJson))
                  }
                }
              }
              .map { builder =>
                val pairLambda = builder.build[(Key, Json)]
                val iterableExpr = isMap.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.jsonFromKeyedPairs[Pair, Key](
                    Expr.splice(iterableExpr),
                    Expr.splice(pairLambda),
                    Expr.splice(keyEncoderLambda)
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
        // 1. A KeyEncoder in scope (user-provided, or circe's own for Int/Long/...) takes precedence over everything
        //    else (like the decoder, and like circe's `Encoder.encodeMap`); the built-in toString is only a fallback.
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

        Types.KeyEncoder[K].summonExprIgnoring().toEither match {
          case Right(keyEncoderExpr) =>
            Log.info(s"Found implicit KeyEncoder[${Type[K].prettyPrint}]") >>
              MIO.pure(Some(Expr.quote { (k: K) =>
                Expr.splice(keyEncoderExpr).apply(k)
              }))
          case Left(_) if builtIn.isDefined =>
            MIO.pure(builtIn)
          case Left(_) =>
            // 2. Try value type — unwrap to inner, recurse
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
                    val childrenList =
                      flattenedChildren[K](enumm)(_ => false) // nested case-object hierarchies are flattened
                    val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                      case (_, child) =>
                        SingletonValue.unapply(child.Underlying).isDefined
                    }
                    if (allCaseObjects) {
                      val configExpr = ctx.config
                      // Use the constructor name (as the decoder and enum values do), not `toString`, which a
                      // case object may override. Enumeration/Java enum values have no singleton type to compare
                      // against here, and their name is their `toString` unless overridden.
                      val singletons: List[(String, Expr[K])] = childrenList.flatMap { case (name, child) =>
                        SingletonValue
                          .unapply(child.Underlying)
                          .map(sv => (name, sv.singletonExpr.asInstanceOf[Expr[K]]))
                      }
                      if (Type[K].isEnumeration || Type[K].isJavaEnum || singletons.size != childrenList.size)
                        MIO.pure(Some(Expr.quote { (k: K) =>
                          Expr.splice(configExpr).transformConstructorNames(k.toString)
                        }))
                      else
                        MIO.pure(Some(Expr.quote { (k: K) =>
                          Expr
                            .splice(configExpr)
                            .transformConstructorNames(Expr.splice {
                              val kExpr: Expr[K] = Expr.quote(k)
                              singletons.foldRight(Expr.quote(k.toString): Expr[String]) {
                                case ((name, singleton), elseExpr) =>
                                  Expr.quote {
                                    if (Expr.splice(kExpr) == Expr.splice(singleton)) Expr.splice(Expr(name))
                                    else Expr.splice(elseExpr)
                                  }
                              }
                            })
                        }))
                    } else MIO.pure(None)
                  case None => MIO.pure(None)
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
