package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsMapRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val StringT: Type[String] = CTypes.String
    implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
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
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      import isMap.{Key, Value}
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      if (Key <:< Type[String]) {
        // String keys — derive value encoder, plus key-as-value encoder for mapAsArray
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .flatMap { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            // Derive a key-as-value encoder for String (for mapAsArray mode)
            LambdaBuilder
              .of1[Key]("keyAsValue")
              .traverse { keyExpr =>
                // String keys: write as string value
                MIO.pure(Expr.quote {
                  Expr.splice(ectx.writer).writeVal(Expr.splice(keyExpr).asInstanceOf[String])
                })
              }
              .map { keyValueBuilder =>
                val keyValueLambda = keyValueBuilder.build[Unit]
                val iterableExpr = isMap.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  if (Expr.splice(ectx.config).mapAsArray) {
                    JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                      Expr.splice(ectx.writer),
                      Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                      Expr.splice(keyValueLambda),
                      Expr.splice(valueLambda)
                    )
                  } else {
                    JsoniterDerivationUtils.writeMapStringKeyed[Value](
                      Expr.splice(ectx.writer),
                      Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
                      Expr.splice(valueLambda)
                    )
                  }
                })
              }
          }
      } else {
        // Non-String keys — try to derive key encoding for object mode
        deriveKeyEncoding[Key].flatMap {
          case Some(keyEncoderLambda) =>
            // Derive value encoder for V
            LambdaBuilder
              .of1[Value]("mapValue")
              .traverse { valueExpr =>
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
              }
              .flatMap { valueBuilder =>
                val valueLambda = valueBuilder.build[Unit]
                // Derive key-as-value encoder for mapAsArray mode
                LambdaBuilder
                  .of1[Key]("keyAsValue")
                  .traverse { keyExpr =>
                    deriveEncoderRecursively[Key](using ectx.nest(keyExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueLambda = keyValueBuilder.build[Unit]
                    val iterableExpr = isMap.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      if (Expr.splice(ectx.config).mapAsArray) {
                        JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyValueLambda),
                          Expr.splice(valueLambda)
                        )
                      } else {
                        JsoniterDerivationUtils.writeMapWithKeyEncoder[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyEncoderLambda),
                          Expr.splice(valueLambda)
                        )
                      }
                    })
                  }
              }
          case None =>
            // No key encoder — try value-level encoding for mapAsArray-only support
            LambdaBuilder
              .of1[Value]("mapValue")
              .traverse { valueExpr =>
                deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
              }
              .flatMap { valueBuilder =>
                val valueLambda = valueBuilder.build[Unit]
                LambdaBuilder
                  .of1[Key]("keyAsValue")
                  .traverse { keyExpr =>
                    deriveEncoderRecursively[Key](using ectx.nest(keyExpr))
                  }
                  .map { keyValueBuilder =>
                    val keyValueLambda = keyValueBuilder.build[Unit]
                    val iterableExpr = isMap.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      if (Expr.splice(ectx.config).mapAsArray) {
                        JsoniterDerivationUtils.writeMapAsArray[Key, Value](
                          Expr.splice(ectx.writer),
                          Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                          Expr.splice(keyValueLambda),
                          Expr.splice(valueLambda)
                        )
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
                    s"Map key type ${Key.prettyPrint} is not String and no key encoder could be derived"
                  )
                )
              }
        }
      }
    }

    /** Try to derive a (K, JsonWriter) => Unit function for map key encoding. Returns None if derivation fails. */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def deriveKeyEncoding[K: Type](implicit
        ctx: EncoderCtx[?]
    ): MIO[Option[Expr[(K, JsonWriter) => Unit]]] =

      Log.info(s"Attempting to derive key encoder for ${Type[K].prettyPrint}") >> {
        // 1. Built-in types with native writeKey overloads
        val builtIn: Option[MIO[Option[Expr[(K, JsonWriter) => Unit]]]] = {
          def makeKeyEncoder(
              body: (Expr[K], Expr[JsonWriter]) => Expr[Unit]
          ): MIO[Option[Expr[(K, JsonWriter) => Unit]]] =
            LambdaBuilder
              .of2[K, JsonWriter]("key", "writer")
              .traverse { case (keyExpr, writerExpr) =>
                MIO.pure(body(keyExpr, writerExpr))
              }
              .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])

          if (Type[K] =:= Type.of[Int])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Int]))))
          else if (Type[K] =:= Type.of[Long])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Long]))))
          else if (Type[K] =:= Type.of[Double])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Double]))))
          else if (Type[K] =:= Type.of[Float])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Float]))))
          else if (Type[K] =:= Type.of[Short])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Short]))))
          else if (Type[K] =:= Type.of[Boolean])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Boolean]))))
          else if (Type[K] =:= Type.of[BigDecimal])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[BigDecimal]))))
          else if (Type[K] =:= Type.of[BigInt])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[BigInt]))))
          else
            None
        }

        builtIn.getOrElse {
          // 2. Try summoning user-provided JsonKeyCodec[K]
          CTypes.JsonKeyCodec[K].summonExprIgnoring().toEither match {
            case Right(keyCodecExpr) =>
              Log.info(s"Found implicit JsonKeyCodec[${Type[K].prettyPrint}]") >>
                LambdaBuilder
                  .of2[K, JsonWriter]("key", "writer")
                  .traverse { case (keyExpr, writerExpr) =>
                    MIO.pure(Expr.quote {
                      Expr.splice(keyCodecExpr).encodeKey(Expr.splice(keyExpr), Expr.splice(writerExpr))
                    })
                  }
                  .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
            case Left(_) =>
              // 3. Value type — unwrap to inner, recurse
              Type[K] match {
                case IsValueType(isValueType) =>
                  import isValueType.Underlying as Inner
                  deriveKeyEncoding[Inner].flatMap {
                    case Some(innerKeyEncoder) =>
                      LambdaBuilder
                        .of2[K, JsonWriter]("key", "writer")
                        .traverse { case (keyExpr, writerExpr) =>
                          val unwrapped = isValueType.value.unwrap(keyExpr)
                          MIO.pure(Expr.quote {
                            Expr.splice(innerKeyEncoder).apply(Expr.splice(unwrapped), Expr.splice(writerExpr))
                          })
                        }
                        .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
                    case None => MIO.pure(None)
                  }
                case _ =>
                  // 4. Enum (all case objects) — write case name as key
                  Enum.parse[K].toOption match {
                    case Some(enumm) =>
                      val childrenList = enumm.directChildren.toList
                      val allCaseObjects = Type[K].isEnumeration || Type[K].isJavaEnum || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        // Use .toString on case objects — for case objects, .toString returns the simple name.
                        // Use adtLeafClassNameMapper for custom name mapping.
                        LambdaBuilder
                          .of2[K, JsonWriter]("key", "writer")
                          .traverse { case (keyExpr, writerExpr) =>
                            MIO.pure(Expr.quote {
                              Expr
                                .splice(writerExpr)
                                .writeKey(
                                  Expr.splice(ctx.config).adtLeafClassNameMapper(Expr.splice(keyExpr).toString)
                                )
                            })
                          }
                          .map(builder => Some(builder.build[Unit]): Option[Expr[(K, JsonWriter) => Unit]])
                      } else MIO.pure(None)
                    case None => MIO.pure(None)
                  }
              }
          }
        }
      }
  }

}
