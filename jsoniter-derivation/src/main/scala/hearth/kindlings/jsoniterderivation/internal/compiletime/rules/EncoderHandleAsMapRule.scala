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

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    private[rules] def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      import isMap.{Key, Value}
      @scala.annotation.nowarn("msg=is never used")
      implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig
      // String keys use the inlined fast path unless a user-provided JsonKeyCodec[String] exists.
      if (Key =:= Type[String] && CTypes.JsonKeyCodec[Key].summonExprIgnoring().toEither.isLeft) {
        val mapAsArrayStaticallyFalse = ectx.evaluatedConfig.exists(!_.mapAsArray)
        if (mapAsArrayStaticallyFalse)
          deriveStringKeyedMapInline[A, Pair, Key, Value](isMap)
        else
          deriveStringKeyedMapViaHelper[A, Pair, Key, Value](isMap)
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
          case None if ectx.evaluatedConfig.exists(!_.mapAsArray) =>
            val err = CodecDerivationError.UnsupportedMapKey(Key.prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
          case None =>
            // No key encoder — try value-level encoding for mapAsArray-only support (config only known at runtime)
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

    @scala.annotation.nowarn("msg=is never used")
    private def deriveStringKeyedMapInline[A: EncoderCtx, Pair: Type, Key: Type, Value: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      val dummyValue: Expr[Value] = Expr.quote(null.asInstanceOf[Value])
      deriveEncoderRecursively[Value](using ectx.nest(dummyValue)).flatMap { _ =>
        ectx.getHelper[Value].map { helperOpt =>
          val helper = helperOpt.get
          // `isMap.foreach` lowers to a zero-closure `while` loop over the map's iterator (Hearth's default
          // `IsCollectionOf.foreach`), so this needs no hand-rolled loop — the optimisation lives in Hearth and is
          // shared by every module that iterates a map.
          val writeEntries = isMap.foreach(ectx.value) { pairExpr =>
            Expr.quote {
              val entry = Expr.splice(pairExpr).asInstanceOf[(String, Value)]
              Expr.splice(ectx.writer).writeKey(entry._1)
              Expr.splice(helper(Expr.quote(entry._2), ectx.writer, ectx.config))
            }
          }
          Rule.matched(Expr.quote {
            val out = Expr.splice(ectx.writer)
            out.writeObjectStart()
            Expr.splice(writeEntries)
            out.writeObjectEnd()
          })
        }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveStringKeyedMapViaHelper[A: EncoderCtx, Pair: Type, Key: Type, Value: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] =
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

          val literal = literalKey[K]
          if (literal.isDefined) {
            val keyString = literal.get._1
            Some(makeKeyEncoder((_, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(Expr(keyString))))))
          } else if (Type[K] =:= Type.of[Int])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Int]))))
          else if (Type[K] =:= Type.of[String])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[String]))))
          else if (Type[K] =:= Type.of[Byte])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Byte]))))
          else if (Type[K] =:= Type.of[Char])
            Some(makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[Char]))))
          else if (Type[K] =:= CTypes.UUID)
            Some(
              makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.util.UUID])))
            )
          else if (Type[K] =:= CTypes.Duration)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.Duration]))
              )
            )
          else if (Type[K] =:= CTypes.Instant)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.Instant]))
              )
            )
          else if (Type[K] =:= CTypes.LocalDate)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.LocalDate]))
              )
            )
          else if (Type[K] =:= CTypes.LocalDateTime)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.LocalDateTime]))
              )
            )
          else if (Type[K] =:= CTypes.LocalTime)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.LocalTime]))
              )
            )
          else if (Type[K] =:= CTypes.MonthDay)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.MonthDay]))
              )
            )
          else if (Type[K] =:= CTypes.OffsetDateTime)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.OffsetDateTime]))
              )
            )
          else if (Type[K] =:= CTypes.OffsetTime)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.OffsetTime]))
              )
            )
          else if (Type[K] =:= CTypes.Period)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.Period]))
              )
            )
          else if (Type[K] =:= CTypes.Year)
            Some(
              makeKeyEncoder((k, w) => Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.Year])))
            )
          else if (Type[K] =:= CTypes.YearMonth)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.YearMonth]))
              )
            )
          else if (Type[K] =:= CTypes.ZonedDateTime)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.ZonedDateTime]))
              )
            )
          else if (Type[K] =:= CTypes.ZoneId)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.ZoneId]))
              )
            )
          else if (Type[K] =:= CTypes.ZoneOffset)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.time.ZoneOffset]))
              )
            )
          else if (Type[K] =:= CTypes.JavaCharacter)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Character].charValue()))
              )
            )
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
          // Java boxed primitives — unbox, then use native writeKey overload
          else if (Type[K] =:= CTypes.JavaByte)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Byte].byteValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaShort)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Short].shortValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaInteger)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Integer].intValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaLong)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Long].longValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaFloat)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Float].floatValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaDouble)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Double].doubleValue()))
              )
            )
          else if (Type[K] =:= CTypes.JavaBoolean)
            Some(
              makeKeyEncoder((k, w) =>
                Expr.quote(Expr.splice(w).writeKey(Expr.splice(k).asInstanceOf[java.lang.Boolean].booleanValue()))
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
                case _
                    if !ctx.evaluatedConfig
                      .exists(!_.inlineOneValueClasses) && oneValueClassField[K](ctx.evaluatedConfig).isDefined =>
                  // 3b. inlineOneValueClasses: a one-value class key is its field's key (as jsoniter-scala). With a
                  // runtime config it is always unwrapped (there is no other key encoding for it).
                  val (product, fName, param) = oneValueClassField[K](ctx.evaluatedConfig).get
                  import param.tpe.Underlying as Inner
                  deriveKeyEncoding[Inner].flatMap {
                    case Some(innerKeyEncoder) =>
                      LambdaBuilder
                        .of2[K, JsonWriter]("key", "writer")
                        .traverse { case (keyExpr, writerExpr) =>
                          val inner = product.fieldValuesAt(keyExpr)(fName).value.asInstanceOf[Expr[Inner]]
                          MIO.pure(Expr.quote {
                            Expr.splice(innerKeyEncoder).apply(Expr.splice(inner), Expr.splice(writerExpr))
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
                      val allCaseObjects = Type[K].isEnumeration || isJavaEnumType[K] || childrenList.forall {
                        case (_, child) =>
                          SingletonValue.unapply(child.Underlying).isDefined
                      }
                      if (allCaseObjects) {
                        // The same names as enum values (see enumLeafNameExpr); Enumeration ids with
                        // useScalaEnumValueId, as in jsoniter-scala.
                        val cases: List[(Expr[K], Expr[String])] = childrenList.flatMap { case (childName, child) =>
                          import child.Underlying as ChildType
                          SingletonValue.unapply(Type[ChildType]).map { sv =>
                            (
                              sv.singletonExpr.asInstanceOf[Expr[K]],
                              enumLeafNameExpr[K, ChildType](childName, ctx.config)
                            )
                          }
                        }
                        LambdaBuilder
                          .of2[K, JsonWriter]("key", "writer")
                          .traverse { case (keyExpr, writerExpr) =>
                            val byName: Expr[Unit] = cases.foldRight(
                              Expr.quote(Expr.splice(writerExpr).writeKey(Expr.splice(keyExpr).toString))
                            ) { case ((value, name), elseExpr) =>
                              Expr.quote {
                                if (
                                  Expr.splice(keyExpr).asInstanceOf[AnyRef] eq Expr.splice(value).asInstanceOf[AnyRef]
                                )
                                  Expr.splice(writerExpr).writeKey(Expr.splice(name))
                                else Expr.splice(elseExpr)
                              }
                            }
                            MIO.pure(
                              if (Type[K].isEnumeration) Expr.quote {
                                if (Expr.splice(ctx.config).useScalaEnumValueId)
                                  Expr
                                    .splice(writerExpr)
                                    .writeKey(
                                      hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
                                        .scalaEnumValueId(Expr.splice(keyExpr))
                                    )
                                else Expr.splice(byName)
                              }
                              else byName
                            )
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
