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

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    @scala.annotation.nowarn("msg=is never used")
    private[rules] def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val EitherDFValue: Type[Either[DecodingFailure, Value]] = DTypes.DecoderResult[Value]
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]

      implicit val AnyT: Type[Any] = DTypes.Any

      val keyDecoderMIO: MIO[Option[Expr[String => Either[DecodingFailure, Key]]]] =
        if (Key <:< Type[String]) {
          implicit val EitherDFKey: Type[Either[DecodingFailure, Key]] = DTypes.DecoderResult[Key]
          MIO.pure(Some(Expr.quote((s: String) => Right(s.asInstanceOf[Key]): Either[DecodingFailure, Key])))
        } else deriveKeyDecoder[Key]

      keyDecoderMIO.flatMap {
        case Some(keyDecoderLambda) =>
          LambdaBuilder
            .of1[HCursor]("valueCursor")
            .traverse { valueCursorExpr =>
              deriveDecoderRecursively[Value](using dctx.nest[Value](valueCursorExpr)).map(_.upcast[Any])
            }
            .map { builder =>
              val decodeFn = builder.build[Any]
              val factoryExpr = isMap.factory

              // Like circe's MapDecoder: fail-fast stops at the first failure, accumulating collects all of them; key
              // failures are reported at the key's cursor.
              val readLoop: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
                val cursor = Expr.splice(dctx.cursor)
                val failFast = Expr.splice(dctx.failFast)
                val decode = Expr.splice(decodeFn)
                val keyDecoder = Expr.splice(keyDecoderLambda)
                val mapBuilder = Expr.splice(factoryExpr).newBuilder
                cursor.keys match {
                  case None =>
                    throw new CirceDerivationUtils.CollectionBuildException(
                      CirceDerivationUtils.wrongTypeFailure("object", cursor)
                    )
                  case Some(keys) =>
                    val errors = new CirceDerivationUtils.FailureCollector
                    val iter = keys.iterator
                    while (iter.hasNext) {
                      val keyStr = iter.next()
                      val valueCursor = cursor.downField(keyStr).asInstanceOf[HCursor]
                      keyDecoder(keyStr) match {
                        case Right(key) =>
                          val value = decode(valueCursor)
                          if (errors.add(failFast, value))
                            mapBuilder += Expr.splice(
                              isMap.pair(
                                Expr.quote(key),
                                Expr.quote(CirceDerivationUtils.resultValue[Value](value))
                              )
                            )
                        case Left(err) =>
                          errors.addFailure(failFast, CirceDerivationUtils.keyFailure(err, valueCursor))
                      }
                    }
                    errors.throwIfFailed()
                }
                mapBuilder
              }
              wrapBuildResult[A](isMap.build, isMap.build.ctor(readLoop).asInstanceOf[Expr[Any]])
            }
        case None =>
          MIO.pure(
            Rule.yielded(s"Map key type ${Key.prettyPrint} is not String and no key decoder could be derived")
          )
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
        // A KeyDecoder in scope (user-provided, or circe's own for Int/Long/...) takes precedence over everything
        // else; the built-in parsers below are only a fallback.
        def builtIn: Option[Expr[String => Either[DecodingFailure, K]]] =
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

        {
          DTypes.KeyDecoder[K].summonExprIgnoring().toEither match {
            case Right(keyDecoderExpr) =>
              Log.info(s"Found implicit KeyDecoder[${Type[K].prettyPrint}]") >>
                MIO.pure(
                  Some(Expr.quote { (s: String) =>
                    Expr.splice(keyDecoderExpr).apply(s) match {
                      case Some(k) => Right(k): Either[DecodingFailure, K]
                      case None    => Left(CirceDerivationUtils.invalidKeyFailure): Either[DecodingFailure, K]
                    }
                  }): Option[Expr[String => Either[DecodingFailure, K]]]
                )
            case Left(_) if builtIn.isDefined =>
              MIO.pure(builtIn)
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
                      val childrenList =
                        flattenedChildren[K](enumm)(_ => false) // nested case-object hierarchies are flattened
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
                                            MIO.fail(
                                              DecoderDerivationError
                                                .UnexpectedParameterInSingleton(
                                                  childName,
                                                  "Unexpected parameter in enum singleton"
                                                )
                                            )
                                        }).flatMap {
                                          case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[K]]))
                                          case None       =>
                                            val err =
                                              DecoderDerivationError.CannotConstructType(childName, isSingleton = true)
                                            Log.error(err.message) >> MIO.fail(err)
                                        }
                                      case None =>
                                        val err =
                                          DecoderDerivationError.CannotConstructType(childName, isSingleton = true)
                                        Log.error(err.message) >> MIO.fail(err)
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
