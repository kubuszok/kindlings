package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsEnumRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            // Note: caching is handled by deriveDecoderRecursively — do NOT call setHelper here.
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val StringT: Type[String] = CTypes.String
      implicit val ListStringT: Type[List[String]] = CTypes.ListString

      // Use exhaustive (leaf) children to flatten nested sealed trait hierarchies.
      // This ensures intermediate sealed traits like MotorVehicle in Vehicle > MotorVehicle > Truck
      // are skipped — only concrete leaf types (Truck, Motorcycle, Bicycle) are decoded.
      val childrenList = adtLeaves[A](enumm)

      // Check at compile time if all children are singletons (case objects with no fields)
      val isJavaEnum = isJavaEnumType[A]
      val allCaseObjects = Type[A].isEnumeration || isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Expr
              .splice(dctx.reader)
              .decodeError(
                "Enum " + Expr.splice(Expr(Type[A].prettyPrint)) + " has no subtypes"
              ): A
          })

        case Some(children) =>
          val isScalaEnumeration = Type[A].isEnumeration
          val isEnumerationOrJavaEnum = isScalaEnumeration || isJavaEnum

          // For each child: a wrapper-mode decoder (`{"Leaf":{...}}`; singletons skip the inner object), an inline
          // (discriminator-mode) decoder, and for singletons a string decoder (`"Leaf"`).
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              val isSingleton = SingletonValue.unapply(Type[ChildType]).isDefined || isEnumerationOrJavaEnum
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <-
                    if (isEnumerationOrJavaEnum) MIO.pure(None)
                    else deriveChildDecoder[A, ChildType](childName).map(Some(_))
                  inline <-
                    if (isEnumerationOrJavaEnum) MIO.pure(None)
                    else deriveChildDecoderInline[A, ChildType](childName).map(Some(_))
                  stringEnum <-
                    if (isSingleton) deriveChildDecoderStringEnum[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                  enumId <-
                    if (isScalaEnumeration) deriveChildDecoderEnumId[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                } yield (wrapper, inline, stringEnum, enumId)
              }
            }
            .map { allDispatchers =>
              val wrapperDispatchers = allDispatchers.toList.flatMap(_._1)
              // Leaves with user-provided codecs re-read the whole object (see deriveChildDecoderInline).
              val markLeaves: Expr[Boolean] = Expr(childrenList.exists { case (_, child) =>
                import child.Underlying as ChildType
                hasUserCodecAsLeaf[ChildType]
              })
              val inlineDispatchers = allDispatchers.toList.flatMap(_._2)
              val stringDispatchers = allDispatchers.toList.flatMap(_._3)
              val enumIdDispatchers = allDispatchers.toList.flatMap(_._4)

              implicit val IntT: Type[Int] = CTypes.Int
              implicit val BooleanT: Type[Boolean] = CTypes.Boolean

              // jsoniter-scala's errors: `illegal value of discriminator field "type"` in discriminator mode,
              // `illegal discriminator` for an unknown wrapper key / object name, `illegal enum value "X"` for enums.
              val wrapperError: Expr[String] => Expr[A] =
                _ => Expr.quote(JsoniterDerivationUtils.unknownDiscriminator[A](Expr.splice(dctx.reader), null))
              val enumValueError: Expr[String] => Expr[A] =
                name => Expr.quote(Expr.splice(dctx.reader).enumValueError(Expr.splice(name)): A)
              val inlineError: Expr[A] = Expr.quote {
                JsoniterDerivationUtils.unknownDiscriminator[A](
                  Expr.splice(dctx.reader),
                  Expr.splice(dctx.config).discriminatorFieldName.orNull
                )
              }

              // Per project rule 5, [[LambdaBuilder]] is reserved for collection / Optional iteration lambdas.
              // The dispatch functions built here are spliced into runtime helper calls; each `dispatcher` is a
              // closure composing its body with the active Quotes, so a direct cross-quotes function literal works.
              def dispatchLambda(
                  dispatchers: List[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]],
                  error: Expr[String] => Expr[A]
              ): Expr[String => A] =
                Expr.quote { (typeName: String) =>
                  Expr.splice {
                    dispatchers.foldRight(error(Expr.quote(typeName))) { case (dispatcher, elseExpr) =>
                      dispatcher(Expr.quote(typeName), dctx.reader, elseExpr)
                    }
                  }
                }

              val intDispatchLambda: Expr[Int => A] =
                Expr.quote { (enumId: Int) =>
                  Expr.splice {
                    enumIdDispatchers.foldRight(
                      Expr.quote(Expr.splice(dctx.reader).decodeError("illegal enum value " + enumId): A)
                    ) { case (dispatcher, elseExpr) =>
                      dispatcher(Expr.quote(enumId), dctx.reader, elseExpr)
                    }
                  }
                }

              def inlineDispatchLambda: Expr[(String, Boolean) => A] =
                Expr.quote { (typeName: String, afterOpen: Boolean) =>
                  Expr.splice {
                    inlineDispatchers.foldRight(inlineError) { case (dispatcher, elseExpr) =>
                      dispatcher(Expr.quote(typeName), dctx.reader, Expr.quote(afterOpen), elseExpr)
                    }
                  }
                }

              if (isEnumerationOrJavaEnum) {
                // As in jsoniter-scala: Java enum and Enumeration values are always JSON strings (or the
                // Enumeration value's id with useScalaEnumValueId), whatever the discriminator settings.
                val byName = dispatchLambda(stringDispatchers, enumValueError)
                if (isScalaEnumeration) Expr.quote {
                  if (Expr.splice(dctx.config).useScalaEnumValueId)
                    JsoniterDerivationUtils
                      .readScalaEnumValueId[A](Expr.splice(dctx.reader), Expr.splice(dctx.stringifiedExpr))(
                        Expr.splice(intDispatchLambda)
                      )
                  else JsoniterDerivationUtils.readEnumAsString[A](Expr.splice(dctx.reader))(Expr.splice(byName))
                }
                else
                  Expr.quote(JsoniterDerivationUtils.readEnumAsString[A](Expr.splice(dctx.reader))(Expr.splice(byName)))
              } else {
                val wrapperFn = dispatchLambda(wrapperDispatchers, wrapperError)
                val inlineFn = inlineDispatchLambda
                // Singletons as strings: in wrapper mode (`"Leaf"`, unless circeLikeObjectEncoding), and with
                // enumAsStrings for ADTs of case objects only. Objects are accepted on decoding too.
                val stringFn: Option[Expr[String => A]] =
                  if (stringDispatchers.isEmpty) None
                  else
                    Some(
                      dispatchLambda(stringDispatchers, wrapperError)
                    ) // as jsoniter-scala: `illegal discriminator` (Java enums / Enumeration: `illegal enum value`)
                val enumAsStringsApplies: Expr[Boolean] =
                  if (allCaseObjects) Expr.quote(Expr.splice(dctx.config).enumAsStrings) else Expr(false)
                stringFn match {
                  case Some(strings) =>
                    Expr.quote {
                      val config = Expr.splice(dctx.config)
                      val reader = Expr.splice(dctx.reader)
                      if (reader.isNextToken('"'.toByte)) {
                        reader.rollbackToken()
                        if (config.discriminatorFieldName.isEmpty || Expr.splice(enumAsStringsApplies))
                          JsoniterDerivationUtils.readEnumAsString[A](reader)(Expr.splice(strings))
                        else reader.decodeError("expected '{'")
                      } else {
                        reader.rollbackToken()
                        config.discriminatorFieldName match {
                          case Some(field) =>
                            JsoniterDerivationUtils
                              .readWithDiscriminator[A](
                                reader,
                                field,
                                config.requireDiscriminatorFirst,
                                Expr.splice(markLeaves)
                              )(
                                Expr.splice(inlineFn)
                              )
                          case None => JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperFn))
                        }
                      }
                    }
                  case None =>
                    Expr.quote {
                      val config = Expr.splice(dctx.config)
                      val reader = Expr.splice(dctx.reader)
                      config.discriminatorFieldName match {
                        case Some(field) =>
                          JsoniterDerivationUtils
                            .readWithDiscriminator[A](
                              reader,
                              field,
                              config.requireDiscriminatorFirst,
                              Expr.splice(markLeaves)
                            )(
                              Expr.splice(inlineFn)
                            )
                        case None => JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperFn))
                      }
                    }
                }
              }
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      // A non-abstract sealed base as its own leaf: deriving it recursively would be this codec.
      val baseAsLeaf: Option[ProductType[ChildType]] =
        if (Type[ChildType] =:= Type[A]) ProductType.asLeaf[ChildType] else None

      if (baseAsLeaf.isDefined)
        DecoderHandleAsCaseClassRule
          .decodeProduct[ChildType](baseAsLeaf.get)(using dctx.nest[ChildType](dctx.reader))
          .map { decodedExpr => (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
            Expr.quote {
              if (Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr))
                Expr.splice(decodedExpr).asInstanceOf[A]
              else Expr.splice(elseExpr)
            }
          }
      else
        summonJsonValueCodecCached[ChildType] match {
          case Right(codecExpr) =>
            Log.info(s"Found implicit JsonValueCodec[$childName], using it") >>
              MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
                Expr.quote {
                  if (Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr))
                    Expr
                      .splice(codecExpr)
                      .decodeValue(Expr.splice(readerExpr), Expr.splice(codecExpr).nullValue)
                      .asInstanceOf[A]
                  else
                    Expr.splice(elseExpr)
                }
              }

          case Left(_) =>
            // Try singletonOf first — handles Enumeration values, Java enum values, case objects
            Expr.singletonOf[ChildType] match {
              case Some(singleton) =>
                Log.info(s"Using singleton for $childName") >>
                  MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
                    Expr.quote {
                      if (
                        Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr)
                      ) {
                        // Consume the inner empty object {} written by EncoderHandleAsSingletonRule
                        JsoniterDerivationUtils.readEmptyObject(Expr.splice(readerExpr))
                        Expr.splice(singleton).asInstanceOf[A]
                      } else
                        Expr.splice(elseExpr)
                    }
                  }
              case None =>
                // No singleton - derive via full rules chain
                deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader)).flatMap { decodedExpr =>
                  dctx.getHelper[ChildType].map {
                    case Some(helper) =>
                      (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) => {
                        val helperCallExpr = helper(readerExpr, dctx.config)
                        Expr.quote {
                          if (
                            Expr
                              .splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(
                              typeNameExpr
                            )
                          )
                            Expr.splice(helperCallExpr).asInstanceOf[A]
                          else
                            Expr.splice(elseExpr)
                        }
                      }

                    case None =>
                      // No helper registered (e.g., built-in types like String, Int) — use the derived expression directly
                      (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                        Expr.quote {
                          if (
                            Expr
                              .splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(
                              typeNameExpr
                            )
                          )
                            Expr.splice(decodedExpr).asInstanceOf[A]
                          else
                            Expr.splice(elseExpr)
                        }
                  }
                }
            }
        }
    }

    /** Derive an inline child decoder for discriminator mode. Uses decodeCaseClassFieldsInline to read fields from an
      * already-opened object.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderInline[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[Boolean], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean

      def matches(typeNameExpr: Expr[String]): Expr[Boolean] = Expr.quote {
        Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr)
      }

      if (hasUserCodecAsLeaf[ChildType]) {
        // As in jsoniter-scala: a user-provided codec of a leaf reads the whole object (discriminator included).
        val codecExpr = summonJsonValueCodecCached[ChildType].toOption.get
        MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], _: Expr[Boolean], elseExpr: Expr[A]) =>
          Expr.quote {
            if (Expr.splice(matches(typeNameExpr)))
              JsoniterDerivationUtils
                .readWholeObjectWith(Expr.splice(readerExpr), Expr.splice(codecExpr))
                .asInstanceOf[A]
            else Expr.splice(elseExpr)
          }
        }
      } else
        ProductType.asLeaf[ChildType] match {
          case Some(cc) if SingletonValue.unapply(Type[ChildType]).isEmpty =>
            DecoderHandleAsCaseClassRule
              .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
              .map {
                inlineDecode =>
                  (typeNameExpr: Expr[String], _: Expr[JsonReader], afterOpenExpr: Expr[Boolean], elseExpr: Expr[A]) =>
                    Expr.quote {
                      if (Expr.splice(matches(typeNameExpr))) Expr.splice(inlineDecode(afterOpenExpr)).asInstanceOf[A]
                      else Expr.splice(elseExpr)
                    }
              }

          case _ =>
            Expr.singletonOf[ChildType] match {
              case Some(singleton) =>
                // Singleton in discriminator mode: skip the rest of the object (jsoniter-scala skips the content of
                // objects decoded as case objects), including the discriminator when the object was re-read.
                Log.info(s"Using singleton for inline child $childName") >>
                  MIO.pure {
                    (
                        typeNameExpr: Expr[String],
                        readerExpr: Expr[JsonReader],
                        afterOpenExpr: Expr[Boolean],
                        elseExpr: Expr[A]
                    ) =>
                      Expr.quote {
                        if (Expr.splice(matches(typeNameExpr))) {
                          JsoniterDerivationUtils.readInlineObjectWithoutFields(
                            Expr.splice(readerExpr),
                            true,
                            null,
                            false,
                            Expr.splice(afterOpenExpr)
                          )
                          Expr.splice(singleton).asInstanceOf[A]
                        } else Expr.splice(elseExpr)
                      }
                  }
              case None =>
                // Non-singleton, non-case-class: fall back to wrapper-style decoder
                deriveChildDecoder[A, ChildType](childName).map {
                  wrapper =>
                    (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], _: Expr[Boolean], elseExpr: Expr[A]) =>
                      wrapper(typeNameExpr, readerExpr, elseExpr)
                }
            }
        }
    }

    private def hasUserCodecAsLeaf[ChildType: Type]: Boolean =
      SingletonValue.unapply(Type[ChildType]).isEmpty && summonJsonValueCodecCached[ChildType].isRight

    /** Derive a string-enum child decoder that returns the singleton instance directly without reading from the reader.
      * Used when all enum children are case objects and enumAsStrings is enabled.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderStringEnum[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      // Try SingletonValue first — handles Enumeration values, Java enum values, case objects
      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for string enum child $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr))
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          // Fall back to CaseClass.construct for zero-arg case classes
          CaseClass.parse[ChildType].toOption match {
            case Some(cc) if cc.primaryConstructor.totalParameters.flatten.isEmpty =>
              val constructMIO: MIO[Option[Expr[ChildType]]] =
                cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                  def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                    val err = CodecDerivationError.UnexpectedParameterInSingleton(
                      Type[ChildType].prettyPrint,
                      "Unexpected parameter in singleton"
                    )
                    Log.error(err.message) >> MIO.fail(err)
                  }
                })
              constructMIO.flatMap {
                case Some(instanceExpr) =>
                  MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                    Expr.quote {
                      if (
                        Expr.splice(enumLeafNameExpr[A, ChildType](childName, dctx.config)) == Expr.splice(typeNameExpr)
                      )
                        Expr.splice(instanceExpr).asInstanceOf[A]
                      else
                        Expr.splice(elseExpr)
                    }
                  }
                case None =>
                  val err = CodecDerivationError.CannotConstructType(Type[ChildType].prettyPrint, isSingleton = true)
                  Log.error(err.message) >> MIO.fail(err)
              }

            case _ =>
              // Not a zero-param case class — shouldn't happen when allCaseObjects is true
              val err = CodecDerivationError.UnexpectedParameterInSingleton(
                Type[ChildType].prettyPrint,
                "Expected singleton/case object for string enum but got"
              )
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }

    /** Derive an Int-based enum-id child decoder for useScalaEnumValueId support. Matches the child's .id at runtime.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderEnumId[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[Int], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val IntT: Type[Int] = CTypes.Int
      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for enum-id child $childName") >>
            MIO.pure { (idExpr: Expr[Int], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (Expr.splice(idExpr) == JsoniterDerivationUtils.scalaEnumValueId(Expr.splice(sv.singletonExpr)))
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          MIO.pure { (_: Expr[Int], _: Expr[JsonReader], elseExpr: Expr[A]) =>
            elseExpr
          }
      }
    }
  }

}
