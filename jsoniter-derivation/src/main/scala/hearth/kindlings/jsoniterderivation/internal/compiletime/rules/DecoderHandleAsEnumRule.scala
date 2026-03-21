package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonCodec, KindlingsJsonValueCodec}
import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonCodec,
  JsonKeyCodec,
  JsonReader,
  JsonReaderException,
  JsonValueCodec,
  JsonWriter
}

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

      val childrenList = enumm.directChildren.toList

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
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
          val knownNames: List[String] = children.toList.map(_._1)

          val isScalaEnumeration = Type[A].isEnumeration

          // For each child, derive BOTH wrapper-mode and inline (discriminator-mode) decoders,
          // and optionally string-enum dispatchers for case-object children
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              val isSingleton = SingletonValue.unapply(Type[ChildType]).isDefined || isScalaEnumeration ||
                Type[A].isJavaEnum
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <- deriveChildDecoder[A, ChildType](childName)
                  inline <- deriveChildDecoderInline[A, ChildType](childName)
                  stringEnum <-
                    if (allCaseObjects || isSingleton)
                      deriveChildDecoderStringEnum[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                  enumId <-
                    if (allCaseObjects && isScalaEnumeration)
                      deriveChildDecoderEnumId[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                  circeLikeWrapper <-
                    if (isSingleton)
                      deriveChildDecoderCirceLikeWrapper[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                } yield (wrapper, inline, stringEnum, enumId, circeLikeWrapper)
              }
            }
            .flatMap { allDispatchers =>
              val wrapperDispatchers = allDispatchers.toList.map(_._1)
              val inlineDispatchers = allDispatchers.toList.map(_._2)

              implicit val IntT: Type[Int] = CTypes.Int

              def buildErrorExpr(typeNameExpr: Expr[String]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown type discriminator: " + Expr.splice(typeNameExpr) +
                      ". Expected one of: " + Expr.splice(Expr(knownNames)).mkString(", ")
                  ): A
              }

              def buildDispatchLambda(
                  dispatchers: List[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[String => A]] =
                LambdaBuilder
                  .of1[String]("typeName")
                  .traverse { typeNameExpr =>
                    MIO.pure(dispatchers.foldRight(buildErrorExpr(typeNameExpr)) { case (dispatcher, elseExpr) =>
                      dispatcher(typeNameExpr, dctx.reader, elseExpr)
                    })
                  }
                  .map(_.build[A])

              def buildIntErrorExpr(idExpr: Expr[Int]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown enum value id: " + Expr.splice(idExpr)
                  ): A
              }

              def buildIntDispatchLambda(
                  dispatchers: List[(Expr[Int], Expr[JsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[Int => A]] =
                LambdaBuilder
                  .of1[Int]("enumId")
                  .traverse { idExpr =>
                    MIO.pure(dispatchers.foldRight(buildIntErrorExpr(idExpr)) { case (dispatcher, elseExpr) =>
                      dispatcher(idExpr, dctx.reader, elseExpr)
                    })
                  }
                  .map(_.build[A])

              for {
                wrapperDispatchFn <- buildDispatchLambda(wrapperDispatchers)
                inlineDispatchFn <- buildDispatchLambda(inlineDispatchers)
                result <-
                  if (allCaseObjects) {
                    val stringEnumDispatchers = allDispatchers.toList.flatMap(_._3)
                    val enumIdDispatchers = allDispatchers.toList.flatMap(_._4)
                    for {
                      stringEnumDispatchFn <- buildDispatchLambda(stringEnumDispatchers)
                      enumIdDispatchFnOpt <-
                        if (enumIdDispatchers.nonEmpty) buildIntDispatchLambda(enumIdDispatchers).map(Some(_))
                        else MIO.pure(None)
                    } yield enumIdDispatchFnOpt match {
                      case Some(enumIdDispatchFn) =>
                        // Scala Enumeration with useScalaEnumValueId support
                        Expr.quote {
                          val config = Expr.splice(dctx.config)
                          val reader = Expr.splice(dctx.reader)
                          if (config.useScalaEnumValueId)
                            JsoniterDerivationUtils.readScalaEnumValueId[A](reader)(
                              Expr.splice(enumIdDispatchFn)
                            )
                          else if (config.enumAsStrings)
                            JsoniterDerivationUtils.readEnumAsString[A](reader)(
                              Expr.splice(stringEnumDispatchFn)
                            )
                          else
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                        }
                      case None =>
                        // Non-Enumeration all-case-objects enum
                        Expr.quote {
                          val config = Expr.splice(dctx.config)
                          val reader = Expr.splice(dctx.reader)
                          if (config.enumAsStrings)
                            JsoniterDerivationUtils.readEnumAsString[A](reader)(
                              Expr.splice(stringEnumDispatchFn)
                            )
                          else
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                        }
                    }
                  } else {
                    // Mixed enum (case objects + case classes)
                    // Build circe-like combined dispatchers: singletons consume inner {},
                    // case classes use regular wrapper dispatch
                    val circeLikeDispatchers = allDispatchers.toList.map { t =>
                      t._5.getOrElse(t._1) // circe-like wrapper for singletons, regular wrapper for case classes
                    }
                    val hasSingletons = allDispatchers.toList.exists(_._5.isDefined)
                    (if (hasSingletons)
                       buildDispatchLambda(circeLikeDispatchers).map(Some(_))
                     else MIO.pure(None)).map { circeLikeDispatchFnOpt =>
                      circeLikeDispatchFnOpt match {
                        case Some(circeLikeDispatchFn) =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            val reader = Expr.splice(dctx.reader)
                            if (config.circeLikeObjectEncoding)
                              JsoniterDerivationUtils.readWrapped[A](reader)(
                                Expr.splice(circeLikeDispatchFn)
                              )
                            else
                              config.discriminatorFieldName match {
                                case Some(field) =>
                                  JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                    Expr.splice(inlineDispatchFn)
                                  )
                                case None =>
                                  JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                              }
                          }
                        case None =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            val reader = Expr.splice(dctx.reader)
                            config.discriminatorFieldName match {
                              case Some(field) =>
                                JsoniterDerivationUtils.readWithDiscriminator[A](reader, field)(
                                  Expr.splice(inlineDispatchFn)
                                )
                              case None =>
                                JsoniterDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                            }
                          }
                      }
                    }
                  }
              } yield result
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      CTypes
        .JsonValueCodec[ChildType]
        .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(codecExpr) =>
          Log.info(s"Found implicit JsonValueCodec[$childName], using it") >>
            MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
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
                MIO.pure { (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      Expr.splice(singleton).asInstanceOf[A]
                    else
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
                          Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
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
                          Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
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
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      CaseClass.parse[ChildType].toOption match {
        case Some(cc) =>
          DecoderHandleAsCaseClassRule
            .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
            .map { inlineExpr => (typeNameExpr: Expr[String], _: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  Expr.splice(inlineExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }

        case None =>
          // Not a case class (e.g., case object) — fall back to wrapper-style decoder
          deriveChildDecoder[A, ChildType](childName)
      }
    }

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
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          // Fall back to CaseClass.construct for zero-arg case classes
          CaseClass.parse[ChildType].toOption match {
            case Some(cc) if cc.primaryConstructor.parameters.flatten.isEmpty =>
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
                        Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                          .splice(typeNameExpr)
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

    /** Derive a circe-like wrapper child decoder for singleton children. Similar to deriveChildDecoderStringEnum but
      * reads an empty JSON object {} before returning the singleton value, matching circe's encoding of case objects as
      * {"TypeName":{}}.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderCirceLikeWrapper[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[JsonReader], Expr[A]) => Expr[A]] = {
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for circe-like wrapper child $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                ) {
                  JsoniterDerivationUtils.readEmptyObject(Expr.splice(readerExpr))
                  Expr.splice(sv.singletonExpr).asInstanceOf[A]
                } else
                  Expr.splice(elseExpr)
              }
            }
        case None =>
          // Fall back to CaseClass.construct for zero-arg case classes
          CaseClass.parse[ChildType].toOption match {
            case Some(cc) if cc.primaryConstructor.parameters.flatten.isEmpty =>
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
                  MIO.pure { (typeNameExpr: Expr[String], readerExpr: Expr[JsonReader], elseExpr: Expr[A]) =>
                    Expr.quote {
                      if (
                        Expr.splice(dctx.config).adtLeafClassNameMapper(Expr.splice(Expr(childName))) == Expr
                          .splice(typeNameExpr)
                      ) {
                        JsoniterDerivationUtils.readEmptyObject(Expr.splice(readerExpr))
                        Expr.splice(instanceExpr).asInstanceOf[A]
                      } else
                        Expr.splice(elseExpr)
                    }
                  }
                case None =>
                  val err = CodecDerivationError.CannotConstructType(Type[ChildType].prettyPrint, isSingleton = true)
                  Log.error(err.message) >> MIO.fail(err)
              }

            case _ =>
              val err = CodecDerivationError.UnexpectedParameterInSingleton(
                Type[ChildType].prettyPrint,
                "Expected singleton/case object for circe-like wrapper but got"
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

  // Types

  private[compiletime] object CTypes {

    def JsonCodec: Type.Ctor1[JsonCodec] = Type.Ctor1.of[JsonCodec]
    def JsonKeyCodec: Type.Ctor1[JsonKeyCodec] = Type.Ctor1.of[JsonKeyCodec]
    def JsonValueCodec: Type.Ctor1[JsonValueCodec] = Type.Ctor1.of[JsonValueCodec]
    def KindlingsJsonCodec: Type.Ctor1[KindlingsJsonCodec] = Type.Ctor1.of[KindlingsJsonCodec]
    def KindlingsJsonValueCodec: Type.Ctor1[KindlingsJsonValueCodec] =
      Type.Ctor1.of[KindlingsJsonValueCodec]
    val CodecLogDerivation: Type[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation] =
      Type.of[hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.LogDerivation]
    val JsoniterConfig: Type[JsoniterConfig] = Type.of[JsoniterConfig]
    val JsonReader: Type[JsonReader] = Type.of[JsonReader]
    val JsonWriter: Type[JsonWriter] = Type.of[JsonWriter]
    val String: Type[String] = Type.of[String]
    val Unit: Type[Unit] = Type.of[Unit]
    val Any: Type[Any] = Type.of[Any]
    val ArrayAny: Type[Array[Any]] = Type.of[Array[Any]]
    val ListString: Type[List[String]] = Type.of[List[String]]
    val JsonReaderException: Type[JsonReaderException] = Type.of[JsonReaderException]
    def EitherJsonReaderException[A: Type]: Type[Either[JsonReaderException, A]] =
      Type.of[Either[JsonReaderException, A]]
    val FieldName: Type[fieldNameAnn] = Type.of[fieldNameAnn]
    val TransientField: Type[transientField] = Type.of[transientField]
    val Stringified: Type[stringified] = Type.of[stringified]
    val Int: Type[Int] = Type.of[Int]
    val Long: Type[Long] = Type.of[Long]
    val Double: Type[Double] = Type.of[Double]
    val Float: Type[Float] = Type.of[Float]
    val Short: Type[Short] = Type.of[Short]
    val Byte: Type[Byte] = Type.of[Byte]
    val Boolean: Type[Boolean] = Type.of[Boolean]
    val BigDecimal: Type[BigDecimal] = Type.of[BigDecimal]
    val BigInt: Type[BigInt] = Type.of[BigInt]
    val Product: Type[Product] = Type.of[Product]
    val Instant: Type[java.time.Instant] = Type.of[java.time.Instant]
    val LocalDate: Type[java.time.LocalDate] = Type.of[java.time.LocalDate]
    val LocalTime: Type[java.time.LocalTime] = Type.of[java.time.LocalTime]
    val LocalDateTime: Type[java.time.LocalDateTime] = Type.of[java.time.LocalDateTime]
    val OffsetDateTime: Type[java.time.OffsetDateTime] = Type.of[java.time.OffsetDateTime]
    val ZonedDateTime: Type[java.time.ZonedDateTime] = Type.of[java.time.ZonedDateTime]
    val Duration: Type[java.time.Duration] = Type.of[java.time.Duration]
    val Period: Type[java.time.Period] = Type.of[java.time.Period]
  }

}
