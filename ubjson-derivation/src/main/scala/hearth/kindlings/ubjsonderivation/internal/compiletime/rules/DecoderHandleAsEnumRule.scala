package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait DecoderHandleAsEnumRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            // Note: caching is handled by deriveDecoderRecursively
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val StringT: Type[String] = CTypes.String
      implicit val ListStringT: Type[List[String]] = CTypes.ListString

      val childrenList = enumm.directChildren.toList

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

          // For each child, derive wrapper-mode and inline (discriminator) decoders
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                for {
                  wrapper <- deriveChildDecoder[A, ChildType](childName)
                  inline <- deriveChildDecoderInline[A, ChildType](childName)
                  stringEnum <-
                    if (allCaseObjects)
                      deriveChildDecoderStringEnum[A, ChildType](childName).map(Some(_))
                    else MIO.pure(None)
                } yield (wrapper, inline, stringEnum)
              }
            }
            .flatMap { allDispatchers =>
              val wrapperDispatchers = allDispatchers.toList.map(_._1)
              val inlineDispatchers = allDispatchers.toList.map(_._2)

              def buildErrorExpr(typeNameExpr: Expr[String]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown type discriminator: " + Expr.splice(typeNameExpr) +
                      ". Expected one of: " + Expr.splice(Expr(knownNames)).mkString(", ")
                  ): A
              }

              // Per project rule 5, [[LambdaBuilder]] is reserved for collection / Optional iteration lambdas.
              // The enum dispatch function we build here is a `String => A` value spliced into a runtime
              // helper call (`readEnumAsString` etc.); each `dispatcher` is itself a closure over Exprs that
              // composes its body using the active Quotes when invoked, so a direct cross-quotes function
              // literal works.
              def buildDispatchLambda(
                  dispatchers: List[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]]
              ): MIO[Expr[String => A]] =
                MIO.pure(Expr.quote { (typeName: String) =>
                  Expr.splice {
                    dispatchers.foldRight(buildErrorExpr(Expr.quote(typeName))) { case (dispatcher, elseExpr) =>
                      dispatcher(Expr.quote(typeName), dctx.reader, elseExpr)
                    }
                  }
                })

              for {
                wrapperDispatchFn <- buildDispatchLambda(wrapperDispatchers)
                inlineDispatchFn <- buildDispatchLambda(inlineDispatchers)
                result <-
                  if (allCaseObjects) {
                    val stringEnumDispatchers = allDispatchers.toList.flatMap(_._3)
                    buildDispatchLambda(stringEnumDispatchers).map { stringEnumDispatchFn =>
                      Expr.quote {
                        val config = Expr.splice(dctx.config)
                        val reader = Expr.splice(dctx.reader)
                        if (config.enumAsStrings)
                          UBJsonDerivationUtils.readEnumAsString[A](reader)(
                            Expr.splice(stringEnumDispatchFn)
                          )
                        else
                          config.discriminatorFieldName match {
                            case Some(field) =>
                              UBJsonDerivationUtils.readWithDiscriminator[A](reader, field)(
                                Expr.splice(inlineDispatchFn)
                              )
                            case None =>
                              UBJsonDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
                          }
                      }
                    }
                  } else {
                    MIO.pure {
                      Expr.quote {
                        val config = Expr.splice(dctx.config)
                        val reader = Expr.splice(dctx.reader)
                        config.discriminatorFieldName match {
                          case Some(field) =>
                            UBJsonDerivationUtils.readWithDiscriminator[A](reader, field)(
                              Expr.splice(inlineDispatchFn)
                            )
                          case None =>
                            UBJsonDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperDispatchFn))
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
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      Expr.singletonOf[ChildType] match {
        case Some(singleton) =>
          Log.info(s"Using singleton for $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
          deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader)).flatMap { decodedExpr =>
            dctx.getHelper[ChildType].map {
              case Some(helper) =>
                (typeNameExpr: Expr[String], readerExpr: Expr[UBJsonReader], elseExpr: Expr[A]) => {
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
                (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderInline[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      CaseClass.parse[ChildType].toOption match {
        case Some(cc) =>
          DecoderHandleAsCaseClassRule
            .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
            .map { inlineExpr => (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
          deriveChildDecoder[A, ChildType](childName)
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoderStringEnum[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]] = {
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      SingletonValue.unapply(Type[ChildType]) match {
        case Some(sv) =>
          Log.info(s"Using singleton for string enum child $childName") >>
            MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
                  MIO.pure { (typeNameExpr: Expr[String], _: Expr[UBJsonReader], elseExpr: Expr[A]) =>
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
              val err = CodecDerivationError.UnexpectedParameterInSingleton(
                Type[ChildType].prettyPrint,
                "Expected singleton/case object for string enum but got"
              )
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }
  }

}
