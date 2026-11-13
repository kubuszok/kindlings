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
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean

      // As in jsoniter-scala, nested sealed hierarchies are flattened to their leaves.
      val childrenList = adtLeaves[A](enumm)
      val isValueEnum = Type[A].isEnumeration || isJavaEnumType[A]
      val allCaseObjects = isValueEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Expr
              .splice(dctx.reader)
              .decodeError("Enum " + Expr.splice(Expr(Type[A].prettyPrint)) + " has no subtypes"): A
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)
          val skipUnexpected: Expr[Boolean] = dctx.evaluatedConfig match {
            case Some(c) => Expr(c.skipUnexpectedFields)
            case None    => Expr.quote(Expr.splice(dctx.config).skipUnexpectedFields)
          }

          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              val nameExpr = enumLeafNameExpr[A, ChildType](childName, dctx.config, dctx.evaluatedConfig)
              def matching(
                  value: Expr[String] => Expr[ChildType] => Expr[A]
              )(found: Expr[ChildType]): ((Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]) =
                (typeName, _, orElse) =>
                  Expr.quote {
                    if (Expr.splice(nameExpr) == Expr.splice(typeName)) Expr.splice(value(typeName)(found))
                    else Expr.splice(orElse)
                  }
              val asA: Expr[String] => Expr[ChildType] => Expr[A] = _ => e => Expr.quote(Expr.splice(e).asInstanceOf[A])
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                SingletonValue.unapply(Type[ChildType]) match {
                  case Some(sv) =>
                    val singleton = sv.singletonExpr
                    // string: the value itself; wrapper: `{"Leaf":{}}`; discriminator: `{"type":"Leaf"}`
                    val asString = Some(matching(asA)(singleton))
                    val wrapper: ((Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]) =
                      (typeName, reader, orElse) =>
                        Expr.quote {
                          if (Expr.splice(nameExpr) == Expr.splice(typeName)) {
                            UBJsonDerivationUtils.readObjectWithoutFields(
                              Expr.splice(reader),
                              Expr.splice(skipUnexpected),
                              false
                            )
                            Expr.splice(singleton).asInstanceOf[A]
                          } else Expr.splice(orElse)
                        }
                    val inline: ((Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A]) = (typeName, reader, orElse) =>
                      Expr.quote {
                        if (Expr.splice(nameExpr) == Expr.splice(typeName)) {
                          UBJsonDerivationUtils.readObjectWithoutFields(
                            Expr.splice(reader),
                            Expr.splice(skipUnexpected),
                            true
                          )
                          Expr.splice(singleton).asInstanceOf[A]
                        } else Expr.splice(orElse)
                      }
                    MIO.pure((wrapper, inline, asString))
                  case None =>
                    // A non-abstract sealed base as its own leaf: deriving it recursively would be this codec.
                    val baseAsLeaf: Option[ProductType[ChildType]] =
                      if (Type[ChildType] =:= Type[A]) ProductType.asLeaf[ChildType] else None
                    val wrapperMIO: MIO[Expr[ChildType]] = baseAsLeaf match {
                      case Some(product) =>
                        DecoderHandleAsCaseClassRule
                          .decodeCaseClassFields[ChildType](product)(using dctx.nest[ChildType](dctx.reader))
                      case None => deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.reader))
                    }
                    for {
                      wrapperValue <- wrapperMIO
                      inlineValue <- (ProductType.asLeaf[ChildType] match {
                        case Some(cc) =>
                          DecoderHandleAsCaseClassRule
                            .decodeCaseClassFieldsInline[ChildType](cc)(using dctx.nest[ChildType](dctx.reader))
                        case None => MIO.pure(wrapperValue)
                      }): MIO[Expr[ChildType]]
                    } yield (matching(asA)(wrapperValue), matching(asA)(inlineValue), None)
                }
              }
            }
            .flatMap { allDispatchers =>
              val all = allDispatchers.toList

              def buildErrorExpr(typeNameExpr: Expr[String]): Expr[A] = Expr.quote {
                Expr
                  .splice(dctx.reader)
                  .decodeError(
                    "Unknown type discriminator: " + Expr.splice(typeNameExpr) +
                      ". Expected one of: " + Expr.splice(Expr(knownNames)).mkString(", ")
                  ): A
              }

              // Per project rule 5, [[LambdaBuilder]] is reserved for collection / Optional iteration lambdas; the
              // `String => A` dispatch function is a direct cross-quotes function literal.
              def buildDispatchLambda(
                  dispatchers: List[((Expr[String], Expr[UBJsonReader], Expr[A]) => Expr[A])]
              ): Expr[String => A] =
                Expr.quote { (typeName: String) =>
                  Expr.splice {
                    dispatchers.foldRight(buildErrorExpr(Expr.quote(typeName))) { case (dispatcher, elseExpr) =>
                      dispatcher(Expr.quote(typeName), dctx.reader, elseExpr)
                    }
                  }
                }

              val wrapperFn = buildDispatchLambda(all.map(_._1))
              val inlineFn = buildDispatchLambda(all.map(_._2))
              val stringFn = buildDispatchLambda(all.flatMap(_._3))
              val enumAsStrings: Expr[Boolean] =
                if (allCaseObjects) Expr.quote(Expr.splice(dctx.config).enumAsStrings) else Expr(false)

              MIO.pure {
                if (isValueEnum) {
                  // As in jsoniter-scala: Java enum and Enumeration values are always strings (Enumeration values are
                  // their ids with useScalaEnumValueId)
                  val byName: Expr[A] =
                    Expr.quote(
                      UBJsonDerivationUtils.readEnumAsString[A](Expr.splice(dctx.reader))(Expr.splice(stringFn))
                    )
                  if (!Type[A].isEnumeration) byName
                  else {
                    implicit val IntT: Type[Int] = CTypes.Int
                    val byId: Expr[A] = {
                      val ids: List[(Expr[A])] = childrenList.flatMap { case (_, child) =>
                        SingletonValue.unapply(child.Underlying).map(_.singletonExpr.asInstanceOf[Expr[A]])
                      }
                      val dispatch: Expr[Int => A] = Expr.quote { (id: Int) =>
                        Expr.splice {
                          ids.foldRight(
                            Expr.quote(Expr.splice(dctx.reader).decodeError("illegal enum value " + id): A)
                          ) { (value, elseExpr) =>
                            Expr.quote {
                              if (id == UBJsonDerivationUtils.scalaEnumValueId(Expr.splice(value))) Expr.splice(value)
                              else Expr.splice(elseExpr)
                            }
                          }
                        }
                      }
                      Expr.quote(
                        UBJsonDerivationUtils.readScalaEnumValueId[A](Expr.splice(dctx.reader))(Expr.splice(dispatch))
                      )
                    }
                    dctx.evaluatedConfig.map(_.useScalaEnumValueId) match {
                      case Some(true)  => byId
                      case Some(false) => byName
                      case None        =>
                        Expr.quote(
                          if (Expr.splice(dctx.config).useScalaEnumValueId) Expr.splice(byId) else Expr.splice(byName)
                        )
                    }
                  }
                } else
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val reader = Expr.splice(dctx.reader)
                    if (Expr.splice(enumAsStrings))
                      UBJsonDerivationUtils.readEnumAsString[A](reader)(Expr.splice(stringFn))
                    else
                      config.discriminatorFieldName match {
                        case Some(field) =>
                          if (config.requireDiscriminatorFirst)
                            UBJsonDerivationUtils.readWithDiscriminator[A](reader, field)(Expr.splice(inlineFn))
                          else
                            UBJsonDerivationUtils.readWithDiscriminatorAnywhere[A](reader, field)(Expr.splice(inlineFn))
                        case None =>
                          // Case objects are strings in wrapper mode (unless circeLikeObjectEncoding)
                          if (!config.circeLikeObjectEncoding && reader.peekToken() == 'S'.toByte)
                            UBJsonDerivationUtils.readEnumAsString[A](reader)(Expr.splice(stringFn))
                          else UBJsonDerivationUtils.readWrapped[A](reader)(Expr.splice(wrapperFn))
                      }
                  }
              }
            }
      }
    }
  }
}
