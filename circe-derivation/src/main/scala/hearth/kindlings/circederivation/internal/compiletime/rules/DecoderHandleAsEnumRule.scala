package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.{Validated, ValidatedNel}
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                decodeEnumCases[A](enumm)(using dctx.nestInCache(cursor, config, failFast))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    Expr
                      .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  }))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Any]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val ListStringT: Type[List[String]] = DTypes.ListString
      implicit val TupleT: Type[(String, HCursor)] = DTypes.StringHCursorTuple
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]

      val childrenList = enumm.directChildren.toList

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            val err = DecodingFailure(
              s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes",
              Expr.splice(dctx.cursor).history
            )
            (if (Expr.splice(dctx.failFast))
               Left(err): Either[DecodingFailure, A]
             else
               Validated.invalidNel(err): ValidatedNel[DecodingFailure, A]): Any
          })

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)

          // For each child, derive a decoder and produce a dispatch function
          // that takes (typeNameExpr, innerCursorExpr, elseExpr) and returns Any
          // (Either when failFast=true, ValidatedNel when failFast=false)
          children
            .parTraverse { case (childName, child) =>
              import child.Underlying as ChildType
              Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                deriveChildDecoder[A, ChildType](childName)
              }
            }
            .flatMap { childDispatchers =>
              // Build a dispatch lambda: (String, HCursor) => Any
              LambdaBuilder
                .of1[(String, HCursor)]("readResult")
                .traverse { readResultExpr =>
                  // Extract typeName and innerCursor from the tuple
                  val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                  val innerCursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(readResultExpr)._2)

                  // Build the if-else dispatch chain (foldRight to get correct order)
                  val errorExpr: Expr[Any] = Expr.quote {
                    val failure = CirceDerivationUtils.failedToMatchSubtype(
                      Expr.splice(typeNameExpr),
                      Expr.splice(innerCursorExpr),
                      Expr.splice(Expr(knownNames))
                    )
                    (if (Expr.splice(dctx.failFast))
                       Left(failure): Either[DecodingFailure, A]
                     else
                       Validated.invalidNel(failure): ValidatedNel[DecodingFailure, A]): Any
                  }

                  MIO.pure(childDispatchers.toList.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                    dispatcher(typeNameExpr, innerCursorExpr, elseExpr)
                  })
                }
                .map { builder =>
                  val dispatchFn = builder.build[Any]
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    val cursor = Expr.splice(dctx.cursor)
                    val failFast = Expr.splice(dctx.failFast)
                    if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings) {
                      // String enum decode path: read plain string, dispatch on name
                      cursor.as[String](io.circe.Decoder.decodeString) match {
                        case Right(typeName) =>
                          Expr.splice(dispatchFn)((typeName, cursor))
                        case Left(_) =>
                          val err = DecodingFailure("Expected a JSON string for enum value", cursor.history)
                          (if (failFast)
                             Left(err): Either[DecodingFailure, A]
                           else
                             Validated.invalidNel(err): ValidatedNel[DecodingFailure, A]): Any
                      }
                    } else {
                      val readResult: Either[DecodingFailure, (String, HCursor)] =
                        config.discriminator match {
                          case Some(field) => CirceDerivationUtils.decodeDiscriminator(cursor, field)
                          case None        => CirceDerivationUtils.decodeWrapped(cursor)
                        }
                      if (failFast) {
                        readResult.flatMap { r =>
                          Expr.splice(dispatchFn)(r).asInstanceOf[Either[DecodingFailure, A]]
                        }: Any
                      } else {
                        (readResult match {
                          case Right(r) => Expr.splice(dispatchFn)(r)
                          case Left(e)  => Validated.invalidNel(e): ValidatedNel[DecodingFailure, A]
                        }): Any
                      }
                    }
                  }
                }
            }
      }
    }

    /** Derives a decoder for a single enum child type and returns a dispatch function. The dispatch function takes
      * (typeNameExpr, innerCursorExpr, elseExpr) and produces an if-else expression that checks if the type name
      * matches and decodes accordingly. Returns Any (Either when failFast=true, ValidatedNel when failFast=false).
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildDecoder[A: DecoderCtx, ChildType: Type](
        childName: String
    ): MIO[(Expr[String], Expr[HCursor], Expr[Any]) => Expr[Any]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]

      // Try to summon implicit Decoder[ChildType] first
      DTypes
        .Decoder[ChildType]
        .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(decoderExpr) =>
          Log.info(s"Found implicit Decoder[$childName], using it") >>
            MIO.pure { (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) =>
              Expr.quote {
                if (
                  Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                    .splice(typeNameExpr)
                )
                  (if (Expr.splice(dctx.failFast))
                     Expr
                       .splice(decoderExpr)
                       .apply(Expr.splice(innerCursorExpr))
                       .asInstanceOf[Either[DecodingFailure, A]]
                   else
                     (Expr
                       .splice(decoderExpr)
                       .decodeAccumulating(Expr.splice(innerCursorExpr)): Any)
                       .asInstanceOf[ValidatedNel[DecodingFailure, A]]): Any
                else
                  Expr.splice(elseExpr)
              }
            }

        case Left(_) =>
          // Try singletonOf first — handles Enumeration values, Java enum values, case objects
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              Log.info(s"Using singleton for $childName") >>
                MIO.pure { (typeNameExpr: Expr[String], _: Expr[HCursor], elseExpr: Expr[Any]) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    )
                      (if (Expr.splice(dctx.failFast))
                         Right(Expr.splice(singleton).asInstanceOf[A]): Either[DecodingFailure, A]
                       else
                         Validated.valid(Expr.splice(singleton).asInstanceOf[A]): ValidatedNel[DecodingFailure, A]): Any
                    else
                      Expr.splice(elseExpr)
                  }
                }
            case None =>
              // No singleton - derive via full rules chain (this sets up a helper in cache)
              deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.cursor)).flatMap { _ =>
                dctx.getHelper[ChildType].map {
                  case Some(helper) =>
                    (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) => {
                      val helperCallExpr = helper(innerCursorExpr, dctx.config, dctx.failFast)
                      Expr.quote {
                        if (
                          Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                            .splice(typeNameExpr)
                        )
                          Expr.splice(helperCallExpr) // already returns Any (Either or ValidatedNel based on failFast)
                        else
                          Expr.splice(elseExpr)
                      }
                    }

                  case None =>
                    // No helper - the child was handled by implicit or value type rule
                    // This shouldn't normally happen since we checked implicit above
                    (_: Expr[String], _: Expr[HCursor], elseExpr: Expr[Any]) => elseExpr
                }
              }
          }
      }
    }
  }
}
