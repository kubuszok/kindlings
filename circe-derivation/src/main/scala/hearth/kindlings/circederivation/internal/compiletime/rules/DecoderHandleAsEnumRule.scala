package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.{Validated, ValidatedNel}
import io.circe.{Decoder, DecodingFailure, HCursor}

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
                    CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
                      Expr.splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                    )
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
      implicit val DecoderAnyT: Type[Decoder[Any]] = DTypes.DecoderAny
      implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny

      val hasUserDecoder: ?? => Boolean = child => userDecoderOf(child).isDefined

      // Nested sealed hierarchies are flattened like upstream circe: leaves are dispatched on by their own names.
      val childrenList: List[(String, ??)] = decoderSumEntries[A](enumm)

      def isSingletonEntry(child: ??): Boolean = SingletonValue.unapply(child.Underlying).isDefined

      // Check at compile time if all children are singletons (case objects with no fields)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum || childrenList.forall { case (_, child) =>
        isSingletonEntry(child)
      }
      // Like circe's ConfiguredEnumCodec (`summonSingletonCase` recurses into nested sums and ignores their instances),
      // enumAsStrings looks at the leaves: a hierarchy of case objects whose intermediates have user-provided instances
      // is still an enum, decoded from the leaf names.
      val leafEntries: List[(String, ??)] =
        if (allCaseObjects) childrenList else flattenedChildren[A](enumm)(_ => false)
      val allLeavesSingletons = allCaseObjects || leafEntries.forall { case (_, child) => isSingletonEntry(child) }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(
            Expr.quote {
              val err = DecodingFailure(
                s"Enum ${Expr.splice(Expr(Type[A].prettyPrint))} has no subtypes",
                Expr.splice(dctx.cursor).history
              )
              (if (Expr.splice(dctx.failFast))
                 Left(err): Either[DecodingFailure, A]
               else
                 Validated.invalidNel(err): ValidatedNel[DecodingFailure, A]): Any
            }
          )

        case Some(_) if !allLeavesSingletons && evaluatedConfigFlags.exists(_.enumAsStrings) =>
          // Like circe's ConfiguredEnumCodec, which only derives for sums of singletons.
          val err = DecoderDerivationError.EnumAsStringsWithNonSingletonCases(
            Type[A].prettyPrint,
            leafEntries.collect { case (name, child) if !isSingletonEntry(child) => name }
          )
          Log.error(err.message) >> MIO.fail(err)

        case Some(children) =>
          val knownNames: List[String] = children.toList.map(_._1)
          // circe's `constructorNames` (the direct subtypes) for the strict wrapper failure.
          val directNames: List[String] = directChildNames[A](enumm)
          // circe names a sum by its simple name (`Mirror.MirroredLabel`) in failures.
          val sumNameExpr: Expr[String] = Expr(Type[A].shortName)
          // User-provided instances of intermediate sums: those derived by Kindlings handle their own subtypes' names.
          val withIntermediates = hasIntermediateDecoders(childrenList)
          val intermediatesExpr: Expr[List[Decoder[Any]]] = intermediateDecodersExpr(childrenList)
          // Children that may not encode to a JSON object (value types, user-provided instances): with a discriminator
          // they are written in the wrapped form (see `CirceDerivationUtils.addDiscriminator`) and read back from it.
          val nonObjectNames: List[String] = children.toList.collect {
            case (name, child) if {
                  import child.Underlying as C
                  val isValueType = Type[C] match {
                    case IsValueType(_) => !Type[C].isNamedTuple
                    case _              => false
                  }
                  isValueType || hasUserDecoder(child) ||
                  (CaseClass.parse[C].toOption.isEmpty && SingletonValue.unapply(Type[C]).isEmpty)
                } =>
              name
          }

          // Builds a dispatch lambda `((String, HCursor)) => Any` (Either when failFast=true, ValidatedNel when
          // failFast=false) over the given entries.
          def buildDispatch(
              entries: NonEmptyList[(String, ??)],
              entriesAreCaseObjects: Boolean,
              delegateToIntermediates: Boolean
          ): MIO[Expr[((String, HCursor)) => Any]] =
            entries
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                  deriveChildDecoder[A, ChildType](childName, entriesAreCaseObjects)
                }
              }
              .flatMap { childDispatchers =>
                LambdaBuilder
                  .of1[(String, HCursor)]("readResult")
                  .traverse { readResultExpr =>
                    // Extract typeName and innerCursor from the tuple
                    val typeNameExpr: Expr[String] = Expr.quote(Expr.splice(readResultExpr)._1)
                    val innerCursorExpr: Expr[HCursor] = Expr.quote(Expr.splice(readResultExpr)._2)

                    val unknownExpr: Expr[Any] = Expr.quote {
                      val failure =
                        if (Expr.splice(Expr(entriesAreCaseObjects)) && Expr.splice(dctx.config).enumAsStrings)
                          CirceDerivationUtils.unknownEnumCase(
                            Expr.splice(sumNameExpr),
                            Expr.splice(typeNameExpr),
                            Expr.splice(innerCursorExpr)
                          )
                        else
                          CirceDerivationUtils.failedToMatchSubtype(
                            Expr.splice(sumNameExpr),
                            Expr.splice(typeNameExpr),
                            Expr.splice(innerCursorExpr)
                          )
                      CirceDerivationUtils.failResult(Expr.splice(dctx.failFast), failure)
                    }
                    // A name of a subtype of a Kindlings-derived intermediate: that intermediate decodes the whole
                    // value (its own instance reads the wrapper key or the discriminator again).
                    val errorExpr: Expr[Any] =
                      if (!delegateToIntermediates) unknownExpr
                      else
                        Expr.quote {
                          val intermediate =
                            CirceDerivationUtils.intermediateFor(
                              Expr.splice(typeNameExpr),
                              Expr.splice(intermediatesExpr)
                            )
                          if (intermediate ne null)
                            CirceDerivationUtils.runDecoder(
                              intermediate,
                              Expr.splice(dctx.cursor),
                              Expr.splice(dctx.failFast)
                            )
                          else Expr.splice(unknownExpr)
                        }

                    MIO.pure(childDispatchers.toList.foldRight(errorExpr) { case (dispatcher, elseExpr) =>
                      dispatcher(typeNameExpr, innerCursorExpr, elseExpr)
                    })
                  }
                  .map(_.build[Any])
              }

          val stringDispatchMIO: MIO[Option[Expr[((String, HCursor)) => Any]]] =
            if (allCaseObjects || !allLeavesSingletons) MIO.pure(None)
            else
              NonEmptyList.fromList(leafEntries) match {
                case Some(leaves) =>
                  buildDispatch(leaves, entriesAreCaseObjects = true, delegateToIntermediates = false).map(Some(_))
                case None => MIO.pure(None)
              }

          for {
            dispatchFn <- buildDispatch(children, allCaseObjects, withIntermediates)
            stringDispatchOpt <- stringDispatchMIO
          } yield {
            val stringDispatchFn = stringDispatchOpt.getOrElse(dispatchFn)
            Expr.quote {
              val config = Expr.splice(dctx.config)
              val cursor = Expr.splice(dctx.cursor)
              val failFast = Expr.splice(dctx.failFast)
              if (config.enumAsStrings && Expr.splice(Expr(!allLeavesSingletons)))
                // enumAsStrings is only known at runtime here (statically it is a compile-time error)
                CirceDerivationUtils.failResult(
                  failFast,
                  CirceDerivationUtils.enumAsStringsNotSupported(Expr.splice(sumNameExpr), cursor)
                )
              else if (config.enumAsStrings) {
                // String enum decode path, like circe's ConfiguredEnumDecoder: read a string, dispatch on it
                cursor.as[String](io.circe.Decoder.decodeString) match {
                  case Right(typeName) => Expr.splice(stringDispatchFn)((typeName, cursor))
                  case Left(err)       => CirceDerivationUtils.failResult(failFast, err)
                }
              } else {
                val readResult: Either[DecodingFailure, (String, HCursor)] =
                  config.discriminator match {
                    case Some(field) =>
                      if (Expr.splice(Expr(nonObjectNames.nonEmpty)))
                        CirceDerivationUtils.decodeDiscriminatorOrWrapped(
                          cursor,
                          Expr.splice(sumNameExpr),
                          field,
                          Expr.splice(Expr(nonObjectNames)).iterator.map(config.transformConstructorNames).toSet,
                          Expr.splice(intermediatesExpr)
                        )
                      else CirceDerivationUtils.decodeDiscriminator(cursor, Expr.splice(sumNameExpr), field)
                    case None =>
                      CirceDerivationUtils.decodeWrapped(
                        cursor,
                        Expr.splice(sumNameExpr),
                        config.strictDecoding,
                        Expr.splice(Expr(directNames)).map(config.transformConstructorNames),
                        Expr.splice(Expr(knownNames)).map(config.transformConstructorNames),
                        Expr.splice(intermediatesExpr)
                      )
                  }
                readResult match {
                  case Right(r) => Expr.splice(dispatchFn)(r)
                  case Left(e)  => CirceDerivationUtils.failResult(failFast, e)
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
        childName: String,
        allCaseObjects: Boolean
    ): MIO[(Expr[String], Expr[HCursor], Expr[Any]) => Expr[Any]] = {
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
      implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
      val childShortName: String = Type[ChildType].shortName

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
                MIO.pure { (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) =>
                  Expr.quote {
                    if (
                      Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                        .splice(typeNameExpr)
                    ) {
                      // A singleton case is encoded as `{}` (wrapped) or just the discriminator, so like circe
                      // require an object - except for enumAsStrings, where the cursor focuses the name string (#235).
                      val config = Expr.splice(dctx.config)
                      val inner = Expr.splice(innerCursorExpr)
                      if (Expr.splice(Expr(allCaseObjects)) && config.enumAsStrings)
                        CirceDerivationUtils.okResult(
                          Expr.splice(dctx.failFast),
                          Expr.splice(singleton).asInstanceOf[A]
                        )
                      else if (!inner.value.isObject)
                        CirceDerivationUtils.notAnObject(inner, Expr.splice(dctx.failFast))
                      else if (config.strictDecoding)
                        // Like circe's derived decoder of a case object (a product without fields): only the
                        // discriminator is a valid key.
                        CirceDerivationUtils.fromEitherResult(
                          Expr.splice(dctx.failFast),
                          CirceDerivationUtils
                            .checkStrictDecoding(inner, Expr.splice(Expr(childShortName)), config.discriminator.toList)
                            .map(_ => Expr.splice(singleton).asInstanceOf[A])
                        )
                      else
                        CirceDerivationUtils.okResult(
                          Expr.splice(dctx.failFast),
                          Expr.splice(singleton).asInstanceOf[A]
                        )
                    } else
                      Expr.splice(elseExpr)
                  }
                }
            case None =>
              // No singleton - derive via full rules chain (this sets up a helper in cache)
              deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.cursor))
                .flatMap { _ =>
                  dctx.getHelper[ChildType].map[Option[(Expr[String], Expr[HCursor], Expr[Any]) => Expr[Any]]] {
                    case Some(helper) =>
                      Some { (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) =>
                        val helperCallExpr = helper(innerCursorExpr, dctx.config, dctx.failFast)
                        Expr.quote {
                          if (
                            Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == Expr
                              .splice(typeNameExpr)
                          )
                            Expr.splice(
                              helperCallExpr
                            ) // already returns Any (Either or ValidatedNel based on failFast)
                          else
                            Expr.splice(elseExpr)
                        }
                      }

                    case None =>
                      // No helper - the child was handled by a rule that does not cache one (e.g. a value-type child
                      // of a universal trait: `case class IntId(v: Int) extends AnyVal with Id`). Build a Decoder below.
                      None
                  }
                }
                .flatMap {
                  case Some(dispatch) => MIO.pure(dispatch)
                  case None           =>
                    deriveFieldDecoder[ChildType].map {
                      decoderExpr =>
                        (typeNameExpr: Expr[String], innerCursorExpr: Expr[HCursor], elseExpr: Expr[Any]) =>
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
                }
          }
      }
    }
  }
}
