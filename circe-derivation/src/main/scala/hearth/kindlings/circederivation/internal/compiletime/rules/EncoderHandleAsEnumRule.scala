package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=Non local returns")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Json]] = {
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum

      // Memoized: does a (child) type have a user-provided Encoder? Such children keep their instance (circe parity).
      val userEncoderCache = scala.collection.mutable.Map.empty[String, Boolean]
      val hasUserEncoder: ?? => Boolean = { child =>
        import child.Underlying as C
        userEncoderCache.getOrElseUpdate(
          Type[C].plainPrint,
          Types.Encoder[C].summonExprIgnoring(EncoderUseImplicitWhenAvailableRule.ignoredImplicits*).toEither.isRight
        )
      }

      // Nested sealed hierarchies are flattened like upstream circe: leaves are written under their own names.
      val entries: List[(String, ??)] = flattenedChildren[A](enumm)(hasUserEncoder)

      def isSingletonEntry(child: ??): Boolean = SingletonValue.unapply(child.Underlying).isDefined

      // Check at compile time if all (flattened) children are singletons (case objects with no fields)
      val allCaseObjects = isEnumerationOrJavaEnum || entries.forall { case (_, child) => isSingletonEntry(child) }
      // Like circe's ConfiguredEnumCodec (`summonSingletonCase` recurses into nested sums and ignores their instances),
      // enumAsStrings looks at the leaves: a hierarchy of case objects whose intermediates have user-provided instances
      // is still an enum, encoded as the leaf names.
      val leafEntries: List[(String, ??)] = if (allCaseObjects) entries else flattenedChildren[A](enumm)(_ => false)
      val allLeavesSingletons = allCaseObjects || leafEntries.forall { case (_, child) => isSingletonEntry(child) }

      if (!allLeavesSingletons && evaluatedConfigFlags.exists(_.enumAsStrings)) {
        // Like circe's ConfiguredEnumCodec, which only derives for sums of singletons.
        val err = EncoderDerivationError.EnumAsStringsWithNonSingletonCases(
          Type[A].prettyPrint,
          leafEntries.collect { case (name, child) if !isSingletonEntry(child) => name }
        )
        return Log.error(err.message) >> MIO.fail(err)
      }

      def noChildren: MIO[Expr[Json]] = {
        val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
        Log.error(err.message) >> MIO.fail(err)
      }

      val mainMIO: MIO[Expr[Json]] =
        encodeCasesOf[A, A](enumm, ectx.value, entries, allCaseObjects, isEnumerationOrJavaEnum, hasUserEncoder)
          .flatMap {
            case Some(result) => MIO.pure(result)
            case None         => noChildren
          }

      if (allCaseObjects || !allLeavesSingletons) mainMIO
      else {
        implicit val JsonT: Type[Json] = Types.Json
        // enumAsStrings: the leaves' names; otherwise the intermediates' user-provided instances are used.
        val stringsMIO: MIO[Expr[Json]] =
          encodeCasesOf[A, A](
            enumm,
            ectx.value,
            leafEntries,
            allCaseObjects = true,
            isEnumerationOrJavaEnum,
            _ => false
          )
            .flatMap {
              case Some(result) => MIO.pure(result)
              case None         => noChildren
            }
        for {
          main <- mainMIO
          strings <- stringsMIO
        } yield Expr.quote {
          if (Expr.splice(ectx.config).enumAsStrings) Expr.splice(strings) else Expr.splice(main)
        }
      }
    }

    /** Builds the `match` over `B`'s children. A child that is a nested derived sum gets its own nested `match` (so its
      * leaves are written with `A`'s naming/discriminator); other children are encoded as the entry they belong to.
      */
    private def encodeCasesOf[A: EncoderCtx, B: Type](
        enumB: Enum[B],
        value: Expr[B],
        entries: List[(String, ??)],
        allCaseObjects: Boolean,
        isEnumerationOrJavaEnum: Boolean,
        hasUserEncoder: ?? => Boolean
    ): MIO[Option[Expr[Json]]] = {
      implicit val JsonT: Type[Json] = Types.Json
      enumB
        .parMatchOn[MIO, Json](value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val nestedSum: Option[Enum[EnumCase]] =
              if (
                flattensNestedSums[A] && isNestedSum[EnumCase] && entryFor[EnumCase](entries)
                  .forall { case (_, entry) => !(entry.Underlying =:= Type[EnumCase]) }
              ) Enum.parse[EnumCase].toOption
              else None
            nestedSum match {
              case Some(nested) =>
                encodeCasesOf[A, EnumCase](
                  nested,
                  enumCaseValue,
                  entries,
                  allCaseObjects,
                  isEnumerationOrJavaEnum,
                  hasUserEncoder
                ).flatMap {
                  case Some(result) => MIO.pure(result)
                  case None         =>
                    val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[EnumCase].prettyPrint)
                    Log.error(err.message) >> MIO.fail(err)
                }
              case None =>
                val (caseName, entryType) =
                  entryFor[EnumCase](entries).getOrElse((Type[EnumCase].shortName, Type[EnumCase].as_??))
                import entryType.Underlying as Entry
                val userProvided = !isEnumerationOrJavaEnum && hasUserEncoder(entryType)
                // A user-provided instance of an intermediate sum: when it was derived by Kindlings for that sum
                // (`KindlingsSumEncoder`), its output is used as it is (circe's `ConfiguredEncoder.encodeSum`).
                val isKindlingsSumExpr: Expr[Boolean] =
                  if (userProvided && isNestedSum[Entry])
                    Types
                      .Encoder[Entry]
                      .summonExprIgnoring(EncoderUseImplicitWhenAvailableRule.ignoredImplicits*)
                      .toOption
                      .fold(Expr(false)) { instance =>
                        @scala.annotation.nowarn("msg=is never used")
                        implicit val EncoderEntry: Type[io.circe.Encoder[Entry]] = Types.Encoder[Entry]
                        Expr.quote(CirceDerivationUtils.isKindlingsSumEncoder(Expr.splice(instance)))
                      }
                  else Expr(false)
                val caseJsonMIO: MIO[Expr[Json]] =
                  if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(Json.obj()))
                  else if (Type[Entry] =:= Type[EnumCase])
                    deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
                  else
                    // Scala 2: a leaf under an intermediate sum with a user-provided Encoder
                    deriveEncoderRecursively[Entry](using ectx.nest(enumCaseValue.upcast[Entry]))
                caseJsonMIO.map { caseJson =>
                  if (allCaseObjects) {
                    Expr.quote {
                      val config = Expr.splice(ectx.config)
                      val name = config.transformConstructorNames(Expr.splice(Expr(caseName)))
                      if (config.enumAsStrings) {
                        CirceDerivationUtils.encodeEnumAsString(name)
                      } else {
                        val json = Expr.splice(caseJson)
                        if (Expr.splice(isKindlingsSumExpr)) json
                        else
                          config.discriminator match {
                            case Some(discriminatorField) =>
                              if (Expr.splice(Expr(userProvided)))
                                CirceDerivationUtils.addDiscriminatorIfAbsent(discriminatorField, name, json)
                              else CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                            case None =>
                              CirceDerivationUtils.wrapWithTypeName(name, json)
                          }
                      }
                    }
                  } else {
                    Expr.quote {
                      if (Expr.splice(ectx.config).enumAsStrings)
                        // only known at runtime here (statically it is a compile-time error)
                        throw new IllegalArgumentException(
                          CirceDerivationUtils.enumAsStringsNotSupportedMessage(Expr.splice(Expr(Type[A].shortName)))
                        )
                      val name = Expr.splice(ectx.config).transformConstructorNames(Expr.splice(Expr(caseName)))
                      val json = Expr.splice(caseJson)
                      if (Expr.splice(isKindlingsSumExpr)) json
                      else
                        Expr.splice(ectx.config).discriminator match {
                          case Some(discriminatorField) =>
                            if (Expr.splice(Expr(userProvided)))
                              CirceDerivationUtils.addDiscriminatorIfAbsent(discriminatorField, name, json)
                            else CirceDerivationUtils.addDiscriminator(discriminatorField, name, json)
                          case None =>
                            CirceDerivationUtils.wrapWithTypeName(name, json)
                        }
                    }
                  }
                }
            }
          }
        }
    }
  }
}
