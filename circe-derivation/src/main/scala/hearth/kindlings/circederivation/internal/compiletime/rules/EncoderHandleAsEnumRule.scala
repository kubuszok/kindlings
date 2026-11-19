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
        // With a config known at compile time only the applicable encoding is generated.
        ectx.evaluatedConfig.map(_.enumAsStrings) match {
          case Some(true)  => stringsMIO
          case Some(false) => mainMIO
          case None        =>
            for {
              main <- mainMIO
              strings <- stringsMIO
            } yield Expr.quote {
              if (Expr.splice(ectx.config).enumAsStrings) Expr.splice(strings) else Expr.splice(main)
            }
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
      // Subtype names transformed by a configuration only known at runtime: computed once per instance (with a
      // configuration known at compile time they are constants).
      val namesMIO: MIO[String => Expr[String]] = ectx.evaluatedConfig match {
        case Some(_) => MIO.pure((_: String) => Expr(""): Expr[String]) // not used
        case None    => ectx.transformedNames(entries.map(_._1), true)
      }
      namesMIO.flatMap { runtimeNameOf =>
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
                  // (the summoned instance is a lazy val of the derived instance, shared with its use for encoding)
                  val isKindlingsSumMIO: MIO[Expr[Boolean]] =
                    if (userProvided && isNestedSum[Entry])
                      Types
                        .Encoder[Entry]
                        .summonExprIgnoring(EncoderUseImplicitWhenAvailableRule.ignoredImplicits*)
                        .toOption
                        .fold(MIO.pure(Expr(false))) { instance =>
                          @scala.annotation.nowarn("msg=is never used")
                          implicit val EncoderEntry: Type[io.circe.Encoder[Entry]] = Types.Encoder[Entry]
                          ectx.cachedInstance[Entry](instance).map { cached =>
                            Expr.quote(CirceDerivationUtils.isKindlingsSumEncoder(Expr.splice(cached)))
                          }
                        }
                    else MIO.pure(Expr(false))
                  val caseJsonMIO: MIO[Expr[Json]] =
                    if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(Json.obj()))
                    else if (Type[Entry] =:= Type[EnumCase])
                      deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
                    else
                      // Scala 2: a leaf under an intermediate sum with a user-provided Encoder
                      deriveEncoderRecursively[Entry](using ectx.nest(enumCaseValue.upcast[Entry]))
                  caseJsonMIO
                    .flatMap(caseJson => isKindlingsSumMIO.map(isKindlingsSumExpr => (caseJson, isKindlingsSumExpr)))
                    .map { case (caseJson, isKindlingsSumExpr) =>
                      // With a config known at compile time the name is a constant and only the applicable encoding
                      // (string / discriminator / wrapper) is generated; otherwise the config decides at runtime.
                      @scala.annotation.nowarn("msg=is never used")
                      implicit val StringT: Type[String] = Types.String
                      val evaluated = ectx.evaluatedConfig
                      val nameExpr: Expr[String] = evaluated match {
                        case Some(c) => Expr(c.transformConstructorNames(caseName))
                        case None    => runtimeNameOf(caseName)
                      }
                      val kindlingsSumPossible = userProvided && isNestedSum[Entry]
                      def tagged(name: Expr[String], json: Expr[Json]): Expr[Json] = {
                        def withField(field: Expr[String]): Expr[Json] =
                          if (userProvided)
                            Expr.quote(
                              CirceDerivationUtils
                                .addDiscriminatorIfAbsent(Expr.splice(field), Expr.splice(name), Expr.splice(json))
                            )
                          else
                            Expr.quote(
                              CirceDerivationUtils
                                .addDiscriminator(Expr.splice(field), Expr.splice(name), Expr.splice(json))
                            )
                        val wrapped: Expr[Json] =
                          Expr.quote(CirceDerivationUtils.wrapWithTypeName(Expr.splice(name), Expr.splice(json)))
                        val byConfig: Expr[Json] = evaluated.map(_.discriminator) match {
                          case Some(Some(field)) => withField(Expr(field))
                          case Some(None)        => wrapped
                          case None              =>
                            Expr.quote {
                              Expr.splice(ectx.config).discriminator match {
                                case Some(discriminatorField) => Expr.splice(withField(Expr.quote(discriminatorField)))
                                case None                     => Expr.splice(wrapped)
                              }
                            }
                        }
                        if (!kindlingsSumPossible) byConfig
                        else
                          Expr.quote(if (Expr.splice(isKindlingsSumExpr)) Expr.splice(json) else Expr.splice(byConfig))
                      }
                      def encoded: Expr[Json] =
                        ValDefs.createVal[String](nameExpr, FreshName.FromPrefix("name")).use { name =>
                          ValDefs
                            .createVal[Json](caseJson, FreshName.FromPrefix("json"))
                            .use(json => tagged(name, json))
                        }
                      if (allCaseObjects) {
                        def asString: Expr[Json] =
                          Expr.quote(CirceDerivationUtils.encodeEnumAsString(Expr.splice(nameExpr)))
                        evaluated.map(_.enumAsStrings) match {
                          case Some(true)  => asString
                          case Some(false) => encoded
                          case None        =>
                            Expr.quote(
                              if (Expr.splice(ectx.config).enumAsStrings) Expr.splice(asString)
                              else Expr.splice(encoded)
                            )
                        }
                      } else
                        evaluated.map(_.enumAsStrings) match {
                          case Some(_) => encoded // (`true` is a compile-time error here)
                          case None    =>
                            Expr.quote {
                              if (Expr.splice(ectx.config).enumAsStrings)
                                // only known at runtime here (statically it is a compile-time error)
                                throw new IllegalArgumentException(
                                  CirceDerivationUtils.enumAsStringsNotSupportedMessage(
                                    Expr.splice(Expr(Type[A].shortName))
                                  )
                                )
                              Expr.splice(encoded)
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
