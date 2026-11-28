package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader, CoproductHint, ProductHint}
import hearth.kindlings.sconfigderivation.annotations.configKey
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.{ConfigObject, ConfigValue}
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait ReaderHandleAsEnumRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsEnumRule extends ReaderDerivationRule("handle as enum when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        parseEnum[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: ReaderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Either[ConfigDecodingError, A]]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val FieldHintT: Type[CoproductHint.Field[A]] = RTypes.fieldCoproductHintType[A]
      implicit val WrappedHintT: Type[CoproductHint.Wrapped[A]] = RTypes.wrappedCoproductHintType[A]
      implicit val FirstSuccessHintT: Type[CoproductHint.FirstSuccess[A]] = RTypes.firstSuccessCoproductHintType[A]

      // Per-type override for sealed-trait dispatch: each concrete variant is summoned separately, so users must
      // declare their hint with the concrete variant as the type.
      val maybeFieldHint: Option[Expr[CoproductHint.Field[A]]] =
        Expr.summonImplicit[CoproductHint.Field[A]].toOption
      val maybeWrappedHint: Option[Expr[CoproductHint.Wrapped[A]]] =
        Expr.summonImplicit[CoproductHint.Wrapped[A]].toOption
      val maybeFirstSuccessHint: Option[Expr[CoproductHint.FirstSuccess[A]]] =
        Expr.summonImplicit[CoproductHint.FirstSuccess[A]].toOption

      // How the subtypes' names are transformed: by a per-type hint, or by the global config (`None` when it is known
      // at compile time: the names are then constants).
      val runtimeTransform: Option[Expr[String => String]] = maybeFieldHint match {
        case Some(h) => Some(Expr.quote(Expr.splice(h).transformConstructorNames))
        case None    =>
          maybeWrappedHint match {
            case Some(h) => Some(Expr.quote(Expr.splice(h).transformConstructorNames))
            case None    =>
              if (rctx.evaluatedConfig.isDefined) None
              else Some(Expr.quote(Expr.splice(rctx.instanceConfig).transformConstructorNames))
          }
      }

      // Whether the subtype is named by a discriminator field or by a wrapper key, decided at compile time unless the
      // global config is only known at runtime.
      val discriminatorMode: SumMode = maybeFieldHint match {
        case Some(h) => SumMode.Discriminator(Expr.quote(Expr.splice(h).fieldName))
        case None    =>
          maybeWrappedHint match {
            case Some(_) => SumMode.Wrapped
            case None    =>
              rctx.evaluatedConfig match {
                case Some(c) => c.discriminator.fold[SumMode](SumMode.Wrapped)(f => SumMode.Discriminator(Expr(f)))
                case None    => SumMode.Runtime(Expr.quote(Expr.splice(rctx.config).discriminator))
              }
          }
      }

      // The discriminator field, when it is a compile-time constant (no per-type hint, evaluable global config).
      val staticDiscriminator: Option[String] =
        (maybeFirstSuccessHint, maybeFieldHint, maybeWrappedHint) match {
          case (None, None, None) => rctx.evaluatedConfig.flatMap(_.discriminator)
          case _                  => None
        }

      val childrenList = StableChildrenOrder.directChildren(ReaderHandleAsEnumRuleImpl.this)(enumm)
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          MIO.pure(Expr.quote {
            Left(
              SConfigDerivationUtils.failedToMatchSubtype(
                Expr.splice(Expr(Type[A].prettyPrint)),
                List.empty
              )
            ): Either[ConfigDecodingError, A]
          })

        case Some(children) =>
          val childNames = childrenList.map(_._1)
          // (names are not read with FirstSuccess)
          val namesMIO: MIO[String => Expr[String]] =
            if (maybeFirstSuccessHint.isDefined) MIO.pure((name: String) => Expr(name))
            else subtypeNames[A](childNames, runtimeTransform)
          namesMIO.flatMap { nameOf =>
            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving reader for enum case $childName: ${Type[ChildType].prettyPrint}") {
                  deriveChildReader[A, ChildType](childName, nameOf(childName), staticDiscriminator)
                }
              }
              .map { childReaders =>
                maybeFirstSuccessHint match {
                  case Some(_) => firstSuccess[A](childReaders.toList)
                  case None    => byName[A](childReaders.toList, allCaseObjects, discriminatorMode)
                }
              }
          }
      }
    }

    /** How a sum names its subtypes: in a discriminator field, by a wrapper key, or as the config says at runtime. */
    sealed private trait SumMode
    private object SumMode {
      final case class Discriminator(field: Expr[String]) extends SumMode
      case object Wrapped extends SumMode
      final case class Runtime(discriminator: Expr[Option[String]]) extends SumMode
    }

    /** The (transformed) names of the subtypes: constants when the transform is known at compile time, otherwise an
      * array computed once per instance (a `lazy val`), not on every read.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def subtypeNames[A: ReaderCtx](
        childNames: List[String],
        runtimeTransform: Option[Expr[String => String]]
    ): MIO[String => Expr[String]] = {
      implicit val StringT: Type[String] = RTypes.String
      implicit val ArrayStringT: Type[Array[String]] = RTypes.ArrayString
      implicit val IntT: Type[Int] = RTypes.Int
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      runtimeTransform match {
        case None =>
          val config = rctx.evaluatedConfig.get
          MIO.pure((name: String) => Expr(config.transformConstructorNames(name)))
        case Some(transform) =>
          val cacheKey = s"cached-subtype-names:${Type[A].plainPrint}"
          val names: Expr[Array[String]] = Expr.quote {
            val transformName = Expr.splice(transform)
            Expr.splice(Expr(childNames)).map(transformName).toArray
          }
          for {
            state <- rctx.cache.get
            builder = ValDefBuilder.ofLazy[Array[String]](s"subtypeNames_${Type[A].shortName}")
            _ <-
              if (builder.isBuilt(state, cacheKey)) MIO.pure(())
              else rctx.cache.buildCachedWith(cacheKey, builder)(_ => names)
            cached <- rctx.cache.get0Ary[Array[String]](cacheKey)
          } yield (name: String) => Expr.quote(Expr.splice(cached.get)(Expr.splice(Expr(childNames.indexOf(name)))))
      }
    }

    /** How to read one subtype: `read` reads it from the subtype's value, `readFromObject` too, but (like upstream's
      * derived reader of a case object) requires that value to be an object.
      */
    final private case class ChildReader[A](
        childName: String,
        transformedName: Expr[String],
        read: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]],
        readFromObject: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]],
        // With a discriminator field the subtype is read from the object without the discriminator key (as upstream
        // PureConfig), and `withoutKey` copies the object. `false` only when reading from the object with the key is
        // provably the same: a case object, or a case class derived here that statically allows unknown keys and has
        // no field keyed as the discriminator.
        needsDiscriminatorRemoved: Boolean = true
    )

    /** Subtypes selected by name (as upstream PureConfig):
      *   - with a discriminator field (`FieldCoproductHint`): the value must be an object, its discriminator names the
      *     subtype, which is read from the object without the discriminator key; an unknown name fails with
      *     `UnexpectedValueForFieldCoproductHint` at the discriminator;
      *   - without one, a family of case objects is read from strings (`deriveEnumerationReader`);
      *   - without one, other families are read from single-key objects `{ subtype-name { ... } }`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def byName[A: ReaderCtx](
        childReaders: List[ChildReader[A]],
        allCaseObjects: Boolean,
        mode: SumMode
    ): Expr[Either[ConfigDecodingError, A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ConfigObjectT: Type[ConfigObject] = RTypes.ConfigObject
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val OptionStringT: Type[Option[String]] = RTypes.OptionString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]

      val knownNames: List[String] = childReaders.map(_.childName)

      // `if (typeName == "child-name") <read child> else ... else <unknown>`
      def dispatch(
          typeNameExpr: Expr[String],
          // the inner value, given whether the subtype needs the discriminator removed (a Boolean rather than the
          // ChildReader: lambdas passed into splices must not mention macro-internal types on Scala 2)
          innerValueFor: Boolean => Expr[ConfigValue],
          unknownExpr: Expr[Either[ConfigDecodingError, A]]
      ): Expr[Either[ConfigDecodingError, A]] =
        childReaders.foldRight(unknownExpr) { (child, elseExpr) =>
          val innerValue = innerValueFor(child.needsDiscriminatorRemoved)
          Expr.quote {
            if (Expr.splice(child.transformedName) == Expr.splice(typeNameExpr))
              Expr.splice(child.read(innerValue))
            else Expr.splice(elseExpr)
          }
        }

      val typeNameStr: String = Type[A].shortName

      // (no function value per read: the results are matched, not `flatMap`ped)
      val withoutDiscriminator: Expr[Either[ConfigDecodingError, A]] =
        if (allCaseObjects)
          Expr.quote {
            val v = Expr.splice(rctx.value)
            SConfigDerivationUtils.asString(v) match {
              case Right(str) =>
                Expr.splice(
                  dispatch(
                    Expr.quote(str),
                    _ => Expr.quote(v),
                    Expr.quote {
                      Left(
                        SConfigDerivationUtils.invalidEnumValue(str, Expr.splice(Expr(typeNameStr)))
                      ): Either[ConfigDecodingError, A]
                    }
                  )
                )
              case Left(e) => Left(e): Either[ConfigDecodingError, A]
            }
          }
        else
          Expr.quote {
            SConfigDerivationUtils.asObject(Expr.splice(rctx.value)) match {
              case Right(obj) =>
                SConfigDerivationUtils.readWrapped(obj) match {
                  case Right(wrapped) =>
                    val typeName = wrapped._1
                    val inner = wrapped._2
                    val result: Either[ConfigDecodingError, A] = Expr.splice(
                      dispatch(
                        Expr.quote(typeName),
                        _ => Expr.quote(inner),
                        Expr.quote {
                          Left(
                            SConfigDerivationUtils.failedToMatchSubtype(typeName, Expr.splice(Expr(knownNames)))
                          ): Either[ConfigDecodingError, A]
                        }
                      )
                    )
                    // the subtype's failures are reported under its key
                    if (result.isLeft)
                      Left(result.asInstanceOf[Left[ConfigDecodingError, A]].value.withParentPath(typeName))
                    else result
                  case Left(e) => Left(e): Either[ConfigDecodingError, A]
                }
              case Left(e) => Left(e): Either[ConfigDecodingError, A]
            }
          }

      def withDiscriminator(fieldExpr: Expr[String]): Expr[Either[ConfigDecodingError, A]] =
        Expr.quote {
          val field = Expr.splice(fieldExpr)
          SConfigDerivationUtils.asObject(Expr.splice(rctx.value)) match {
            case Right(obj) =>
              SConfigDerivationUtils.readDiscriminatorField(obj, field) match {
                case Right(typeName) =>
                  Expr.splice(
                    dispatch(
                      Expr.quote(typeName),
                      // `withoutKey` copies the object, so it is done only for subtypes whose reading can depend on it
                      (needsRemoval: Boolean) =>
                        if (needsRemoval) Expr.quote(obj.withoutKey(field): ConfigValue)
                        else Expr.quote(obj: ConfigValue),
                      Expr.quote {
                        Left(SConfigDerivationUtils.unexpectedDiscriminatorValue(obj, field)): Either[
                          ConfigDecodingError,
                          A
                        ]
                      }
                    )
                  )
                case Left(e) => Left(e): Either[ConfigDecodingError, A]
              }
            case Left(e) => Left(e): Either[ConfigDecodingError, A]
          }
        }

      mode match {
        case SumMode.Discriminator(field)        => withDiscriminator(field)
        case SumMode.Wrapped                     => withoutDiscriminator
        case SumMode.Runtime(discriminatorField) =>
          Expr.quote {
            Expr.splice(discriminatorField) match {
              case Some(runtimeField) => Expr.splice(withDiscriminator(Expr.quote(runtimeField)))
              case None               => Expr.splice(withoutDiscriminator)
            }
          }
      }
    }

    /** Upstream `FirstSuccessCoproductHint`: every subtype is attempted on the same value, in the alphabetical order of
      * the subtypes' names (as upstream's derivation passes them to the hint), and the first one read successfully
      * wins. When none can be read, the failure is `NoValidCoproductOptionFound` with every attempt's failure.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def firstSuccess[A: ReaderCtx](
        childReaders: List[ChildReader[A]]
    ): Expr[Either[ConfigDecodingError, A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val ListStringT: Type[List[String]] = RTypes.ListString
      implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val EitherAnyT: Type[Either[ConfigDecodingError, Any]] = RTypes.EitherErrorAny
      implicit val ListEitherT: Type[List[Either[ConfigDecodingError, Any]]] = RTypes.ListEitherErrorAny

      val ordered = childReaders.sortBy(_.childName)
      val optionNames: List[String] = ordered.map(_.childName)

      // Each attempt is bound to a `val`; the next subtype is attempted only when it failed, and when every attempt
      // failed, their failures are reported together.
      def attempt(
          value: Expr[ConfigValue],
          remaining: List[ChildReader[A]],
          failedReversed: List[Expr[Either[ConfigDecodingError, A]]]
      ): Expr[Either[ConfigDecodingError, A]] = remaining match {
        case child :: rest =>
          ValDefs.createVal[Either[ConfigDecodingError, A]](child.readFromObject(value)).use { result =>
            Expr.quote {
              if (Expr.splice(result).isRight) Expr.splice(result)
              else Expr.splice(attempt(value, rest, result :: failedReversed))
            }
          }
        case Nil =>
          val results = failedReversed.foldLeft(Expr.quote(List.empty[Either[ConfigDecodingError, Any]])) { (acc, r) =>
            Expr.quote((Expr.splice(r): Either[ConfigDecodingError, Any]) :: Expr.splice(acc))
          }
          Expr.quote {
            Left(
              SConfigDerivationUtils.noValidCoproductOptionFromResults(
                Expr.splice(value),
                Expr.splice(Expr(optionNames)),
                Expr.splice(results)
              )
            )
          }
      }

      ValDefs.createVal[ConfigValue](rctx.value).use(value => attempt(value, ordered, Nil))
    }

    /** Derive how to read a single sealed-trait subtype (see [[ChildReader]]): a user-provided implicit
      * `ConfigReader[ChildType]`, a singleton case object, or a recursive derivation via the cached helper.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def deriveChildReader[A: ReaderCtx, ChildType: Type](
        childName: String,
        transformedNameExpr: Expr[String],
        staticDiscriminator: Option[String]
    ): MIO[ChildReader[A]] = {
      implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
      implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
      implicit val StringT: Type[String] = RTypes.String
      implicit val FuncT: Type[String => String] = RTypes.StringToString
      implicit val ReaderResultA: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
      implicit val ReaderResultChild: Type[Either[ConfigDecodingError, ChildType]] = RTypes.ReaderResult[ChildType]
      implicit val ReaderChild: Type[ConfigReader[ChildType]] = RTypes.ConfigReader[ChildType]

      def same(read: Expr[ConfigValue] => Expr[Either[ConfigDecodingError, A]]): ChildReader[A] =
        ChildReader[A](childName, transformedNameExpr, read, read)

      RTypes
        .ConfigReader[ChildType]
        .summonExprIgnoring(ReaderUseImplicitWhenAvailableRule.ignoredImplicits*)
        .toEither match {
        case Right(readerExpr) =>
          Log.info(s"Found implicit ConfigReader[$childName], using it") >>
            rctx.cachedInstance[ChildType](readerExpr).map { cached =>
              same { (innerValueExpr: Expr[ConfigValue]) =>
                Expr.quote {
                  Expr
                    .splice(cached)
                    .from(Expr.splice(innerValueExpr))
                    .asInstanceOf[Either[ConfigDecodingError, A]]
                }
              }
            }

        case Left(_) =>
          Expr.singletonOf[ChildType] match {
            case Some(singleton) =>
              implicit val BooleanT: Type[Boolean] = RTypes.Boolean
              val (staticAllowUnknownKeys, allowUnknownKeysExpr) =
                ReaderHandleAsCaseClassRule.allowUnknownKeysOf[ChildType]
              Log.info(s"Using singleton for $childName") >>
                MIO.pure(
                  // A case object that does not allow unknown keys (upstream's `ProductHint[Obj.type](allowUnknownKeys =
                  // false)`) reports every key of the object it is read from (without the discriminator).
                  if (!staticAllowUnknownKeys.contains(true))
                    ChildReader[A](
                      childName,
                      transformedNameExpr,
                      (innerValueExpr: Expr[ConfigValue]) =>
                        Expr.quote {
                          SConfigDerivationUtils.checkNoKeys[A](
                            Expr.splice(innerValueExpr),
                            Expr.splice(singleton).asInstanceOf[A],
                            Expr.splice(allowUnknownKeysExpr)
                          )
                        },
                      (innerValueExpr: Expr[ConfigValue]) =>
                        Expr.quote {
                          SConfigDerivationUtils.readEmptyProduct[A](
                            Expr.splice(innerValueExpr),
                            Expr.splice(singleton).asInstanceOf[A],
                            Expr.splice(allowUnknownKeysExpr)
                          )
                        }
                    )
                  else
                    ChildReader[A](
                      childName,
                      transformedNameExpr,
                      (_: Expr[ConfigValue]) =>
                        Expr.quote(Right(Expr.splice(singleton).asInstanceOf[A]): Either[ConfigDecodingError, A]),
                      (innerValueExpr: Expr[ConfigValue]) =>
                        Expr.quote {
                          SConfigDerivationUtils.readObjectAs[A](
                            Expr.splice(innerValueExpr),
                            Expr.splice(singleton).asInstanceOf[A]
                          )
                        },
                      needsDiscriminatorRemoved = false // `read` ignores the value
                    )
                )
            case None =>
              deriveReaderRecursively[ChildType](using rctx.nest[ChildType](rctx.value)).flatMap { _ =>
                rctx.getHelper[ChildType].flatMap {
                  case Some(helper) =>
                    MIO.pure(
                      same { (innerValueExpr: Expr[ConfigValue]) =>
                        val helperCallExpr = helper(innerValueExpr, rctx.config)
                        Expr.quote(Expr.splice(helperCallExpr).asInstanceOf[Either[ConfigDecodingError, A]])
                      }.copy(needsDiscriminatorRemoved = !discriminatorKeyIsIgnored[ChildType](staticDiscriminator))
                    )
                  case None =>
                    val err = ReaderDerivationError.CannotConstructType(
                      Type[ChildType].prettyPrint,
                      isSingleton = false,
                      Some("no cached reader for the subtype")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
              }
          }
      }
    }

    /** Whether the reader derived for the case class `ChildType` reads the same from an object with or without the
      * discriminator key: it statically allows unknown keys (global config evaluable at compile time, no per-type
      * `ProductHint`) and no field's key is the (statically known) discriminator. Anything else - a non-case class, a
      * hint, a runtime config or discriminator - conservatively answers `false`.
      */
    private def discriminatorKeyIsIgnored[ChildType: Type](staticDiscriminator: Option[String])(implicit
        ctx: ReaderCtx[?]
    ): Boolean = {
      implicit val configKeyT: Type[configKey] = RTypes.ConfigKey
      implicit val ProductHintT: Type[ProductHint[ChildType]] = RTypes.productHintType[ChildType]
      (for {
        discriminator <- staticDiscriminator
        config <- ctx.evaluatedConfig
        if config.allowUnknownKeys
        if Expr.summonImplicit[ProductHint[ChildType]].toOption.isEmpty
        caseClass <- CaseClass.parse[ChildType].toEither.toOption
      } yield caseClass.primaryConstructor.totalParameters.flatten.forall { case (name, param) =>
        getAnnotationStringArg[configKey](param).getOrElse(config.transformMemberNames(name)) != discriminator
      }).getOrElse(false)
    }
  }
}
