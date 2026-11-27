package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsEnumRule extends DecoderDerivationRule("handle as sealed trait/enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a sealed trait/enum") >> {
        Enum.parse[A].toEither match {
          case Right(parsedEnum) =>
            decodeEnumCases[A](parsedEnum).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    private def hasUserDecoder[A: DecoderCtx]: ?? => Boolean =
      DecoderUseImplicitWhenAvailableRule.hasUserDecoder(dctx.derivedType)

    /** The name of a subtype: a constant with `@xmlName` or a configuration known at compile time. */
    @scala.annotation.nowarn("msg=is never used")
    private def nameExprOf[A: DecoderCtx](subtype: XmlSubtype, config: Expr[XmlConfig]): Expr[String] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
      (subtype.customName, dctx.evaluatedConfig) match {
        case (Some(custom), _) => Expr(custom)
        case (None, Some(cfg)) => Expr(cfg.constructorNameMapper(subtype.rawName))
        case (None, None)      =>
          Expr.quote(Expr.splice(config).constructorNameMapper(Expr.splice(Expr(subtype.rawName))))
      }
    }

    /** The names of the subtypes, as a list (a constant with a configuration known at compile time). */
    @scala.annotation.nowarn("msg=is never used")
    private def namesListOf[A: DecoderCtx](subtypes: List[XmlSubtype], config: Expr[XmlConfig]): Expr[List[String]] = {
      implicit val StringT: Type[String] = DTypes.String
      val custom = subtypes.forall(_.customName.isDefined)
      dctx.evaluatedConfig match {
        case Some(cfg)      => Expr(subtypes.map(_.nameWith(cfg)))
        case None if custom => Expr(subtypes.flatMap(_.customName))
        case None           =>
          subtypes.foldRight(Expr.quote(List.empty[String])) { (subtype, acc) =>
            Expr.quote(Expr.splice(nameExprOf[A](subtype, config)) :: Expr.splice(acc))
          }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeEnumCases[A: DecoderCtx](
        enumType: Enum[A]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
      // the leaves of the hierarchy (nested sealed subtypes are flattened), as the encoder writes them
      val subtypes = xmlSubtypesOf[A](enumType)(hasUserDecoder[A])
      val isValueEnum = Type[A].isJavaEnum || Type[A].isEnumeration
      // an enum whose every case is a singleton can be read from the case name (`enumAsStrings`)
      val isStringEnum = subtypes.nonEmpty && (isValueEnum || subtypes.forall { subtype =>
        SingletonValue.unapply(subtype.tpe.Underlying).isDefined
      })

      // Pre-compute discriminator attribute name from config
      // (`@xmlDiscriminator` on the type wins over the configuration)
      val staticDiscriminator: Option[Option[String]] = xmlStaticDiscriminatorOf[A](dctx.evaluatedConfig)
      val discriminatorAttr: Option[String] = staticDiscriminator.flatten
      val discriminatorAttrName: String = discriminatorAttr.getOrElse("type")
      val hasDiscriminator: Option[Boolean] = staticDiscriminator.map(_.isDefined)

      // an enum of singletons: the name read from the discriminator (or from the element inside) is the value
      def taggedSingletons: MIO[Expr[Either[XmlDecodingError, A]]] = enumFromName[A](subtypes).map { fromName =>
        // (expressions are created inside the quotes using them: Scala 3 ties them to the enclosing splice)
        def withDiscriminator(attribute: => Expr[String]): Expr[Either[XmlDecodingError, A]] = Expr.quote {
          XmlDerivationUtils.decodeDiscriminator(Expr.splice(dctx.elem), Expr.splice(attribute)) match {
            case Right(typeName) => Expr.splice(fromName(Expr.quote(typeName), dctx.config))
            case Left(error)     => Left(error): Either[XmlDecodingError, A]
          }
        }
        def wrapped: Expr[Either[XmlDecodingError, A]] = Expr.quote {
          XmlDerivationUtils.decodeWrapped(Expr.splice(dctx.elem)) match {
            case Right(nameAndElem) => Expr.splice(fromName(Expr.quote(nameAndElem._1), dctx.config))
            case Left(error)        => Left(error): Either[XmlDecodingError, A]
          }
        }
        hasDiscriminator match {
          case Some(true)  => withDiscriminator(Expr(discriminatorAttrName))
          case Some(false) => wrapped
          case None        =>
            Expr.quote {
              if (Expr.splice(dctx.config).discriminatorAttribute.isDefined)
                Expr.splice(withDiscriminator(Expr.quote(Expr.splice(dctx.config).discriminatorAttribute.get)))
              else Expr.splice(wrapped)
            }
        }
      }

      def tagged: MIO[Expr[Either[XmlDecodingError, A]]] = NonEmptyList.fromList(subtypes) match {
        case None =>
          val knownNames: List[String] = Nil
          MIO.pure(Expr.quote {
            Left(
              XmlDerivationUtils.failedToMatchSubtype("", Expr.splice(Expr(knownNames)))
            ): Either[XmlDecodingError, A]
          })

        case Some(children) =>
          // Derive all child decoders, collecting their helpers as lambdas
          children
            .traverse { subtype =>
              import subtype.tpe.Underlying as ChildType
              deriveChildDecoderLambda[A, ChildType]
            }
            .map { childLambdas =>
              val childDecoderExprs: List[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]] = childLambdas.toList

              // With a configuration only known at runtime the names are transformed by its `constructorNameMapper`
              val namesListExpr: Expr[List[String]] = namesListOf[A](subtypes, dctx.config)
              val decodersListExpr: Expr[List[scala.xml.Elem => Either[XmlDecodingError, Any]]] =
                childDecoderExprs.foldRight(
                  Expr.quote(List.empty[scala.xml.Elem => Either[XmlDecodingError, Any]])
                ) { (decoder, acc) =>
                  Expr.quote(Expr.splice(decoder) :: Expr.splice(acc))
                }

              hasDiscriminator match {
                case Some(true) =>
                  // Discriminator is statically known to be present
                  Expr.quote {
                    XmlDerivationUtils.getAttribute(
                      Expr.splice(dctx.elem),
                      Expr.splice(Expr(discriminatorAttrName))
                    ) match {
                      case Right(typeName) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          Expr.splice(dctx.elem),
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(_) =>
                        Left(
                          XmlDecodingError
                            .MissingDiscriminator(
                              Expr.splice(Expr(discriminatorAttrName)),
                              Expr.splice(dctx.elem).label
                            )
                        )
                    }
                  }
                case Some(false) =>
                  // No discriminator — try matching by wrapper element name
                  Expr.quote {
                    XmlDerivationUtils.decodeWrapped(Expr.splice(dctx.elem)) match {
                      case Right((typeName, innerElem)) =>
                        XmlDerivationUtils.dispatchByName[A](
                          typeName,
                          innerElem,
                          Expr.splice(namesListExpr),
                          Expr.splice(decodersListExpr)
                        )
                      case Left(err) => Left(err)
                    }
                  }
                case None =>
                  // evaluatedConfig not available — use runtime check
                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    config.discriminatorAttribute match {
                      case scala.Some(attr) =>
                        XmlDerivationUtils.getAttribute(Expr.splice(dctx.elem), attr) match {
                          case Right(typeName) =>
                            XmlDerivationUtils.dispatchByName[A](
                              typeName,
                              Expr.splice(dctx.elem),
                              Expr.splice(namesListExpr),
                              Expr.splice(decodersListExpr)
                            )
                          case Left(_) =>
                            Left(XmlDecodingError.MissingDiscriminator(attr, Expr.splice(dctx.elem).label))
                        }
                      case scala.None =>
                        XmlDerivationUtils.decodeWrapped(Expr.splice(dctx.elem)) match {
                          case Right((typeName, innerElem)) =>
                            XmlDerivationUtils.dispatchByName[A](
                              typeName,
                              innerElem,
                              Expr.splice(namesListExpr),
                              Expr.splice(decodersListExpr)
                            )
                          case Left(err) => Left(err)
                        }
                    }
                  }
              }
            }
      }

      def fromString: MIO[Expr[Either[XmlDecodingError, A]]] = enumFromName[A](subtypes).map { fromName =>
        Expr.quote {
          val text = Expr.splice(dctx.elem).text.trim
          Expr.splice(fromName(Expr.quote(text), dctx.config))
        }
      }

      // `enumAsStrings` is only for enums of singletons: with a case that is not one it is a compile-time error (when
      // the config is only known at runtime: a decoding error when it turns out to be enabled)
      def nonSingletons: List[String] = subtypes.collect {
        case subtype if SingletonValue.unapply(subtype.tpe.Underlying).isEmpty => subtype.rawName
      }

      // With a config known at compile time only the applicable decoding is generated.
      if (!isStringEnum && subtypes.nonEmpty)
        dctx.evaluatedConfig.map(_.enumAsStrings) match {
          case Some(true) =>
            val err = DecoderDerivationError.EnumAsStringsWithNonSingletonCases(Type[A].prettyPrint, nonSingletons)
            Log.error(err.message) >> MIO.fail(err)
          case Some(false) => tagged
          case None        =>
            tagged.map { element =>
              Expr.quote {
                if (Expr.splice(dctx.config).enumAsStrings)
                  XmlDerivationUtils.enumAsStringsNotSupportedResult[A](Expr.splice(Expr(Type[A].plainPrint)))
                else Expr.splice(element)
              }
            }
        }
      else if (!isStringEnum) tagged
      else
        dctx.evaluatedConfig.map(_.enumAsStrings) match {
          case Some(true)  => fromString
          case Some(false) => taggedSingletons
          case None        =>
            for {
              string <- fromString
              element <- taggedSingletons
            } yield Expr.quote {
              if (Expr.splice(dctx.config).enumAsStrings) Expr.splice(string) else Expr.splice(element)
            }
        }
    }

    /** The cached `(case name, config) => value` def of an enum whose cases are all singletons: how it is read from the
      * text of its element with `enumAsStrings`, and from an attribute value.
      */
    @scala.annotation.nowarn("msg=is never used")
    def enumFromName[A: DecoderCtx](
        subtypes: List[XmlSubtype]
    ): MIO[(Expr[String], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val StringT: Type[String] = DTypes.String
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
      dctx.getEnumFromNameHelper[A].flatMap {
        case Some(helper) => MIO.pure(helper)
        case None         =>
          dctx.setEnumFromNameHelper[A] { (name, config) =>
            val unknown: Expr[Either[XmlDecodingError, A]] = Expr.quote {
              Left(
                XmlDerivationUtils
                  .failedToMatchSubtype(Expr.splice(name), Expr.splice(namesListOf[A](subtypes, config)))
              ): Either[XmlDecodingError, A]
            }
            // (the singletons are created here, in the scope of the cached def)
            subtypes.foldRight(MIO.pure(unknown)) { (subtype, otherwiseMIO) =>
              import subtype.tpe.Underlying as Leaf
              otherwiseMIO.flatMap { otherwise =>
                Expr.singletonOf[Leaf] match {
                  case Some(singleton) =>
                    // (not typed as the leaf: an `Enumeration` value has the type of all its values)
                    val value: Expr[A] = singleton.asInstanceOf[Expr[A]]
                    MIO.pure(Expr.quote {
                      if (Expr.splice(name) == Expr.splice(nameExprOf[A](subtype, config)))
                        Right(Expr.splice(value)): Either[XmlDecodingError, A]
                      else Expr.splice(otherwise)
                    })
                  case None =>
                    val err = DecoderDerivationError.SingletonDisappeared(Type[Leaf].prettyPrint)
                    Log.error(err.message) >> MIO.fail(err)
                }
              }
            }
          } >> dctx.getEnumFromNameHelper[A].flatMap {
            case Some(helper) => MIO.pure(helper)
            case None         => MIO.fail(new RuntimeException(s"No enum name helper for ${Type[A].prettyPrint}"))
          }
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveChildDecoderLambda[A: DecoderCtx, ChildType: Type]
        : MIO[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny

      // Derive via full rules chain (singletons use setHelper too), then use cached helper
      implicit val EitherChildT: Type[Either[XmlDecodingError, ChildType]] = DTypes.DecoderResult[ChildType]
      implicit val ConfigT: Type[XmlConfig] = DTypes.XmlConfig
      deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.elem)).flatMap { _ =>
        dctx.getHelper[ChildType].flatMap {
          case Some(helper) =>
            val configExpr = dctx.config
            val lambda: Expr[scala.xml.Elem => Either[XmlDecodingError, Any]] =
              Expr.quote { (childElem: scala.xml.Elem) =>
                Expr
                  .splice(helper(Expr.quote(childElem), configExpr))
                  .asInstanceOf[Either[XmlDecodingError, Any]]
              }
            MIO.pure(lambda)
          case None =>
            MIO.fail(
              new RuntimeException(s"No helper found for enum case ${Type[ChildType].prettyPrint}")
            )
        }
      }
    }
  }

}
