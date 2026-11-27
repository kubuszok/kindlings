package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.annotations.xmlName
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](
              caseClass,
              caseClass.primaryConstructor.totalParameters.flatten.toList
            )
              .map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A],
        fields: List[(String, Parameter)]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val xmlNameT: Type[xmlName] = DTypes.XmlNameAnnotation

      val constructor = caseClass.primaryConstructor

      // Where each field is read from (shared with the encoder), or what is wrong with the annotations
      val layoutsOrErrors = xmlFieldLayouts(
        Type[A].prettyPrint,
        fields,
        dctx.evaluatedConfig,
        DecoderUseImplicitWhenAvailableRule.hasUserDecoder(dctx.derivedType)
      )

      layoutsOrErrors match {
        case Left(errors) =>
          val err = DecoderDerivationError.InvalidFieldAnnotations(Type[A].prettyPrint, errors)
          Log.error(err.message) >> MIO.fail(err)
        case Right(layouts) if fields.nonEmpty =>
          val decodings: List[FieldDecoding] = fields.zip(layouts).map { case ((fName, param), layout) =>
            import param.tpe.Underlying as FieldType
            val customName = getAnnotationStringArg[xmlName](param)
            val isOptionField = Type[FieldType] match {
              case IsOption(_) => true
              case _           => false
            }
            // (`String` is an `Iterable[Char]` to `IsCollection`, but an absent `String` is missing, not empty)
            val isCollectionField = !isOptionField && !(Type[FieldType] =:= DTypes.String) && (Type[FieldType] match {
              case IsMap(_)        => true
              case IsCollection(_) => true
              case _               => false
            })

            // Apply fieldNameMapper: annotation overrides config mapper
            // When evaluatedConfig is available, pre-compute at compile time; otherwise use runtime call
            val xmlFieldNameExpr: Expr[String] = customName match {
              case Some(name) => Expr(name)
              case None       =>
                dctx.evaluatedConfig match {
                  case Some(cfg) => Expr(cfg.fieldNameMapper(fName))
                  case None      =>
                    Expr.quote(Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                }
            }

            val hasDefault = param.hasDefault
            val useDefaultsStaticallyTrue = dctx.evaluatedConfig.exists(_.useDefaults)
            val useDefaultsStaticallyFalse = dctx.evaluatedConfig.exists(!_.useDefaults)

            // An absent `Option` element/attribute is `None` (as written by `withTransientNone`, and as circe & co.
            // treat absent optional fields) - or the field's default when `useDefaults` is enabled.
            def optionWhenAbsent: Expr[Any] = {
              val noneExpr: Expr[Any] = Expr.quote(None: Any)
              if (hasDefault && useDefaultsStaticallyTrue) resolveDefaultValue(fName, param)
              else if (hasDefault && !useDefaultsStaticallyFalse) {
                val defaultExpr = resolveDefaultValue(fName, param)
                Expr.quote {
                  if (Expr.splice(dctx.config).useDefaults) Expr.splice(defaultExpr) else Expr.splice(noneExpr)
                }
              } else noneExpr
            }

            // Decode from an attribute; with `useDefaults` an absent attribute is the field's default
            def asAttribute(shape: XmlAttrShape): FieldDecoding =
              if (shape.optional) FieldDecoding.FromOptionalAttribute(xmlFieldNameExpr, shape.shape, optionWhenAbsent)
              else if (hasDefault && useDefaultsStaticallyTrue)
                FieldDecoding
                  .FromAttributeWithDefault(xmlFieldNameExpr, shape.shape, resolveDefaultValue(fName, param))
              else if (hasDefault && !useDefaultsStaticallyFalse)
                FieldDecoding
                  .FromAttributeWithRuntimeDefault(xmlFieldNameExpr, shape.shape, resolveDefaultValue(fName, param))
              else FieldDecoding.FromAttribute(xmlFieldNameExpr, shape.shape)

            // Decode from a child element
            def asElement: FieldDecoding =
              if (isOptionField) FieldDecoding.FromChildElementWithDefault(xmlFieldNameExpr, optionWhenAbsent)
              else if (hasDefault && useDefaultsStaticallyTrue) {
                // useDefaults is statically true — use default when missing
                FieldDecoding.FromChildElementWithDefault(xmlFieldNameExpr, resolveDefaultValue(fName, param))
              } else if (hasDefault && !useDefaultsStaticallyFalse) {
                // useDefaults is not statically known — emit runtime check
                FieldDecoding.FromChildElementWithRuntimeDefault(xmlFieldNameExpr, resolveDefaultValue(fName, param))
              } else if (isCollectionField) {
                // No default or useDefaults is statically false. An absent collection/map element is decoded as
                // an empty element, i.e. an empty collection (as written by `withTransientEmpty`).
                FieldDecoding.FromChildElementOrEmpty(xmlFieldNameExpr)
              } else FieldDecoding.FromChildElement(xmlFieldNameExpr)

            // The default of a collection without any item (`@xmlUnwrapped`) / without its `@xmlWrapper` element
            def collectionDefault: (Option[Expr[Any]], Boolean) =
              if (hasDefault && useDefaultsStaticallyTrue) (Some(resolveDefaultValue(fName, param)), false)
              else if (hasDefault && !useDefaultsStaticallyFalse) (Some(resolveDefaultValue(fName, param)), true)
              else (None, false)

            layout match {
              case XmlFieldLayout.Transient =>
                val defaultValue: Expr[Any] = param.defaultValue match {
                  case Some(method) =>
                    foldInstanceFree(method, "Default value")(
                      onTypes = _ => Map.empty,
                      onValues = _ => Map.empty
                    ) match {
                      case Right(expr) => expr.value.asInstanceOf[Expr[Any]]
                      case Left(_)     =>
                        Environment.reportErrorAndAbort(
                          s"Field '$fName' is annotated with @transientField but its default value could not be resolved"
                        )
                    }
                  case None =>
                    Environment.reportErrorAndAbort(
                      s"Field '$fName' is annotated with @transientField but has no default value"
                    )
                }
                FieldDecoding.Default(defaultValue)
              case XmlFieldLayout.Content(shape)                    => FieldDecoding.FromContent(shape)
              case XmlFieldLayout.Attribute(shape)                  => asAttribute(shape)
              case XmlFieldLayout.Element                           => asElement
              case XmlFieldLayout.ByMode(shape, needsEnumAsStrings) =>
                // `defaultFieldMode` decides, as in the encoder
                FieldDecoding.DynamicMode(asAttribute(shape), asElement, needsEnumAsStrings)
              case XmlFieldLayout.Repeated =>
                val (default, needsUseDefaults) = collectionDefault
                FieldDecoding.FromRepeatedChildren(xmlFieldNameExpr, default, needsUseDefaults)
              case XmlFieldLayout.Wrapped(wrapper, true) =>
                // a present wrapper holds the items (none: an empty collection); an absent one is the default
                val (default, needsUseDefaults) = collectionDefault
                FieldDecoding.FromWrapper(
                  wrapper,
                  FieldDecoding.FromRepeatedChildren(xmlFieldNameExpr, None, false),
                  default,
                  needsUseDefaults
                )
              case XmlFieldLayout.Wrapped(wrapper, false) =>
                // an absent wrapper is read as an empty one: the field's element is absent
                FieldDecoding.FromWrapper(wrapper, asElement, None, false)
            }
          }
          buildDecodeExpr[A](caseClass, constructor, fields, decodings)
        case Right(_) =>
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                MIO.fail(
                  new RuntimeException(
                    s"Unexpected parameter in zero-argument case class ${Type[A].prettyPrint}"
                  )
                )
            })
            .flatMap {
              case Some(constructExpr) =>
                MIO.pure(Expr.quote {
                  Right(Expr.splice(constructExpr))
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint)
                Log.error(err.message) >> MIO.fail(err)
            }
      }
    }

    /** How a single text value (an attribute value) of type `T` is decoded: with the decoder of `T` on an element
      * holding the text, or - for an enum of singletons - from the case name.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def textDecoder[A: DecoderCtx, T: Type](
        shape: XmlTextShape
    ): MIO[Expr[String] => Expr[Either[XmlDecodingError, T]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
      val configExpr = dctx.config
      shape match {
        case XmlTextShape.StringEnum(subtypes) =>
          implicit val EitherT: Type[Either[XmlDecodingError, T]] = DTypes.DecoderResult[T]
          DecoderHandleAsEnumRule.enumFromName[T](subtypes)(using dctx.nest[T](dctx.elem)).map {
            fromName => (text: Expr[String]) =>
              // (with a config known at compile time `enumAsStrings` was checked then)
              if (dctx.evaluatedConfig.isDefined) fromName(Expr.quote(Expr.splice(text).trim), configExpr)
              else
                Expr.quote {
                  if (Expr.splice(configExpr).enumAsStrings)
                    Expr.splice(fromName(Expr.quote(Expr.splice(text).trim), configExpr))
                  else
                    XmlDerivationUtils.enumAsTextNeedsEnumAsStringsResult[T](Expr.splice(Expr(Type[T].plainPrint)))
                }
          }
        case XmlTextShape.BuiltIn if builtInAttributeDecoder[T].isDefined =>
          MIO.pure(builtInAttributeDecoder[T].get)
        case _ =>
          for {
            _ <- deriveDecoderRecursively[T](using dctx.nest[T](dctx.elem))
            helperOpt <- dctx.getHelper[T]
          } yield {
            val helper = helperOpt.get
            (text: Expr[String]) => helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(Expr.splice(text))), configExpr)
          }
      }
    }

    /** The attribute value of exactly a built-in type, decoded from its text by a runtime helper (the same result as
      * its decoder gives for an element holding the text, without building that element).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def builtInAttributeDecoder[T: Type]: Option[Expr[String] => Expr[Either[XmlDecodingError, T]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val StringT: Type[String] = DTypes.String
      implicit val EitherT: Type[Either[XmlDecodingError, T]] = DTypes.DecoderResult[T]
      def as(decode: Expr[String] => Expr[Either[XmlDecodingError, Any]]): Expr[String] => Expr[
        Either[XmlDecodingError, T]
      ] = text => decode(text).asInstanceOf[Expr[Either[XmlDecodingError, T]]]
      if (Type[T] =:= DTypes.String) Some(as(t => Expr.quote(XmlDerivationUtils.attributeString(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Int) Some(as(t => Expr.quote(XmlDerivationUtils.attributeInt(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Long) Some(as(t => Expr.quote(XmlDerivationUtils.attributeLong(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Double)
        Some(as(t => Expr.quote(XmlDerivationUtils.attributeDouble(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Float) Some(as(t => Expr.quote(XmlDerivationUtils.attributeFloat(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Boolean)
        Some(as(t => Expr.quote(XmlDerivationUtils.attributeBoolean(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Short) Some(as(t => Expr.quote(XmlDerivationUtils.attributeShort(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Byte) Some(as(t => Expr.quote(XmlDerivationUtils.attributeByte(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.Char) Some(as(t => Expr.quote(XmlDerivationUtils.attributeChar(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.BigDecimal)
        Some(as(t => Expr.quote(XmlDerivationUtils.attributeBigDecimal(Expr.splice(t)))))
      else if (Type[T] =:= DTypes.BigInt)
        Some(as(t => Expr.quote(XmlDerivationUtils.attributeBigInt(Expr.splice(t)))))
      else None
    }

    /** An `Option[Inner]` attribute: absent is `None` (how `None` is written), present is `Some` of the decoded value -
      * also when empty (`Some("")`), except that an empty value that does not decode (e.g. of an `Option[Int]`) is
      * `None`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def optionalAttributeDecodeExpr[A: DecoderCtx, Inner: Type](
        attrNameExpr: Expr[String],
        shape: XmlTextShape,
        whenAbsent: Expr[Any]
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      textDecoder[A, Inner](shape).map { decode =>
        def decodeValue(attrValue: Expr[String]): Expr[Either[XmlDecodingError, Any]] = Expr.quote {
          Expr.splice(decode(attrValue)).asInstanceOf[Either[XmlDecodingError, Any]]
        }
        Expr.quote {
          XmlDerivationUtils
            .getOptionalAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
            .flatMap {
              case scala.Some(attrValue) =>
                if (attrValue.trim.isEmpty)
                  XmlDerivationUtils
                    .decodedOrNone(Expr.splice(decodeValue(Expr.quote(attrValue)))): Either[XmlDecodingError, Any]
                else
                  Expr
                    .splice(decodeValue(Expr.quote(attrValue)))
                    .map(value => scala.Some(value)): Either[XmlDecodingError, Any]
              case scala.None =>
                Right(Expr.splice(whenAbsent)): Either[XmlDecodingError, Any]
            }
        }
      }
    }

    /** The `@xmlContent` field, from the text of the element (an `Option`: `None` without text). */
    @scala.annotation.nowarn("msg=is never used")
    private def contentDecodeExpr[A: DecoderCtx, FieldT: Type](
        shape: XmlAttrShape
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      def contentOf[T: Type]: MIO[Expr[Either[XmlDecodingError, Any]]] = shape.shape match {
        case XmlTextShape.StringEnum(subtypes) =>
          implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig
          DecoderHandleAsEnumRule.enumFromName[T](subtypes)(using dctx.nest[T](dctx.elem)).map { fromName =>
            val decoded: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
              Expr
                .splice(fromName(Expr.quote(Expr.splice(dctx.elem).text.trim), dctx.config))
                .asInstanceOf[Either[XmlDecodingError, Any]]
            }
            // (with a config known at compile time `enumAsStrings` was checked then)
            if (dctx.evaluatedConfig.isDefined) decoded
            else
              Expr.quote {
                if (Expr.splice(dctx.config).enumAsStrings) Expr.splice(decoded)
                else
                  XmlDerivationUtils.enumAsTextNeedsEnumAsStringsResult[Any](Expr.splice(Expr(Type[T].plainPrint)))
              }
          }
        case _ =>
          deriveDecoderRecursively[T](using dctx.nest[T](dctx.elem)).map { decoded =>
            Expr.quote(Expr.splice(decoded).asInstanceOf[Either[XmlDecodingError, Any]])
          }
      }
      Type[FieldT] match {
        case IsOption(isOption) if shape.optional =>
          import isOption.Underlying as Inner
          // blank text is a `String`/`Char` value: only no text at all is `None` there
          val keepsBlanks = shape.shape == XmlTextShape.BuiltIn && isXmlStringValue(Type[Inner])
          contentOf[Inner].map { decoded =>
            val some: Expr[Either[XmlDecodingError, Any]] =
              Expr.quote(Expr.splice(decoded).map(value => scala.Some(value)): Either[XmlDecodingError, Any])
            if (keepsBlanks)
              Expr.quote {
                if (Expr.splice(dctx.elem).text.isEmpty) Right(scala.None): Either[XmlDecodingError, Any]
                else Expr.splice(some)
              }
            else
              Expr.quote {
                if (Expr.splice(dctx.elem).text.trim.isEmpty) Right(scala.None): Either[XmlDecodingError, Any]
                else Expr.splice(some)
              }
          }
        case _ => contentOf[FieldT]
      }
    }

    /** A collection/map field from the child elements named after the field (`@xmlUnwrapped`, or inside the
      * `@xmlWrapper` element).
      */
    @scala.annotation.nowarn("msg=is never used")
    private def repeatedDecodeExpr[A: DecoderCtx, FieldT: Type](
        nameExpr: Expr[String]
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      implicit val EitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
      val fieldCtx: DecoderCtx[FieldT] = dctx.nest[FieldT](dctx.elem)
      val decoded: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, FieldT]]]] = Type[FieldT] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          isCollection.value.asMap match {
            case Some(isMap) =>
              DecoderHandleAsMapRule.decodeMapEntries[FieldT, Item](isMap, Some(nameExpr))(using
                fieldCtx,
                implicitly[Type[Item]]
              )
            case None =>
              DecoderHandleAsCollectionRule.decodeItems[FieldT, Item](isCollection.value, Some(nameExpr))(using
                fieldCtx,
                implicitly[Type[Item]]
              )
          }
        case _ => MIO.pure(Rule.yielded(s"${Type[FieldT].prettyPrint} is not a collection or a map"))
      }
      decoded.flatMap {
        case Rule.Applicability.Matched(result) =>
          MIO.pure(Expr.quote(Expr.splice(result).asInstanceOf[Either[XmlDecodingError, Any]]))
        case Rule.Applicability.Yielded(reasons) =>
          MIO.fail(DecoderDerivationError.UnsupportedType(Type[FieldT].prettyPrint, reasons.toList))
      }
    }

    /** The decoding of one field, as `Either[XmlDecodingError, Any]`. */
    @scala.annotation.nowarn("msg=is never used")
    private def fieldDecodeExpr[A: DecoderCtx, FieldT: Type](
        decoding: FieldDecoding
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig

      // the cached decode helper of the field's type: (element, config) => result
      def withHelper(
          body: ((Expr[scala.xml.Elem], Expr[XmlConfig]) => Expr[Either[XmlDecodingError, FieldT]]) => Expr[
            Either[XmlDecodingError, Any]
          ]
      ): MIO[Expr[Either[XmlDecodingError, Any]]] = for {
        _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
        helperOpt <- dctx.getHelper[FieldT]
      } yield body(helperOpt.get)

      // how the field is decoded from the text of its attribute
      def withAttribute(shape: XmlTextShape)(
          body: (Expr[String] => Expr[Either[XmlDecodingError, FieldT]]) => Expr[Either[XmlDecodingError, Any]]
      ): MIO[Expr[Either[XmlDecodingError, Any]]] = textDecoder[A, FieldT](shape).map(body)

      val configExpr = dctx.config
      decoding match {
        case FieldDecoding.Default(defaultExpr) =>
          MIO.pure(Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]))

        case FieldDecoding.FromContent(shape) => contentDecodeExpr[A, FieldT](shape)

        case FieldDecoding.FromAttribute(attrNameExpr, shape) =>
          withAttribute(shape) { decode =>
            Expr.quote {
              XmlDerivationUtils
                .getAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
                .flatMap(attrValue => Expr.splice(decode(Expr.quote(attrValue))))
                .asInstanceOf[Either[XmlDecodingError, Any]]
            }
          }

        case FieldDecoding.FromAttributeWithDefault(attrNameExpr, shape, defaultExpr) =>
          // useDefaults is statically true — an absent attribute is the default
          withAttribute(shape) { decode =>
            Expr.quote {
              XmlDerivationUtils
                .getOptionalAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
                .flatMap {
                  case scala.Some(attrValue) =>
                    Expr.splice(decode(Expr.quote(attrValue))).asInstanceOf[Either[XmlDecodingError, Any]]
                  case scala.None =>
                    Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                }
            }
          }

        case FieldDecoding.FromAttributeWithRuntimeDefault(attrNameExpr, shape, defaultExpr) =>
          // useDefaults not statically known — an absent attribute is the default only when it is enabled
          withAttribute(shape) { decode =>
            Expr.quote {
              val attrName = Expr.splice(attrNameExpr)
              XmlDerivationUtils
                .getOptionalAttribute(Expr.splice(dctx.elem), attrName)
                .flatMap {
                  case scala.Some(attrValue) =>
                    Expr.splice(decode(Expr.quote(attrValue))).asInstanceOf[Either[XmlDecodingError, Any]]
                  case scala.None =>
                    if (Expr.splice(configExpr).useDefaults)
                      Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                    else
                      Left(
                        XmlDecodingError.MissingAttribute(attrName, Expr.splice(dctx.elem).label)
                      ): Either[XmlDecodingError, Any]
                }
            }
          }

        case FieldDecoding.FromOptionalAttribute(attrNameExpr, shape, whenAbsent) =>
          Type[FieldT] match {
            case IsOption(isOption) =>
              import isOption.Underlying as Inner
              optionalAttributeDecodeExpr[A, Inner](attrNameExpr, shape, whenAbsent)
            case _ =>
              MIO.fail(new RuntimeException(s"Expected an Option attribute, got ${Type[FieldT].prettyPrint}"))
          }

        case FieldDecoding.FromRepeatedChildren(childNameExpr, defaultWhenAbsent, defaultNeedsUseDefaults) =>
          repeatedDecodeExpr[A, FieldT](childNameExpr).map { decoded =>
            defaultWhenAbsent match {
              case None              => decoded
              case Some(defaultExpr) =>
                val absent: Expr[Boolean] = Expr.quote {
                  !XmlDerivationUtils.hasChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                }
                val useDefault: Expr[Boolean] =
                  if (defaultNeedsUseDefaults) Expr.quote(Expr.splice(configExpr).useDefaults && Expr.splice(absent))
                  else absent
                Expr.quote {
                  if (Expr.splice(useDefault)) Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                  else Expr.splice(decoded)
                }
            }
          }

        case FieldDecoding.FromWrapper(wrapperName, inner, defaultWhenAbsent, defaultNeedsUseDefaults) =>
          MIO.scoped { runSafe =>
            ValDefs
              .createVal[scala.xml.Elem](
                Expr.quote {
                  XmlDerivationUtils.childElemOrNull(Expr.splice(dctx.elem), Expr.splice(Expr(wrapperName)))
                },
                FreshName.FromPrefix("wrapperOrNull")
              )
              .use { wrapperOrNull =>
                // an absent wrapper is read as an empty one: what is inside it is absent
                val fromWrapper: Expr[Either[XmlDecodingError, Any]] = ValDefs
                  .createVal[scala.xml.Elem](
                    Expr.quote {
                      if (Expr.splice(wrapperOrNull) == null)
                        XmlDerivationUtils.makeEmptyElem(Expr.splice(Expr(wrapperName)))
                      else Expr.splice(wrapperOrNull)
                    },
                    FreshName.FromPrefix("wrapper")
                  )
                  .use { wrapper =>
                    runSafe(
                      fieldDecodeExpr[A, FieldT](inner)(using dctx.copy(elem = wrapper), implicitly[Type[FieldT]])
                    )
                  }
                defaultWhenAbsent match {
                  case None              => fromWrapper
                  case Some(defaultExpr) =>
                    val absent: Expr[Boolean] = Expr.quote(Expr.splice(wrapperOrNull) == null)
                    val useDefault: Expr[Boolean] =
                      if (defaultNeedsUseDefaults)
                        Expr.quote(Expr.splice(absent) && Expr.splice(configExpr).useDefaults)
                      else absent
                    Expr.quote {
                      if (Expr.splice(useDefault)) Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                      else Expr.splice(fromWrapper)
                    }
                }
              }
          }

        case FieldDecoding.FromChildElementOrEmpty(childNameExpr) =>
          withHelper { helper =>
            Expr.quote {
              val childName = Expr.splice(childNameExpr)
              XmlDerivationUtils
                .getOptionalChildElem(Expr.splice(dctx.elem), childName)
                .flatMap { childElemOpt =>
                  val childElem = childElemOpt match {
                    case scala.Some(found) => found
                    case scala.None        => XmlDerivationUtils.makeElem(childName, Nil, Nil)
                  }
                  Expr
                    .splice(helper(Expr.quote(childElem), configExpr))
                    .asInstanceOf[Either[XmlDecodingError, Any]]
                }
            }
          }

        case FieldDecoding.FromChildElement(childNameExpr) =>
          withHelper { helper =>
            Expr.quote {
              XmlDerivationUtils
                .getChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                .flatMap { childElem =>
                  Expr
                    .splice(helper(Expr.quote(childElem), configExpr))
                    .asInstanceOf[Either[XmlDecodingError, Any]]
                }
            }
          }

        case FieldDecoding.FromChildElementWithDefault(childNameExpr, defaultExpr) =>
          // useDefaults is statically true — use optional child lookup with fallback
          withHelper { helper =>
            Expr.quote {
              XmlDerivationUtils
                .getOptionalChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                .flatMap {
                  case scala.Some(childElem) =>
                    Expr
                      .splice(helper(Expr.quote(childElem), configExpr))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
                  case scala.None =>
                    Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                }
            }
          }

        case FieldDecoding.FromChildElementWithRuntimeDefault(childNameExpr, defaultExpr) =>
          // useDefaults not statically known — emit runtime check against config
          withHelper { helper =>
            Expr.quote {
              if (Expr.splice(configExpr).useDefaults) {
                XmlDerivationUtils
                  .getOptionalChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                  .flatMap {
                    case scala.Some(childElem) =>
                      Expr
                        .splice(helper(Expr.quote(childElem), configExpr))
                        .asInstanceOf[Either[XmlDecodingError, Any]]
                    case scala.None =>
                      Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                  }
              } else {
                XmlDerivationUtils
                  .getChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                  .flatMap { childElem =>
                    Expr
                      .splice(helper(Expr.quote(childElem), configExpr))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
                  }
              }
            }
          }

        case FieldDecoding.DynamicMode(asAttribute, asElement, needsEnumAsStrings) =>
          // defaultFieldMode not statically known — read it from the config
          for {
            fromAttribute <- fieldDecodeExpr[A, FieldT](asAttribute)
            fromElement <- fieldDecodeExpr[A, FieldT](asElement)
          } yield
            if (needsEnumAsStrings)
              Expr.quote {
                if (
                  XmlDerivationUtils.isAttributeMode(Expr.splice(configExpr).defaultFieldMode) &&
                  Expr.splice(configExpr).enumAsStrings
                ) Expr.splice(fromAttribute)
                else Expr.splice(fromElement)
              }
            else
              Expr.quote {
                if (XmlDerivationUtils.isAttributeMode(Expr.splice(configExpr).defaultFieldMode))
                  Expr.splice(fromAttribute)
                else Expr.splice(fromElement)
              }
      }
    }

    /** Resolve a parameter's default value to an Expr[Any]. */
    private def resolveDefaultValue(fName: String, param: Parameter): Expr[Any] =
      param.defaultValue match {
        case Some(method) =>
          foldInstanceFree(method, "Default value")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ) match {
            case Right(expr) => expr.value.asInstanceOf[Expr[Any]]
            case Left(_)     =>
              Environment.reportErrorAndAbort(
                s"Field '$fName' has useDefaults enabled but its default value could not be resolved"
              )
          }
        case None =>
          Environment.reportErrorAndAbort(
            s"Field '$fName' has useDefaults enabled but has no default value"
          )
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def buildDecodeExpr[A: DecoderCtx](
        caseClass: CaseClass[A],
        constructor: Method,
        fields: List[(String, Parameter)],
        decodings: List[FieldDecoding]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      implicit val XmlConfigT: Type[XmlConfig] = DTypes.XmlConfig

      fields
        .zip(decodings)
        .foldLeft(MIO.pure(List.empty[(Expr[Either[XmlDecodingError, Any]], Expr[Any] => (String, Expr_??))])) {
          case (accMIO, ((fName, param), decoding)) =>
            import param.tpe.Underlying as FieldT
            for {
              acc <- accMIO
              decodeExpr <- fieldDecodeExpr[A, FieldT](decoding)
            } yield {
              val cast: Expr[Any] => (String, Expr_??) = value => (fName, castTo[FieldT](value).as_??)
              acc :+ ((decodeExpr, cast))
            }
        }
        .map { fieldData =>
          val fieldDecodeExprs = fieldData.map(_._1)
          val fieldCasts = fieldData.map(_._2)

          val constructFF: List[Expr[Any]] => Expr[A] = { values =>
            val fieldMap: Map[String, Expr_??] = fieldCasts.zip(values).map { case (cast, value) => cast(value) }.toMap
            foldInstanceFree(constructor, "Constructor")(
              onTypes = _ => Map.empty,
              onValues = _ => fieldMap
            ) match {
              case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
              case Left(error)          =>
                Environment.reportErrorAndAbort(
                  DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, Some(error)).message
                )
            }
          }
          // The first failing field (in field order) is the result and the later ones are not decoded: each field is
          // decoded by its own local def, and the value is constructed when the last result is a `Right` (see
          // FlatFieldsConstruct).
          constructFailFast[Either[XmlDecodingError, Any], A, Either[XmlDecodingError, A]](fieldDecodeExprs)(
            isSuccess = r => Expr.quote(Expr.splice(r).isRight),
            valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[XmlDecodingError, Any]].value),
            construct = constructFF,
            onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[XmlDecodingError, A]),
            onFailure = r => Expr.quote(Expr.splice(r).asInstanceOf[Either[XmlDecodingError, A]])
          )
        }
    }

    /** `value.asInstanceOf[F]` (a method of its own so that `F` is a type parameter in the quote). */
    private def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])
  }

}
