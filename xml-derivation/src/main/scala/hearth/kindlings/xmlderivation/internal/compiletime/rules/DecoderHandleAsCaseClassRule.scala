package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError, XmlFieldMode}
import hearth.kindlings.xmlderivation.annotations.{transientField, xmlAttribute, xmlContent, xmlElement, xmlName}
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
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField
      implicit val xmlNameT: Type[xmlName] = DTypes.XmlNameAnnotation
      implicit val xmlAttributeT: Type[xmlAttribute] = DTypes.XmlAttributeAnnotation
      implicit val xmlContentT: Type[xmlContent] = DTypes.XmlContentAnnotation
      implicit val xmlElementT: Type[xmlElement] = DTypes.XmlElementAnnotation

      val constructor = caseClass.primaryConstructor

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .traverse { case (fName, param) =>
              import param.tpe.Underlying as FieldType
              val isTransient = hasAnnotationType[transientField](param)
              val customName = getAnnotationStringArg[xmlName](param)
              val isAttrAnnotated = hasAnnotationType[xmlAttribute](param)
              val isContentAnnotated = hasAnnotationType[xmlContent](param)
              val isElemAnnotated = hasAnnotationType[xmlElement](param)
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

              if (isTransient) {
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
                MIO.pure(FieldDecoding.Default(defaultValue))
              } else if (isContentAnnotated) {
                // Decode from text content
                deriveDecoderRecursively[FieldType](using dctx.nest[FieldType](dctx.elem)).map { decodedExpr =>
                  FieldDecoding.FromContent(decodedExpr.asInstanceOf[Expr[Either[XmlDecodingError, Any]]])
                }
              } else {
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
                def asAttribute: FieldDecoding =
                  if (isOptionField) FieldDecoding.FromOptionalAttribute(xmlFieldNameExpr, param.tpe, optionWhenAbsent)
                  else if (hasDefault && useDefaultsStaticallyTrue)
                    FieldDecoding.FromAttributeWithDefault(
                      xmlFieldNameExpr,
                      param.tpe,
                      resolveDefaultValue(fName, param)
                    )
                  else if (hasDefault && !useDefaultsStaticallyFalse)
                    FieldDecoding
                      .FromAttributeWithRuntimeDefault(xmlFieldNameExpr, param.tpe, resolveDefaultValue(fName, param))
                  else FieldDecoding.FromAttribute(xmlFieldNameExpr, param.tpe)

                // Decode from a child element
                def asElement: FieldDecoding =
                  if (isOptionField)
                    FieldDecoding.FromChildElementWithDefault(xmlFieldNameExpr, param.tpe, optionWhenAbsent)
                  else if (hasDefault && useDefaultsStaticallyTrue) {
                    // useDefaults is statically true — use default when missing
                    FieldDecoding
                      .FromChildElementWithDefault(xmlFieldNameExpr, param.tpe, resolveDefaultValue(fName, param))
                  } else if (hasDefault && !useDefaultsStaticallyFalse) {
                    // useDefaults is not statically known — emit runtime check
                    FieldDecoding
                      .FromChildElementWithRuntimeDefault(
                        xmlFieldNameExpr,
                        param.tpe,
                        resolveDefaultValue(fName, param)
                      )
                  } else if (isCollectionField) {
                    // No default or useDefaults is statically false. An absent collection/map element is decoded as
                    // an empty element, i.e. an empty collection (as written by `withTransientEmpty`).
                    FieldDecoding.FromChildElementOrEmpty(xmlFieldNameExpr, param.tpe)
                  } else FieldDecoding.FromChildElement(xmlFieldNameExpr, param.tpe)

                if (isAttrAnnotated) MIO.pure(asAttribute)
                else if (isElemAnnotated || !isXmlAttributeValue(Type[FieldType])) {
                  // `@xmlElement`, or not a single text value (a collection, map, case class, sealed trait...): the
                  // encoder writes it as a child element in both modes
                  MIO.pure(asElement)
                } else
                  // `defaultFieldMode` decides, as in the encoder: in the macro when the config is known
                  dctx.evaluatedConfig.map(_.defaultFieldMode == XmlFieldMode.Attribute) match {
                    case Some(true)  => MIO.pure(asAttribute)
                    case Some(false) => MIO.pure(asElement)
                    case None        => MIO.pure(FieldDecoding.DynamicMode(asAttribute, asElement))
                  }
              }
            }
            .flatMap { decodings =>
              buildDecodeExpr[A](caseClass, constructor, fields, decodings.toList)
            }
        case None =>
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

    /** An `Option[Inner]` attribute: absent is `None` (how `None` is written), present is `Some` of the decoded value -
      * also when empty (`Some("")`), except that an empty value that does not decode (e.g. of an `Option[Int]`) is
      * `None`.
      */
    @scala.annotation.nowarn("msg=is never used")
    private def optionalAttributeDecodeExpr[A: DecoderCtx, Inner: Type](
        attrNameExpr: Expr[String],
        whenAbsent: Expr[Any]
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny
      for {
        _ <- deriveDecoderRecursively[Inner](using dctx.nest[Inner](dctx.elem))
        helperOpt <- dctx.getHelper[Inner]
      } yield {
        val configExpr = dctx.config
        val helper = helperOpt.get
        def decodeValue(attrValue: Expr[String]): Expr[Either[XmlDecodingError, Any]] = Expr.quote {
          Expr
            .splice(helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(Expr.splice(attrValue))), configExpr))
            .asInstanceOf[Either[XmlDecodingError, Any]]
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

    /** The decoding of one field, as `Either[XmlDecodingError, Any]`. */
    @scala.annotation.nowarn("msg=is never used")
    private def fieldDecodeExpr[A: DecoderCtx, FieldT: Type](
        decoding: FieldDecoding
    ): MIO[Expr[Either[XmlDecodingError, Any]]] = {
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
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

      val configExpr = dctx.config
      decoding match {
        case FieldDecoding.Default(defaultExpr) =>
          MIO.pure(Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]))

        case FieldDecoding.FromContent(decodedExpr) =>
          MIO.pure(decodedExpr)

        case FieldDecoding.FromAttribute(attrNameExpr, _) =>
          withHelper { helper =>
            Expr.quote {
              XmlDerivationUtils
                .getAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
                .flatMap { attrValue =>
                  Expr.splice(helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(attrValue)), configExpr))
                }
                .asInstanceOf[Either[XmlDecodingError, Any]]
            }
          }

        case FieldDecoding.FromAttributeWithDefault(attrNameExpr, _, defaultExpr) =>
          // useDefaults is statically true — an absent attribute is the default
          withHelper { helper =>
            Expr.quote {
              XmlDerivationUtils
                .getOptionalAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
                .flatMap {
                  case scala.Some(attrValue) =>
                    Expr
                      .splice(helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(attrValue)), configExpr))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
                  case scala.None =>
                    Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any]
                }
            }
          }

        case FieldDecoding.FromAttributeWithRuntimeDefault(attrNameExpr, _, defaultExpr) =>
          // useDefaults not statically known — an absent attribute is the default only when it is enabled
          withHelper { helper =>
            Expr.quote {
              val attrName = Expr.splice(attrNameExpr)
              XmlDerivationUtils
                .getOptionalAttribute(Expr.splice(dctx.elem), attrName)
                .flatMap {
                  case scala.Some(attrValue) =>
                    Expr
                      .splice(helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(attrValue)), configExpr))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
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

        case FieldDecoding.FromOptionalAttribute(attrNameExpr, _, whenAbsent) =>
          Type[FieldT] match {
            case IsOption(isOption) =>
              import isOption.Underlying as Inner
              optionalAttributeDecodeExpr[A, Inner](attrNameExpr, whenAbsent)
            case _ =>
              MIO.fail(new RuntimeException(s"Expected an Option attribute, got ${Type[FieldT].prettyPrint}"))
          }

        case FieldDecoding.FromChildElementOrEmpty(childNameExpr, _) =>
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

        case FieldDecoding.FromChildElement(childNameExpr, _) =>
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

        case FieldDecoding.FromChildElementWithDefault(childNameExpr, _, defaultExpr) =>
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

        case FieldDecoding.FromChildElementWithRuntimeDefault(childNameExpr, _, defaultExpr) =>
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

        case FieldDecoding.DynamicMode(asAttribute, asElement) =>
          // defaultFieldMode not statically known — read it from the config
          for {
            fromAttribute <- fieldDecodeExpr[A, FieldT](asAttribute)
            fromElement <- fieldDecodeExpr[A, FieldT](asElement)
          } yield Expr.quote {
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
