package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.{XmlConfig, XmlDecodingError}
import hearth.kindlings.xmlderivation.annotations.{transientField, xmlAttribute, xmlContent, xmlName}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass, caseClass.primaryConstructor.totalParameters.flatten.toList)
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
              } else if (isAttrAnnotated) {
                // Decode from attribute
                MIO.pure(FieldDecoding.FromAttribute(xmlFieldNameExpr, param.tpe))
              } else {
                // Decode from child element (default)
                // Check useDefaults: if enabled and field has a default, wrap with optional lookup
                val hasDefault = param.hasDefault
                val useDefaultsStaticallyTrue = dctx.evaluatedConfig.exists(_.useDefaults)
                val useDefaultsStaticallyFalse = dctx.evaluatedConfig.exists(!_.useDefaults)

                if (hasDefault && useDefaultsStaticallyTrue) {
                  // useDefaults is statically true — use default when missing
                  val defaultExpr: Expr[Any] = resolveDefaultValue(fName, param)
                  MIO.pure(FieldDecoding.FromChildElementWithDefault(xmlFieldNameExpr, param.tpe, defaultExpr))
                } else if (hasDefault && !useDefaultsStaticallyFalse) {
                  // useDefaults is not statically known — emit runtime check
                  val defaultExpr: Expr[Any] = resolveDefaultValue(fName, param)
                  MIO.pure(FieldDecoding.FromChildElementWithRuntimeDefault(xmlFieldNameExpr, param.tpe, defaultExpr))
                } else {
                  // No default or useDefaults is statically false
                  MIO.pure(FieldDecoding.FromChildElement(xmlFieldNameExpr, param.tpe))
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

      // Build accessor types needed later
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val IntT: Type[Int] = DTypes.Int

      fields
        .zip(decodings)
        .zipWithIndex
        .foldLeft(MIO.pure(List.empty[(Expr[Either[XmlDecodingError, Any]], Expr[Array[Any]] => (String, Expr_??))])) {
          case (accMIO, (((fName, param), decoding), idx)) =>
            import param.tpe.Underlying as FieldT
            accMIO.flatMap { acc =>
              decoding match {
                case FieldDecoding.Default(defaultExpr) =>
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] =
                    Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any])
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                  }
                  MIO.pure(acc :+ ((decodeExpr, makeAccessor)))

                case FieldDecoding.FromContent(decodedExpr) =>
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                  }
                  MIO.pure(acc :+ ((decodedExpr, makeAccessor)))

                case FieldDecoding.FromAttribute(attrNameExpr, _) =>
                  for {
                    _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
                    helperOpt <- dctx.getHelper[FieldT]
                  } yield {
                    val configExpr = dctx.config
                    val helper = helperOpt.get
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                      XmlDerivationUtils
                        .getAttribute(Expr.splice(dctx.elem), Expr.splice(attrNameExpr))
                        .flatMap { attrValue =>
                          Expr
                            .splice(helper(Expr.quote(XmlDerivationUtils.wrapAttrAsElem(attrValue)), configExpr))
                        }
                        .asInstanceOf[Either[XmlDecodingError, Any]]
                    }
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                    }
                    acc :+ ((decodeExpr, makeAccessor))
                  }

                case FieldDecoding.FromChildElement(childNameExpr, _) =>
                  for {
                    _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
                    helperOpt <- dctx.getHelper[FieldT]
                  } yield {
                    val configExpr = dctx.config
                    val helper = helperOpt.get
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                      XmlDerivationUtils
                        .getChildElem(Expr.splice(dctx.elem), Expr.splice(childNameExpr))
                        .flatMap { childElem =>
                          Expr
                            .splice(helper(Expr.quote(childElem), configExpr))
                            .asInstanceOf[Either[XmlDecodingError, Any]]
                        }
                    }
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                    }
                    acc :+ ((decodeExpr, makeAccessor))
                  }

                case FieldDecoding.FromChildElementWithDefault(childNameExpr, _, defaultExpr) =>
                  // useDefaults is statically true — use optional child lookup with fallback
                  for {
                    _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
                    helperOpt <- dctx.getHelper[FieldT]
                  } yield {
                    val configExpr = dctx.config
                    val helper = helperOpt.get
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
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
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                    }
                    acc :+ ((decodeExpr, makeAccessor))
                  }

                case FieldDecoding.FromChildElementWithRuntimeDefault(childNameExpr, _, defaultExpr) =>
                  // useDefaults not statically known — emit runtime check against config
                  for {
                    _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
                    helperOpt <- dctx.getHelper[FieldT]
                  } yield {
                    val configExpr = dctx.config
                    val helper = helperOpt.get
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
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
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                    }
                    acc :+ ((decodeExpr, makeAccessor))
                  }
              }
            }
        }
        .flatMap { fieldData =>
          val fieldDecodeExprs = fieldData.map(_._1)
          val makeAccessors = fieldData.map(_._2)

          // Build the result list at compile time using foldRight
          val listExpr: Expr[List[Either[XmlDecodingError, Any]]] =
            fieldDecodeExprs.foldRight(Expr.quote(List.empty[Either[XmlDecodingError, Any]])) { (elem, acc) =>
              Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
            }

          // Build the constructor lambda using LambdaBuilder + primaryConstructor
          LambdaBuilder
            .of1[Array[Any]]("decodedValues")
            .traverse { decodedValuesExpr =>
              val fieldMap: Map[String, Expr_??] =
                makeAccessors.map(_(decodedValuesExpr)).toMap
              foldInstanceFree(constructor, "Constructor")(
                onTypes = _ => Map.empty,
                onValues = _ => fieldMap
              ) match {
                case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
                case Left(error)          =>
                  val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, Some(error))
                  Log.error(err.message) >> MIO.fail(err)
              }
            }
            .map { builder =>
              val constructLambda = builder.build[A]
              Expr.quote {
                val fieldResults: List[Either[XmlDecodingError, Any]] = Expr.splice(listExpr)
                XmlDerivationUtils.sequenceDecodeResults(fieldResults).map { arr =>
                  Expr.splice(constructLambda).apply(arr)
                }
              }
            }
        }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def mkFieldAccess[F: Type](arrExpr: Expr[Array[Any]], idx: Int): Expr[F] = {
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val IntT: Type[Int] = DTypes.Int
      Expr.quote {
        Expr.splice(arrExpr)(Expr.splice(Expr(idx))).asInstanceOf[F]
      }
    }
  }

}
