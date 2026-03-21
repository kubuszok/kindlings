package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError
import hearth.kindlings.xmlderivation.annotations.{transientField, xmlAttribute, xmlContent, xmlName}
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass, caseClass.primaryConstructor.parameters.flatten.toList)
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
              val xmlFieldName = customName.getOrElse(fName)

              if (isTransient) {
                val defaultValue: Expr[Any] = param.defaultValue match {
                  case Some(existentialOuter) =>
                    val methodOf = existentialOuter.value
                    methodOf.value match {
                      case noInstance: Method.NoInstance[?] =>
                        noInstance(Map.empty) match {
                          case Right(expr) => expr.asInstanceOf[Expr[Any]]
                          case Left(_)     =>
                            Environment.reportErrorAndAbort(
                              s"Field '$fName' is annotated with @transientField but its default value could not be resolved"
                            )
                        }
                      case _ =>
                        Environment.reportErrorAndAbort(
                          s"Field '$fName' is annotated with @transientField but has no default value"
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
                MIO.pure(FieldDecoding.FromAttribute(xmlFieldName, param.tpe))
              } else {
                // Decode from child element (default)
                MIO.pure(FieldDecoding.FromChildElement(xmlFieldName, param.tpe))
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
                MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}"))
            }
      }
    }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def buildDecodeExpr[A: DecoderCtx](
        caseClass: CaseClass[A],
        constructor: Method.NoInstance[A],
        fields: List[(String, Parameter)],
        decodings: List[FieldDecoding]
    ): MIO[Expr[Either[XmlDecodingError, A]]] = {
      implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
      implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
      implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
      implicit val StringT: Type[String] = DTypes.String
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherAnyT: Type[Either[XmlDecodingError, Any]] = DTypes.DecoderResultAny

      // Build accessor types needed later
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val IntT: Type[Int] = DTypes.Int

      // Build decode expressions, child lambdas, and accessor functions in a single pass.
      // This avoids path-dependent type issues with Expr.quote on Scala 2 (the old code used
      // `import param.tpe.Underlying as FieldT` inside Expr.quote's asInstanceOf, which leaked
      // the path `param` into generated code). Now we use unsafeCastWithFn with the child lambda
      // for type inference, keeping path-dependent types out of Expr.quote.
      fields
        .zip(decodings)
        .zipWithIndex
        .foldLeft(MIO.pure(List.empty[(Expr[Either[XmlDecodingError, Any]], Expr[Array[Any]] => (String, Expr_??))])) {
          case (accMIO, (((fName, param), decoding), idx)) =>
            import param.tpe.Underlying as FieldT
            accMIO.flatMap { acc =>
              decoding match {
                case FieldDecoding.Default(defaultExpr) =>
                  // Build a decode lambda purely for type inference in the accessor.
                  // This avoids using path-dependent FieldT inside Expr.quote.
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] =
                    Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any])
                  LambdaBuilder
                    .of1[scala.xml.Elem]("dummyElem")
                    .traverse { dummyElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dummyElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
                    }

                case FieldDecoding.FromContent(decodedExpr) =>
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  LambdaBuilder
                    .of1[scala.xml.Elem]("contentElem")
                    .traverse { contentElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](contentElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodedExpr, makeAccessor)
                    }

                case FieldDecoding.FromAttribute(attrName, _) =>
                  // Build a decode lambda for type inference in the accessor.
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                    XmlDerivationUtils
                      .getAttribute(Expr.splice(dctx.elem), Expr.splice(Expr(attrName)))
                      .asInstanceOf[Either[XmlDecodingError, Any]]
                  }
                  LambdaBuilder
                    .of1[scala.xml.Elem]("attrElem")
                    .traverse { attrElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](attrElemExpr))
                    }
                    .map { builder =>
                      val castLambda = builder
                        .build[Either[XmlDecodingError, FieldT]]
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(castLambda)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
                    }

                case FieldDecoding.FromChildElement(childName, _) =>
                  implicit val eitherFieldT: Type[Either[XmlDecodingError, FieldT]] = DTypes.DecoderResult[FieldT]
                  LambdaBuilder
                    .of1[scala.xml.Elem]("childElem")
                    .traverse { childElemExpr =>
                      deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](childElemExpr))
                    }
                    .map { builder =>
                      val childLambda = builder.build[Either[XmlDecodingError, FieldT]]
                      val childLambdaAsAny = childLambda
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, Any]]]
                      val childLambdaForCast = childLambda
                        .asInstanceOf[Expr[scala.xml.Elem => Either[XmlDecodingError, FieldT]]]
                      val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                        XmlDerivationUtils.getChildElem(Expr.splice(dctx.elem), Expr.splice(Expr(childName))).flatMap {
                          childElem =>
                            Expr.splice(childLambdaAsAny).apply(childElem)
                        }
                      }
                      val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                        val typedExpr: Expr[FieldT] = Expr.quote {
                          XmlDerivationUtils.unsafeCastWithFn(
                            Expr.splice(arrExpr)(Expr.splice(Expr(idx))),
                            Expr.splice(childLambdaForCast)
                          )
                        }
                        (fName, typedExpr.as_??)
                      }
                      acc :+ (decodeExpr, makeAccessor)
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
              constructor(fieldMap) match {
                case Right(constructExpr) => MIO.pure(constructExpr)
                case Left(error)          =>
                  MIO.fail(new RuntimeException(s"Cannot construct ${Type[A].prettyPrint}: $error"))
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
  }

}
