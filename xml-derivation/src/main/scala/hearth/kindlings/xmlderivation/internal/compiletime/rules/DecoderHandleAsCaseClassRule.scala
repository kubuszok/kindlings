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

      // Per project rule 5, [[LambdaBuilder]] is reserved for collection / Optional iteration lambdas.
      // The previous implementation built per-field "cast lambdas" (`Elem => Either[…, FieldT]`) only
      // to satisfy `unsafeCastWithFn`'s type inference, with the path-dependent `FieldT` import staying
      // outside `Expr.quote`. We now use the helper-method pattern (`mkFieldAccess[F: Type]`) — the path
      // dependence is bound to a regular type parameter at the helper's call site, never leaking into
      // the reified `Expr.quote`. For the FromChildElement case the cached helper from `setHelper` is
      // used directly inside a cross-quotes `flatMap` callback (Either's flatMap is shape-equivalent
      // to Optional iteration so the lambda is per-rule-5 legitimate as well, but here we don't even
      // need a lambda — the body is inlined as part of the splice).
      fields
        .zip(decodings)
        .zipWithIndex
        .foldLeft(MIO.pure(List.empty[(Expr[Either[XmlDecodingError, Any]], Expr[Array[Any]] => (String, Expr_??))])) {
          case (accMIO, (((fName, param), decoding), idx)) =>
            import param.tpe.Underlying as FieldT
            accMIO.flatMap { acc =>
              decoding match {
                case FieldDecoding.Default(defaultExpr) =>
                  // Transient field: the value is the user-supplied default; no per-field decoder
                  // derivation is needed at this site.
                  val decodeExpr: Expr[Either[XmlDecodingError, Any]] =
                    Expr.quote(Right(Expr.splice(defaultExpr)): Either[XmlDecodingError, Any])
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                  }
                  MIO.pure(acc :+ ((decodeExpr, makeAccessor)))

                case FieldDecoding.FromContent(decodedExpr) =>
                  // The decoder body was already derived at the FieldDecoding stage in
                  // `decodeCaseClassFields` (line where `FieldDecoding.FromContent` is built);
                  // use that result directly.
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                  }
                  MIO.pure(acc :+ ((decodedExpr, makeAccessor)))

                case FieldDecoding.FromAttribute(attrName, _) =>
                  // Drive derivation for the field type (populates the shared cache via setHelper
                  // for downstream use); the resulting Expr is unused here because the runtime
                  // value is the raw attribute string, cast through `mkFieldAccess` at the
                  // accessor site (preserves the previous `unsafeCastWithFn`-based behavior).
                  deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem)).map { _ =>
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                      XmlDerivationUtils
                        .getAttribute(Expr.splice(dctx.elem), Expr.splice(Expr(attrName)))
                        .asInstanceOf[Either[XmlDecodingError, Any]]
                    }
                    val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                      (fName, mkFieldAccess[FieldT](arrExpr, idx).as_??)
                    }
                    acc :+ ((decodeExpr, makeAccessor))
                  }

                case FieldDecoding.FromChildElement(childName, _) =>
                  // Drive recursion to populate the shared cache via setHelper for FieldT, then
                  // retrieve the helper-call function and inline it inside a cross-quotes flatMap
                  // callback. The helper-call function uses the active Quotes at invocation time,
                  // so the resulting splice tree is built against the wrapping splice's Quotes.
                  for {
                    _ <- deriveDecoderRecursively[FieldT](using dctx.nest[FieldT](dctx.elem))
                    helperOpt <- dctx.getHelper[FieldT]
                  } yield {
                    val configExpr = dctx.config
                    val helper = helperOpt.get
                    val decodeExpr: Expr[Either[XmlDecodingError, Any]] = Expr.quote {
                      XmlDerivationUtils.getChildElem(Expr.splice(dctx.elem), Expr.splice(Expr(childName))).flatMap {
                        childElem =>
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

    /** Helper method that builds a typed field access expression from an `Array[Any]`. The path-dependent field type
      * `F` is bound to a regular type parameter at the call site, so it never appears inside the reified `Expr.quote`
      * body. This replaces the previous `unsafeCastWithFn`-with-phantom-decoder approach used to dodge path-dependent
      * type leakage on Scala 2. Same shape as `ubjson-derivation/.../DecoderHandleAsCaseClassRule.mkFieldAccessExpr`.
      */
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
