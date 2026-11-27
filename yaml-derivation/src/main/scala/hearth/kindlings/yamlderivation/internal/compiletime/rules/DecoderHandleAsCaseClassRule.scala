package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      val transientFields = fieldsList.filter { case (_, param) => hasAnnotationType[transientField](param) }
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      val transientDefaults: Map[String, Expr_??] = transientFields.flatMap { case (fName, param) =>
        param.defaultValue.flatMap { method =>
          foldInstanceFree(method, "Default value")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ).toOption
            .map(expr => (fName, expr))
        }
      }.toMap

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
                    val err = DecoderDerivationError.CannotConstructType(
                      Type[A].prettyPrint,
                      isSingleton = false,
                      Some(s"Unexpected parameter in zero-argument case class")
                    )
                    Log.error(err.message) >> MIO.fail(err)
                }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  YamlDerivationUtils.checkIsMapping(Expr.splice(dctx.node)).map(_ => Expr.splice(expr))
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherCEAnyT: Type[Either[ConstructError, Any]] = DTypes.EitherCEAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[ConstructError, Any]]] = DTypes.ListEitherCEAny
          implicit val FieldNodesT: Type[java.util.HashMap[String, Node]] = DTypes.FieldNodes
          implicit val EitherFieldNodesT: Type[Either[ConstructError, java.util.HashMap[String, Node]]] =
            DTypes.EitherCEFieldNodes

          val indexedFields = fields.toList.zipWithIndex
          // the names: a @fieldName, constants with a configuration known at compile time, otherwise computed once per
          // instance
          val namesMIO: MIO[String => Expr[String]] = dctx.evaluatedConfig match {
            case Some(evConfig) => MIO.pure((name: String) => Expr(evConfig.transformMemberNames(name)))
            case None           =>
              dctx.transformedMemberNames(fields.toList.collect {
                case (fName, param) if getAnnotationStringArg[fieldName](param).isEmpty => fName
              })
          }
          namesMIO.flatMap { nameOf =>
            NonEmptyList
              .fromList(indexedFields)
              .get
              .parTraverse { case ((fName, param), reindex) =>
                import param.tpe.Underlying as Field
                val nameOverride = getAnnotationStringArg[fieldName](param)
                val defaultAsAnyOpt: Option[Expr[Any]] =
                  if (param.hasDefault)
                    param.defaultValue.flatMap { method =>
                      foldInstanceFree(method, "Default value")(
                        onTypes = _ => Map.empty,
                        onValues = _ => Map.empty
                      ).toOption
                        .map { ee => import ee.Underlying; ee.value.upcast[Any] }
                    }
                  else None
                Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                  deriveFieldDecoder[Field].map { decoderExpr =>
                    val fieldNameExpr: Expr[String] = nameOverride match {
                      case Some(customName) => Expr(customName)
                      case None             => nameOf(fName)
                    }

                    // Check useDefaults statically when possible
                    val useDefaultsStaticallyFalse: Boolean =
                      dctx.evaluatedConfig.exists(!_.useDefaults)

                    // As scala-yaml's derived decoders, an absent `Option` field is `None` (unless its default is used).
                    val isOptionField: Boolean = Type[Field] match {
                      case IsOption(_) => true
                      case _           => false
                    }
                    // `fields`: the mapping's value nodes by key (`YamlDerivationUtils.fieldNodes`, once per decode)
                    def requiredFieldExpr(
                        fields: Expr[java.util.HashMap[String, Node]]
                    ): Expr[Either[ConstructError, Any]] =
                      if (isOptionField)
                        Expr.quote {
                          YamlDerivationUtils.decodeOptionalFieldOf(
                            Expr.splice(fields),
                            Expr.splice(fieldNameExpr),
                            Expr.splice(decoderExpr)
                          )
                        }
                      else
                        Expr.quote {
                          YamlDerivationUtils.decodeRequiredFieldOf(
                            Expr.splice(fields),
                            Expr.splice(dctx.node),
                            Expr.splice(fieldNameExpr),
                            Expr.splice(decoderExpr)
                          )
                        }

                    // As in scala-yaml, the default value is evaluated only when the field is absent (and on every such
                    // decode): it may be an expression with side effects.
                    def fieldOrDefaultExpr(
                        fields: Expr[java.util.HashMap[String, Node]],
                        defaultAnyExpr: Expr[Any]
                    ): Expr[Either[ConstructError, Any]] =
                      Expr.quote {
                        val present = YamlDerivationUtils.decodePresentFieldOf(
                          Expr.splice(fields),
                          Expr.splice(fieldNameExpr),
                          Expr.splice(decoderExpr)
                        )
                        if (present == null) (Right(Expr.splice(defaultAnyExpr)): Either[ConstructError, Any])
                        else present
                      }
                    def decodeExpr(fields: Expr[java.util.HashMap[String, Node]]): Expr[Either[ConstructError, Any]] =
                      defaultAsAnyOpt match {
                        case Some(defaultAnyExpr) if useDefaultsStaticallyFalse =>
                          // useDefaults is statically false: skip the default branch entirely
                          requiredFieldExpr(fields)
                        case Some(defaultAnyExpr) =>
                          dctx.evaluatedConfig match {
                            case Some(evConfig) if evConfig.useDefaults =>
                              // useDefaults is statically true: always use default path
                              fieldOrDefaultExpr(fields, defaultAnyExpr)
                            case _ =>
                              // useDefaults not statically known: runtime check
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults) Expr.splice(fieldOrDefaultExpr(fields, defaultAnyExpr))
                                else Expr.splice(requiredFieldExpr(fields))
                              }
                          }
                        case None => requiredFieldExpr(fields)
                      }
                    // The constructor argument: the field's value (read from its `Right`) cast to the field's type.
                    val makeFieldArg: Expr[Any] => (String, Expr_??) = { valExpr =>
                      (fName, castTo[Field](valExpr).as_??)
                    }
                    (decodeExpr(_), makeFieldArg)
                  }
                }
              }
              .flatMap { fieldData =>
                val decodeExprs = fieldData.toList.map(_._1)
                val makeFieldArgs = fieldData.toList.map(_._2)

                def constructFF(boundValues: List[Expr[Any]]): Expr[A] = {
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeFieldArgs.zip(boundValues).map { case (mk, v) => mk(v) }.toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                    case Left(error)          =>
                      Environment.reportErrorAndAbort(
                        DecoderDerivationError
                          .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                          .message
                      )
                  }
                }
                implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
                // Like scala-yaml, the first failing field (in field order) is reported and the later ones are not
                // decoded: each field is decoded by its own local def, and the value is constructed when the last
                // result is a `Right` (see FlatFieldsConstruct).
                // The mapping is read once (`fieldNodes`); each field is then a hash lookup.
                def construct(fields: Expr[java.util.HashMap[String, Node]]): Expr[Either[ConstructError, A]] =
                  constructFailFast[Either[ConstructError, Any], A, Either[ConstructError, A]](
                    decodeExprs.map(_(fields))
                  )(
                    isSuccess = r => Expr.quote(Expr.splice(r).isRight),
                    valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConstructError, Any]].value),
                    construct = constructFF,
                    onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConstructError, A]),
                    onFailure = r => Expr.quote(Expr.splice(r).asInstanceOf[Either[ConstructError, A]])
                  )
                MIO.pure(
                  ValDefs
                    .createVal[Either[ConstructError, java.util.HashMap[String, Node]]](
                      Expr.quote(YamlDerivationUtils.fieldNodes(Expr.splice(dctx.node))),
                      FreshName.FromPrefix("fieldNodes")
                    )
                    .use { fieldNodes =>
                      Expr.quote {
                        if (Expr.splice(fieldNodes).isLeft)
                          Expr.splice(fieldNodes).asInstanceOf[Either[ConstructError, A]]
                        else
                          Expr.splice(
                            ValDefs
                              .createVal[java.util.HashMap[String, Node]](
                                Expr.quote(
                                  Expr
                                    .splice(fieldNodes)
                                    .asInstanceOf[Right[ConstructError, java.util.HashMap[String, Node]]]
                                    .value
                                ),
                                FreshName.FromPrefix("fields")
                              )
                              .use(construct)
                          )
                      }
                    }
                )
              }
          }
      }
    }

    /** `value.asInstanceOf[F]` (a method of its own so that `F` is a type parameter in the quote). */
    private def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])
  }
}
