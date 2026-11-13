package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonWriter
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, transientField}

trait EncoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) =>
            // Note: caching is handled by deriveEncoderRecursively
            encodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    /** Encode only the key-value field pairs (no writeObjectStart/writeObjectEnd). */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFieldsOnly[A: EncoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      // (plain classes and all parameter lists: values read through the case fields or the val/var accessors)
      val allFields = caseClass.fieldValuesAt(ectx.value).toList
      val instanceValues: Map[String, Expr_??] = allFields.toMap

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      val paramsByName: Map[String, Parameter] =
        if (allFields.isEmpty && SingletonValue.unapply(Type[A]).isDefined) Map.empty
        else caseClass.params.toMap

      // Validate: @transientField on fields without defaults is a compile error
      paramsByName.collectFirst {
        case (name, param) if isTransientParam[A](param, ectx.evaluatedConfig) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None if caseClass.params.exists { case (name, p) =>
              !isTransientParam[A](p, ectx.evaluatedConfig) && !allFields.exists(_._1 == name)
            } =>
          // as jsoniter-scala: the decoder needs the parameter, but the encoder cannot read it
          val name = caseClass.params.collectFirst {
            case (name, p) if !isTransientParam[A](p, ectx.evaluatedConfig) && !allFields.exists(_._1 == name) => name
          }.get
          val err = CodecDerivationError.ParameterWithoutAccessor(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !isTransientParam[A](p, ectx.evaluatedConfig))
          }

          // Duplicated keys (when known at compile time) fail the derivation, as in jsoniter-scala
          val knownKeys: List[Option[String]] = nonTransientFields.map { case (name, _) =>
            paramsByName.get(name).flatMap(fieldNameOverride).orElse(ectx.evaluatedConfig.map(_.fieldNameMapper(name)))
          }
          val collisionCheck: MIO[Unit] =
            if (knownKeys.forall(_.isDefined)) checkFieldNameCollisions[A](knownKeys.flatten) else MIO.pure(())

          collisionCheck >> (NonEmptyList.fromList(nonTransientFields) match {
            case Some(nonEmptyFields) =>
              nonEmptyFields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(fieldNameOverride)

                      // Pre-compute mapped field name when config is statically known
                      val mappedFieldName: Option[String] = nameOverride.orElse(
                        ectx.evaluatedConfig.map(_.fieldNameMapper(fName))
                      )

                      val unconditionalWrite: Expr[Unit] = mappedFieldName match {
                        case Some(resolvedName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeFieldName(Expr.splice(Expr(resolvedName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeFieldName(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }

                      // Skip all transient checks when config is known and all flags are false
                      val noTransientChecksNeeded = ectx.evaluatedConfig.exists { c =>
                        !c.transientDefault && !c.transientNone && !c.transientEmpty && !c.transientNull
                      }
                      // `scala.transient` with a runtime config: skipped when `scalaTransientSupport` is on
                      val runtimeTransient: Option[Expr[Boolean]] =
                        paramsByName
                          .get(fName)
                          .filter(p => isTransientAtRuntimeOnly[A](p, ectx.evaluatedConfig))
                          .map(_ => Expr.quote(Expr.splice(ectx.config).scalaTransientSupport))

                      if (noTransientChecksNeeded && runtimeTransient.isEmpty) {
                        unconditionalWrite
                      } else {
                        // Determine transient skip conditions
                        val param = paramsByName.get(fName)
                        // Defaults of non-first parameter lists may depend on the preceding parameters: they are
                        // computed from the values of the encoded instance (as jsoniter-scala does).
                        val defaultAsAnyOpt: Option[Expr[Any]] =
                          if (noTransientChecksNeeded) None
                          else
                            param.flatMap { p =>
                              parameterDefault(p, instanceValues).map { ee =>
                                import ee.Underlying; ee.value.upcast[Any]
                              }
                            }

                        val isOptionField = Type[Field] match {
                          case IsOption(_) => true
                          case _           => false
                        }
                        // As in jsoniter-scala, `transientEmpty` applies to collections, maps and arrays (also
                        // `IArray`), but not to `String`s.
                        val asIterable: Option[Expr[Iterable[Any]]] =
                          if (isOptionField || Type[Field] =:= CTypes.String) None
                          else
                            Type[Field] match {
                              case IsCollection(isCollection) =>
                                import isCollection.Underlying as Item
                                // Iterable is covariant
                                Some(isCollection.value.asIterable(fieldExpr).asInstanceOf[Expr[Iterable[Any]]])
                              case _ => None
                            }
                        val isArrayLike = asIterable.isDefined && !(Type[Field] <:< Type.of[Iterable[Any]])

                        val conditions = List.newBuilder[Expr[Boolean]]
                        runtimeTransient.foreach(conditions += _)

                        defaultAsAnyOpt.foreach { defaultExpr =>
                          conditions +=
                            (if (isArrayLike) Expr.quote {
                               Expr.splice(ectx.config).transientDefault &&
                               hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils
                                 .valueEquals(Expr.splice(fieldExpr).asInstanceOf[Any], Expr.splice(defaultExpr))
                             }
                             else
                               Expr.quote {
                                 Expr.splice(ectx.config).transientDefault &&
                                 Expr.splice(fieldExpr).asInstanceOf[Any] == Expr.splice(defaultExpr)
                               })
                        }

                        // As in jsoniter-scala: `transientNull` skips nullable (Scala 3 `T | Null`) fields holding null.
                        if (!noTransientChecksNeeded && nullableUnionUnderlying[Field].isDefined) {
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientNull &&
                            (Expr.splice(fieldExpr).asInstanceOf[AnyRef] eq null)
                          }
                        }

                        if (!noTransientChecksNeeded && isOptionField) {
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientNone &&
                            !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
                          }
                        }

                        if (!noTransientChecksNeeded) asIterable.foreach { iterable =>
                          conditions += Expr.quote {
                            Expr.splice(ectx.config).transientEmpty && Expr.splice(iterable).isEmpty
                          }
                        }

                        val condList = conditions.result()
                        if (condList.isEmpty) {
                          unconditionalWrite
                        } else {
                          val skipExpr = condList.reduce { (a, b) =>
                            Expr.quote(Expr.splice(a) || Expr.splice(b))
                          }
                          Expr.quote {
                            if (!Expr.splice(skipExpr)) {
                              Expr.splice(unconditionalWrite)
                            }
                          }
                        }
                      }
                    }
                  }
                }
                .map { fieldExprs =>
                  fieldExprs.toList
                    .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, field) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(field)
                      }
                    }
                }

            case None =>
              MIO.pure(Expr.quote(()): Expr[Unit])
          })
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFields[A: EncoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[Unit]] =
      encodeCaseClassFieldsOnly(caseClass).map { fieldsExpr =>
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(fieldsExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
  }

}
