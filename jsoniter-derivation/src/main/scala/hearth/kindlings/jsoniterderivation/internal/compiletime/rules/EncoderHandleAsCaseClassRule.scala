package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Build transient skip conditions for a single encoder field. Returns empty list when no conditions apply. */
  @scala.annotation.nowarn("msg=is never used")
  private def buildFieldSkipConditions[Field: Type](
      fieldExpr: Expr[Field],
      param: Option[Parameter],
      config: Expr[hearth.kindlings.jsoniterderivation.JsoniterConfig],
      instanceValues: Map[String, Expr_??]
  )(implicit AnyT: Type[Any]): List[Expr[Boolean]] = {
    // Defaults of non-first parameter lists may depend on the preceding parameters: they are computed from the values of
    // the encoded instance (as jsoniter-scala does).
    val defaultAsAnyOpt: Option[Expr[Any]] = param.flatMap { p =>
      parameterDefault(p, instanceValues).map { ee => import ee.Underlying; ee.value.upcast[Any] }
    }

    val isOptionField = Type[Field] match {
      case IsOption(_) => true
      case _           => false
    }
    // As in jsoniter-scala, `transientEmpty` applies to collections, maps and arrays (also `IArray` and integration
    // collections), but not to `String`s (an empty string is written as `""`).
    val asIterable: Option[Expr[Iterable[Any]]] =
      if (isOptionField || Type[Field] =:= CTypes.String) None
      else
        Type[Field] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            Some(isCollection.value.asIterable(fieldExpr).asInstanceOf[Expr[Iterable[Any]]]) // Iterable is covariant
          case _ => None
        }
    // Arrays (and `IArray`s) are compared by contents with their default.
    val isArrayLike = asIterable.isDefined && !(Type[Field] <:< Type.of[Iterable[Any]])

    val conditions = List.newBuilder[Expr[Boolean]]

    defaultAsAnyOpt.foreach { defaultExpr =>
      conditions +=
        (if (isArrayLike) Expr.quote {
           Expr.splice(config).transientDefault &&
           hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
             .valueEquals(Expr.splice(fieldExpr).asInstanceOf[Any], Expr.splice(defaultExpr))
         }
         else
           Expr.quote {
             Expr.splice(config).transientDefault &&
             Expr.splice(fieldExpr).asInstanceOf[Any] == Expr.splice(defaultExpr)
           })
    }

    if (isOptionField) {
      conditions += Expr.quote {
        Expr.splice(config).transientNone &&
        !Expr.splice(fieldExpr).asInstanceOf[Option[Any]].isDefined
      }
    }

    if (Type[Field] <:< Type.of[Iterator[Any]]) {
      // `asIterable` would consume the iterator: check `hasNext` instead (as jsoniter-scala does)
      conditions += Expr.quote {
        Expr.splice(config).transientEmpty && !Expr.splice(fieldExpr).asInstanceOf[Iterator[Any]].hasNext
      }
    } else
      asIterable.foreach { iterable =>
        conditions += Expr.quote {
          Expr.splice(config).transientEmpty && Expr.splice(iterable).isEmpty
        }
      }

    // As in jsoniter-scala: `transientNull` skips fields of nullable types (Scala 3 `T | Null`) holding `null`.
    if (nullableUnionUnderlying[Field].isDefined) {
      conditions += Expr.quote {
        Expr.splice(config).transientNull && (Expr.splice(fieldExpr).asInstanceOf[AnyRef] eq null)
      }
    }

    conditions.result()
  }

  /** Wrap an unconditional write with skip conditions, or return it as-is if no conditions apply. */
  @scala.annotation.nowarn("msg=is never used")
  private def applySkipConditions(
      unconditionalWrite: Expr[Unit],
      condList: List[Expr[Boolean]]
  ): Expr[Unit] = {
    implicit val boolT: Type[Boolean] = CTypes.Boolean
    implicit val unitT: Type[Unit] = CTypes.Unit
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

  @scala.annotation.nowarn("msg=is never used")
  private def inlineBuiltInEncode[Field: Type](value: Expr[Field], writer: Expr[JsonWriter])(implicit
      JsonWriterT: Type[JsonWriter],
      UnitT: Type[Unit]
  ): Option[Expr[Unit]] =
    if (literalKey[Field].isDefined || nullableUnionUnderlying[Field].isDefined)
      None // nullable unions and literal types are written by their own rule
    else if (Type[Field] =:= Type.of[Int])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Int])))
    else if (Type[Field] =:= Type.of[Long])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Long])))
    else if (Type[Field] =:= Type.of[Double])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Double])))
    else if (Type[Field] =:= Type.of[Float])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Float])))
    else if (Type[Field] =:= Type.of[Boolean])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Boolean])))
    else if (Type[Field] =:= Type.of[String])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[String])))
    else if (Type[Field] =:= Type.of[Byte])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Byte])))
    else if (Type[Field] =:= Type.of[Short])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Short])))
    else if (Type[Field] =:= Type.of[Char])
      Some(Expr.quote(Expr.splice(writer).writeVal(Expr.splice(value).asInstanceOf[Char].toString)))
    else
      None

  /** `alwaysEmitDiscriminator`: the codec of an ADT leaf (not derived through its sealed parent) writes the
    * discriminator too, as in jsoniter-scala. Sealed trait codecs write fields only, so this is never duplicated.
    */
  @scala.annotation.nowarn("msg=is never used")
  private[compiletime] def writeAlwaysEmittedDiscriminator[A: EncoderCtx]: Expr[Unit] = {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val StringT: Type[String] = CTypes.String
    alwaysEmittedDiscriminatorKey[A](ectx.config, ectx.evaluatedConfig) match {
      case None      => Expr.quote(())
      case Some(key) =>
        val leafName: Expr[String] = namedAnnotationOf[A] match {
          case Some(name) => Expr(name)
          case None => Expr.quote(Expr.splice(ectx.config).adtLeafClassNameMapper(Expr.splice(Expr(adtLeafName[A]))))
        }
        Expr.quote {
          val discriminatorKey = Expr.splice(key)
          if (discriminatorKey ne null) {
            Expr.splice(ectx.writer).writeKey(discriminatorKey)
            Expr.splice(ectx.writer).writeVal(Expr.splice(leafName))
          }
        }
    }
  }

  object EncoderHandleAsCaseClassRule extends EncoderDerivationRule("handle as case class when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) =>
            // Note: caching is handled by deriveEncoderRecursively — do NOT call setHelper here
            // to avoid conflicts with the forward-declaration in deriveEncoderRecursively.
            encodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    /** Encode only the key-value field pairs (no writeObjectStart/writeObjectEnd). */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFieldsOnly[A: EncoderCtx](
        caseClass: ProductType[A],
        // encoded by its sealed parent's codec, which writes the discriminator next to the fields
        viaSealedParent: Boolean = false
    ): MIO[Expr[Unit]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val AnyT: Type[Any] = CTypes.Any
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val allFields = caseClass.fieldValuesAt(ectx.value).toList
      val instanceValues: Map[String, Expr_??] = allFields.toMap

      // Singletons (case objects, parameterless enum cases) have no primary constructor.
      // Only access primaryConstructor when there are actual fields to process.
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
        case None if paramsByName.exists { case (name, p) =>
              !isTransientParam[A](p, ectx.evaluatedConfig) && !allFields.exists(_._1 == name)
            } =>
          // jsoniter-scala rejects these too: the decoder needs the parameter, but the encoder cannot read it.
          val name = caseClass.params.collectFirst {
            case (name, p) if !isTransientParam[A](p, ectx.evaluatedConfig) && !allFields.exists(_._1 == name) => name
          }.get
          val err = CodecDerivationError.ParameterWithoutAccessor(name, Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case None =>
          val nonTransientFields = allFields.filter { case (name, _) =>
            paramsByName.get(name).forall(p => !isTransientParam[A](p, ectx.evaluatedConfig))
          }

          val collisionCheck: MIO[Unit] = ectx.evaluatedConfig match {
            case Some(config) =>
              val keys = nonTransientFields.map { case (name, _) =>
                paramsByName
                  .get(name)
                  .flatMap(p => fieldNameOverride(p))
                  .getOrElse(config.fieldNameMapper(name))
              }
              // The discriminator is a key of this object only when it is written: through the sealed parent, or by
              // the leaf's own codec with alwaysEmitDiscriminator (as in jsoniter-scala).
              val discriminator =
                if (viaSealedParent || (config.alwaysEmitDiscriminator && hasSealedParent[A]))
                  config.discriminatorFieldName.toList
                else Nil
              checkFieldNameCollisions[A](keys ++ discriminator)
            case None => MIO.pure(())
          }

          collisionCheck >> (NonEmptyList.fromList(nonTransientFields) match {
            case Some(nonEmptyFields) =>
              nonEmptyFields
                .parTraverse { case (fName, fieldValue) =>
                  import fieldValue.{Underlying as Field, value as fieldExpr}
                  Log.namedScope(s"Encoding field ${ectx.value.prettyPrint}.$fName: ${Type[Field].prettyPrint}") {
                    val hasStringifiedAnnotation =
                      paramsByName.get(fName).exists(p => hasAnnotationType[stringified](p))
                    // `@stringified` stringifies the numbers/booleans of the field's value (also inside options,
                    // collections, map values and tuples); otherwise the config's isStringified decides. The flag never
                    // leaks into the fields of nested case classes.
                    val fieldCtx = ectx.nest(fieldExpr).copy(stringified = Some(hasStringifiedAnnotation))
                    val isStringifiedStaticallyFalse = fieldCtx.stringifiedMode.contains(false)
                    val inlined: Option[Expr[Unit]] =
                      if (!isStringifiedStaticallyFalse) None
                      else
                        inlineBuiltInEncode[Field](fieldExpr, ectx.writer).flatMap { _ =>
                          summonJsonValueCodecCached[Field] match {
                            case Right(_) => None
                            case Left(_)  => inlineBuiltInEncode[Field](fieldExpr, ectx.writer)
                          }
                        }
                    val fieldEncMIO: MIO[Expr[Unit]] = inlined match {
                      case Some(enc) => MIO.pure(enc)
                      case None      => deriveEncoderRecursively[Field](using fieldCtx)
                    }
                    fieldEncMIO.map { fieldEnc =>
                      val nameOverride =
                        paramsByName.get(fName).flatMap(p => fieldNameOverride(p))

                      val mappedFieldName: Option[String] = nameOverride.orElse(
                        ectx.evaluatedConfig.map(_.fieldNameMapper(fName))
                      )

                      val isAsciiSafe =
                        mappedFieldName.exists(_.forall(c => c >= 0x20 && c < 0x7f && c != '"' && c != '\\'))

                      val unconditionalWrite: Expr[Unit] = mappedFieldName match {
                        case Some(resolvedName) if isAsciiSafe =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeNonEscapedAsciiKey(Expr.splice(Expr(resolvedName)))
                            Expr.splice(fieldEnc)
                          }
                        case Some(resolvedName) =>
                          Expr.quote {
                            Expr.splice(ectx.writer).writeKey(Expr.splice(Expr(resolvedName)))
                            Expr.splice(fieldEnc)
                          }
                        case None =>
                          Expr.quote {
                            Expr
                              .splice(ectx.writer)
                              .writeKey(Expr.splice(ectx.config).fieldNameMapper(Expr.splice(Expr(fName))))
                            Expr.splice(fieldEnc)
                          }
                      }
                      val noTransientChecksNeeded = ectx.evaluatedConfig.exists { c =>
                        !c.transientDefault && !c.transientNone && !c.transientEmpty && !c.transientNull
                      }
                      val condList =
                        if (noTransientChecksNeeded) Nil
                        else
                          buildFieldSkipConditions[Field](
                            fieldExpr,
                            paramsByName.get(fName),
                            ectx.config,
                            instanceValues
                          )
                      // `scala.transient` with a runtime config: skipped when `scalaTransientSupport` is on
                      val runtimeTransient: List[Expr[Boolean]] =
                        paramsByName
                          .get(fName)
                          .filter(p => isTransientAtRuntimeOnly[A](p, ectx.evaluatedConfig)) match {
                          case Some(_) => List(Expr.quote(Expr.splice(ectx.config).scalaTransientSupport))
                          case None    => Nil
                        }
                      applySkipConditions(unconditionalWrite, runtimeTransient ++ condList)
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

    /** Encode a full JSON object: writeObjectStart + fields + writeObjectEnd. */
    @scala.annotation.nowarn("msg=is never used")
    private[compiletime] def encodeCaseClassFields[A: EncoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[Unit]] =
      encodeCaseClassFieldsOnly(caseClass).map { fieldsExpr =>
        val discriminator = writeAlwaysEmittedDiscriminator[A]
        Expr.quote {
          Expr.splice(ectx.writer).writeObjectStart()
          Expr.splice(discriminator)
          Expr.splice(fieldsExpr)
          Expr.splice(ectx.writer).writeObjectEnd()
        }
      }
  }

}
