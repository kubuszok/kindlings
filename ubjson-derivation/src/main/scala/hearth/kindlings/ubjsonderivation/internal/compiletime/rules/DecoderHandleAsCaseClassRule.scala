package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader}
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, transientField}
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait DecoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Defaults of the transient fields for the constructor. Defaults of non-first parameter lists may depend on the
    * preceding parameters (`nonTransientValues` and the preceding transient defaults).
    */
  private def buildTransientDefaults[A: DecoderCtx](
      fieldsList: List[(String, Parameter)],
      nonTransientValues: Map[String, Expr_??]
  ): Map[String, Expr_??] =
    fieldsList.foldLeft(Map.empty[String, Expr_??]) { case (acc, (fName, param)) =>
      if (!isTransientParam[A](param, dctx.evaluatedConfig)) acc
      else parameterDefault(param, nonTransientValues ++ acc).fold(acc)(expr => acc + (fName -> expr))
    }

  /** Fill expressions of the absent fields (in parameter order, so defaults may depend on preceding parameters). */
  private def buildAbsentFieldFillExprs[A: DecoderCtx](
      nonTransientWithIndex: List[((String, Parameter), Int)],
      decodedValuesExpr: Expr[Array[Any]],
      argumentValues: Map[String, Expr_??]
  ): List[Expr[Unit]] =
    absentFieldFillExprs(
      nonTransientWithIndex.map { case ((fName, param), idx) => (fName, param, idx, fieldNameOverride(param)) },
      decodedValuesExpr,
      dctx.config,
      dctx.reader,
      argumentValues,
      name =>
        nonTransientWithIndex.exists { case ((n, p), _) =>
          n == name && isTransientAtRuntimeOnly[A](p, dctx.evaluatedConfig)
        },
      dctx.evaluatedConfig
    )

  /** A field's decode function, which with a runtime config skips a `scala.transient` field when
    * `scalaTransientSupport` is on (its slot stays absent: it gets its default).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def erasedFieldDecodeFn[A: DecoderCtx, Field: Type](
      param: Parameter,
      decodeFn: Expr[UBJsonReader] => Expr[Field]
  ): Expr[UBJsonReader => Any] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val AnyT: Type[Any] = CTypes.Any
    if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) Expr.quote { (r: UBJsonReader) =>
      UBJsonDerivationUtils.readUnlessTransient(
        r,
        Expr.splice(dctx.config).scalaTransientSupport,
        (r2: UBJsonReader) => Expr.splice(decodeFn(Expr.quote(r2))).asInstanceOf[Any]
      )
    }
    else Expr.quote((r: UBJsonReader) => Expr.splice(decodeFn(Expr.quote(r))).asInstanceOf[Any])
  }

  /** Decodes a case class without non-transient fields (zero-param or all-`@transientField`): constructs it from the
    * transient defaults and reads the object, skipping unknown keys unless `skipUnexpectedFields = false` (as
    * jsoniter-scala does). When `alreadyOpened`, a sealed trait decoder consumed the object start already.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)],
      alreadyOpened: Boolean
  ): MIO[Expr[A]] = {
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    val constructedMIO: MIO[Expr[A]] =
      foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
        onTypes = _ => Map.empty,
        onValues = _ => buildTransientDefaults[A](fieldsList, Map.empty)
      ) match {
        case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
        case Left(error)          =>
          val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
          Log.error(err.message) >> MIO.fail(err)
      }
    constructedMIO.map { constructed =>
      val skipUnexpected: Expr[Boolean] = dctx.evaluatedConfig match {
        case Some(c) => Expr(c.skipUnexpectedFields)
        case None    => Expr.quote(Expr.splice(dctx.config).skipUnexpectedFields)
      }
      Expr.quote {
        UBJsonDerivationUtils.readObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpected),
          Expr.splice(Expr(alreadyOpened))
        )
        Expr.splice(constructed)
      }
    }
  }

  /** Fold a list of Expr[Unit] into a single sequenced Expr[Unit]. */
  @scala.annotation.nowarn("msg=is never used")
  private def foldExprUnits(exprs: List[Expr[Unit]]): Expr[Unit] = {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    exprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
      Expr.quote {
        Expr.splice(acc)
        Expr.splice(step)
      }
    }
  }

  /** [[UBJsonDerivationUtils.FieldIndex]] of the keys, a `lazy val` of the codec instance (built once per type). */
  @scala.annotation.nowarn("msg=is never used")
  private def cachedFieldIndex[A: DecoderCtx](
      names: List[Expr[String]]
  ): MIO[Expr[UBJsonDerivationUtils.FieldIndex]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val ArrayStringT: Type[Array[String]] = CTypes.ArrayString
    implicit val FieldIndexT: Type[UBJsonDerivationUtils.FieldIndex] = CTypes.FieldIndex
    val cacheKey = s"ubjson-fieldIndex:${Type[A].plainPrint}"
    val builder = ValDefBuilder.ofLazy[UBJsonDerivationUtils.FieldIndex](s"fieldIndex_${Type[A].shortName}")
    def setKeys(keys: Expr[Array[String]]): Expr[Unit] =
      foldExprUnits(names.zipWithIndex.map { case (name, i) =>
        Expr.quote(Expr.splice(keys)(Expr.splice(Expr(i))) = Expr.splice(name))
      })
    val keysExpr: Expr[Array[String]] = Expr.quote {
      val keys = new Array[String](Expr.splice(Expr(names.size)))
      Expr.splice(setKeys(Expr.quote(keys)))
      keys
    }
    for {
      cacheState <- dctx.cache.get
      _ <-
        if (builder.isBuilt(cacheState, cacheKey)) MIO.pure(())
        else
          dctx.cache.buildCachedWith(cacheKey, builder)(_ =>
            Expr.quote(UBJsonDerivationUtils.fieldIndex(Expr.splice(keysExpr)))
          )
      value <- dctx.cache.get0Ary[UBJsonDerivationUtils.FieldIndex](cacheKey)
    } yield value.get
  }

  /** The parts of a case class decoder finding keys in a per-instance index (see [[MaxKeysComparedInDecoder]]): the
    * field values are collected into an `Array[Any]` (absent slots hold `UBJsonDerivationUtils.AbsentField`) and keys
    * are matched by their runtime names.
    *
    * @param fieldIndex
    *   the keys of the fields, a `lazy val` of the codec instance (`fieldNameMapper` is called once per field per
    *   instance, not per key per decode)
    * @param fieldCount
    *   the size of the field array
    * @param construct
    *   fills the absent fields (in parameter order: defaults may depend on preceding parameters) and constructs the
    *   value
    * @param readKnownField
    *   `(index of a known key, field array, reader) => statement` reading the field's value into the array
    */
  final protected case class ArrayFieldsDecoding[A](
      fieldIndex: Expr[UBJsonDerivationUtils.FieldIndex],
      fieldCount: Int,
      construct: Expr[Array[Any]] => Expr[A],
      readKnownField: (Expr[Int], Expr[Array[Any]], Expr[UBJsonReader]) => Expr[Unit]
  )

  /** Builds [[ArrayFieldsDecoding]]. The generated code stays linear in the number of fields: each field is read and
    * each absent field filled by its own local `def`, a known key reaches its field through a binary search on its
    * index (no chain of name comparisons, no `fieldNameMapper` call per key), and the values are cast to the field
    * types.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def arrayFieldsDecoding[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)],
      fields: NonEmptyList[(String, Parameter)]
  ): MIO[ArrayFieldsDecoding[A]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val IntT: Type[Int] = CTypes.Int
    implicit val AnyT: Type[Any] = CTypes.Any
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

    val indexed = fields.toList.zipWithIndex
    val fieldCount = indexed.size

    indexed
      .parTraverse { case ((fName, param), index) =>
        import param.tpe.Underlying as Field
        Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
          deriveFieldDecoder[Field].map { rawDecode =>
            val decode = withFieldDefault[Field](param, dctx.config, dctx.evaluatedConfig)(rawDecode)
            val readField: Expr[UBJsonReader] => Expr[Any] =
              if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) { (reader: Expr[UBJsonReader]) =>
                Expr.quote {
                  UBJsonDerivationUtils.readUnlessTransient(
                    Expr.splice(reader),
                    Expr.splice(dctx.config).scalaTransientSupport,
                    (r: UBJsonReader) => Expr.splice(decode(Expr.quote(r))).asInstanceOf[Any]
                  )
                }
              } else (reader: Expr[UBJsonReader]) => Expr.quote(Expr.splice(decode(reader)).asInstanceOf[Any])
            (fName, param, index, readField)
          }
        }
      }
      .flatMap { fieldData =>
        val fieldDataList = fieldData.toList

        // The keys, computed once per codec instance.
        val names: List[Expr[String]] = fieldDataList.map { case (fName, param, _, _) =>
          fieldNameOverride(param) match {
            case Some(customName) => Expr(customName)
            case None             =>
              dctx.evaluatedConfig match {
                case Some(c) => Expr(c.fieldNameMapper(fName))
                case None    => Expr.quote(Expr.splice(dctx.instanceConfig).fieldNameMapper(Expr.splice(Expr(fName))))
              }
          }
        }

        // Fills the absent fields (in parameter order: defaults may depend on preceding parameters) of the field array
        // and constructs the value.
        def construct(decodedValues: Expr[Array[Any]]): Either[String, Expr[A]] = {
          val nonTransientValues: Map[String, Expr_??] = fieldDataList.map { case (fName, param, index, _) =>
            import param.tpe.Underlying as Field
            fName -> mkFieldAccessExpr[Field](decodedValues, index).as_??
          }.toMap
          val fieldMap = nonTransientValues ++ buildTransientDefaults[A](fieldsList, nonTransientValues)
          val fills: List[ValDefs[Expr[Unit]]] = fieldDataList.map { case (fName, param, index, _) =>
            import param.tpe.Underlying as Field
            val fallback: Expr[Field] = absentFieldFallbackOf[Field](
              fName,
              param,
              fieldNameOverride(param),
              dctx.config,
              dctx.reader,
              fieldMap,
              isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig),
              dctx.evaluatedConfig
            )
            ValDefs
              .createDef[Any](Expr.quote(Expr.splice(fallback).asInstanceOf[Any]), FreshName.FromPrefix("absent"))
              .map { fill =>
                Expr.quote {
                  if (UBJsonDerivationUtils.isAbsentField(Expr.splice(decodedValues)(Expr.splice(Expr(index)))))
                    Expr.splice(decodedValues)(Expr.splice(Expr(index))) = Expr.splice(fill)
                }
              }
          }
          foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
            onTypes = _ => Map.empty,
            onValues = _ => fieldMap
          ).map { constructExpr =>
            fills.sequence.use { fillAll =>
              Expr.quote {
                Expr.splice(foldExprUnits(fillAll))
                Expr.splice(constructExpr.value.asInstanceOf[Expr[A]])
              }
            }
          }
        }

        val readKnownField: (Expr[Int], Expr[Array[Any]], Expr[UBJsonReader]) => Expr[Unit] =
          (indexExpr, arr, reader) =>
            fieldDataList
              .map { case (_, _, index, readField) =>
                ValDefs
                  .createDef[Any](readField(reader), FreshName.FromPrefix("readField"))
                  .map(read => Expr.quote(Expr.splice(arr)(Expr.splice(Expr(index))) = Expr.splice(read)))
              }
              .sequence
              .use { reads =>
                // a binary search on the index: the field indices are 0 until fieldCount
                def search(from: Int, until: Int, statements: Vector[Expr[Unit]]): Expr[Unit] =
                  if (until - from == 1) statements(from)
                  else {
                    val middle = (from + until) / 2
                    Expr.quote {
                      if (Expr.splice(indexExpr) < Expr.splice(Expr(middle)))
                        Expr.splice(search(from, middle, statements))
                      else Expr.splice(search(middle, until, statements))
                    }
                  }
                search(0, fieldCount, reads.toVector)
              }

        construct(Expr.quote(null.asInstanceOf[Array[Any]])) match {
          case Left(error) =>
            val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
            Log.error(err.message) >> MIO.fail(err)
          case Right(_) =>
            cachedFieldIndex[A](names).map { fieldIndex =>
              ArrayFieldsDecoding(fieldIndex, fieldCount, arr => construct(arr).toOption.get, readKnownField)
            }
        }
      }
  }

  /** The most keys a decoder compares one by one (with a config known at compile time): wider classes, and every class
    * when the config is only known at runtime, find keys in a per-instance [[UBJsonDerivationUtils.FieldIndex]].
    */
  private val MaxKeysComparedInDecoder = 8

  /** A case class decoder finding keys in a per-instance index (see [[MaxKeysComparedInDecoder]]): reads the object
    * itself (no dispatch function allocated per decode) and constructs the value by a local def.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeCaseClassFieldsIndexed[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)],
      fields: NonEmptyList[(String, Parameter)]
  ): MIO[Expr[A]] =
    arrayFieldsDecoding[A](caseClass, fieldsList, fields).map { decoding =>
      implicit val StringT: Type[String] = CTypes.String
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      val fieldCount = decoding.fieldCount
      Expr.quote {
        val reader = Expr.splice(dctx.reader)
        val fieldIndex = Expr.splice(decoding.fieldIndex) // once per decode, not per key
        val decodedValues = UBJsonDerivationUtils.absentFields(Expr.splice(Expr(fieldCount)))
        val _seen: Array[Boolean] =
          if (Expr.splice(dctx.config).checkFieldDuplication) new Array[Boolean](Expr.splice(Expr(fieldCount)))
          else null
        reader.readObjectStart()
        while (!reader.isObjectEnd()) {
          val fieldName = reader.readFieldName()
          val index = fieldIndex.indexOf(fieldName)
          if (index >= 0) {
            if (_seen != null) {
              if (_seen(index)) UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
              _seen(index) = true
            }
            Expr.splice(decoding.readKnownField(Expr.quote(index), Expr.quote(decodedValues), Expr.quote(reader)))
          } else if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
          else UBJsonDerivationUtils.unexpectedField(reader, fieldName)
        }
        Expr.splice(
          ValDefs
            .createDef[A](decoding.construct(Expr.quote(decodedValues)), FreshName.FromPrefix("construct"))
            .use(constructed => constructed)
        )
      }
    }

  /** [[decodeCaseClassFieldsIndexed]] for an already-opened object (discriminator mode). */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeCaseClassFieldsInlineIndexed[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)],
      fields: NonEmptyList[(String, Parameter)]
  ): MIO[Expr[A]] =
    arrayFieldsDecoding[A](caseClass, fieldsList, fields).map { decoding =>
      implicit val StringT: Type[String] = CTypes.String
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      val fieldCount = decoding.fieldCount
      Expr.quote {
        val fieldIndex = Expr.splice(decoding.fieldIndex) // once per decode, not per key
        UBJsonDerivationUtils.readObjectInline[A](
          Expr.splice(dctx.reader),
          Expr.splice(Expr(fieldCount)),
          (decodedValues: Array[Any]) => Expr.splice(decoding.construct(Expr.quote(decodedValues)))
        ) { case (fieldName, arr, reader) =>
          val index = fieldIndex.indexOf(fieldName)
          if (index >= 0) Expr.splice(decoding.readKnownField(Expr.quote(index), Expr.quote(arr), Expr.quote(reader)))
          else if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
          else UBJsonDerivationUtils.unexpectedField(reader, fieldName)
        }
      }
    }

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) =>
            // Note: caching is handled by deriveDecoderRecursively
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFields[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      // Validate: @transientField on fields without defaults
      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      // the config, when known at compile time and the keys are compared one by one (see MaxKeysComparedInDecoder)
      val comparedKeysConfig = dctx.evaluatedConfig.filter(_ => nonTransientFields.size <= MaxKeysComparedInDecoder)
      NonEmptyList.fromList(nonTransientFields).map(_ -> comparedKeysConfig) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList, alreadyOpened = false)

        case Some((fields, None)) =>
          decodeCaseClassFieldsIndexed[A](caseClass, fieldsList, fields)

        case Some((fields, Some(evConfig))) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { rawDecodeFn =>
                  val decodeFn = withFieldDefault[Field](param, dctx.config, dctx.evaluatedConfig)(rawDecodeFn)
                  val decodeFnErased: Expr[UBJsonReader => Any] = erasedFieldDecodeFn[A, Field](param, decodeFn)
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = mkFieldAccessExpr[Field](arrExpr, arrayIndex)
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  // Absent fields: default / None / empty collection, or `missing required field`
                  // (see DecoderAbsentFieldSupport). The transient* flags only affect encoding.
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults[A](fieldsList, nonTransientFieldMap)
                  val absentFieldFillAll: Expr[Unit] =
                    buildAbsentFieldFillExprs[A](nonTransientWithIndex, decodedValuesExpr, fieldMap)
                      .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                        Expr.quote {
                          Expr.splice(acc)
                          Expr.splice(step)
                        }
                      }
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(
                        Expr.quote {
                          Expr.splice(absentFieldFillAll)
                          Expr.splice(constructExpr.value.asInstanceOf[Expr[A]])
                        }
                      )
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Mapped field names (the config is known at compile time on this path)
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    val mappedName: String = nameOverride.getOrElse(evConfig.fieldNameMapper(name))
                    (name, index, decodeFnErased, mappedName)
                  }

                  val skipCheckFieldDuplication = !evConfig.checkFieldDuplication
                  val skipUnexpectedStatically = evConfig.skipUnexpectedFields

                  if (skipCheckFieldDuplication) {
                    // Optimized: no _seen array needed
                    Expr.quote {
                      UBJsonDerivationUtils.readObject[A](
                        Expr.splice(dctx.reader),
                        Expr.splice(Expr(fieldMappings.size)),
                        Expr.splice(constructLambda)
                      ) { case (fieldName, arr, reader) =>
                        Expr.splice {
                          val fallback: Expr[Unit] = skipUnexpectedStatically match {
                            case true =>
                              Expr.quote(reader.skip())
                            case false =>
                              Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                          }
                          fieldMappings.foldRight(fallback) {
                            case ((_, index, decodeFnErased, resolvedName), elseExpr) =>
                              Expr.quote {
                                if (fieldName == Expr.splice(Expr(resolvedName))) {
                                  arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                                } else Expr.splice(elseExpr)
                              }
                          }
                        }
                      }
                    }
                  } else {
                    // Full path with _seen array
                    Expr.quote {
                      val _seen: Array[Boolean] =
                        if (Expr.splice(dctx.config).checkFieldDuplication)
                          new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                        else null
                      UBJsonDerivationUtils.readObject[A](
                        Expr.splice(dctx.reader),
                        Expr.splice(Expr(fieldMappings.size)),
                        Expr.splice(constructLambda)
                      ) { case (fieldName, arr, reader) =>
                        Expr.splice {
                          val fallback: Expr[Unit] = skipUnexpectedStatically match {
                            case true =>
                              Expr.quote(reader.skip())
                            case false =>
                              Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                          }
                          fieldMappings.foldRight(fallback) {
                            case ((_, index, decodeFnErased, resolvedName), elseExpr) =>
                              Expr.quote {
                                if (fieldName == Expr.splice(Expr(resolvedName))) {
                                  if (_seen != null) {
                                    if (_seen(Expr.splice(Expr(index))))
                                      UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
                                    _seen(Expr.splice(Expr(index))) = true
                                  }
                                  arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                                } else Expr.splice(elseExpr)
                              }
                          }
                        }
                      }
                    }
                  }
                }
            }
      }
    }

    /** Decode fields from an already-opened object (for discriminator mode). */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      // the config, when known at compile time and the keys are compared one by one (see MaxKeysComparedInDecoder)
      val comparedKeysConfig = dctx.evaluatedConfig.filter(_ => nonTransientFields.size <= MaxKeysComparedInDecoder)
      NonEmptyList.fromList(nonTransientFields).map(_ -> comparedKeysConfig) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList, alreadyOpened = true)

        case Some((fields, None)) =>
          decodeCaseClassFieldsInlineIndexed[A](caseClass, fieldsList, fields)

        case Some((fields, Some(evConfig))) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { rawDecodeFn =>
                  val decodeFn = withFieldDefault[Field](param, dctx.config, dctx.evaluatedConfig)(rawDecodeFn)
                  val decodeFnErased: Expr[UBJsonReader => Any] = erasedFieldDecodeFn[A, Field](param, decodeFn)
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = mkFieldAccessExpr[Field](arrExpr, arrayIndex)
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults[A](fieldsList, nonTransientFieldMap)
                  val absentFieldFills: List[Expr[Unit]] =
                    buildAbsentFieldFillExprs[A](nonTransientWithIndex, decodedValuesExpr, fieldMap)
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(absentFieldFills.foldRight(constructExpr.value.asInstanceOf[Expr[A]]) { (fill, rest) =>
                        Expr.quote {
                          Expr.splice(fill)
                          Expr.splice(rest)
                        }
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Mapped field names (the config is known at compile time on this path)
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    val mappedName: String = nameOverride.getOrElse(evConfig.fieldNameMapper(name))
                    (name, index, decodeFnErased, mappedName)
                  }

                  val skipUnexpectedStatically = evConfig.skipUnexpectedFields

                  Expr.quote {
                    UBJsonDerivationUtils.readObjectInline[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        val fallback: Expr[Unit] = skipUnexpectedStatically match {
                          case true =>
                            Expr.quote(reader.skip())
                          case false =>
                            Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                        }
                        fieldMappings.foldRight(fallback) { case ((_, index, decodeFnErased, resolvedName), elseExpr) =>
                          Expr.quote {
                            if (fieldName == Expr.splice(Expr(resolvedName))) {
                              arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                            } else Expr.splice(elseExpr)
                          }
                        }
                      }
                    }
                  }
                }
            }
      }
    }
  }

  /** Helper method to create a typed field access expression from an Array[Any]. Extracted to avoid path-dependent type
    * issues on Scala 2 when `Field` comes from `import param.tpe.Underlying as Field` inside `Expr.quote`.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def mkFieldAccessExpr[F: Type](arrExpr: Expr[Array[Any]], index: Int): Expr[F] = {
    implicit val AnyT: Type[Any] = CTypes.Any
    implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
    Expr.quote {
      Expr.splice(arrExpr)(Expr.splice(Expr(index))).asInstanceOf[F]
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[UBJsonReader] => Expr[Field]] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig

    // `T | Null` fields get their own cache key (`String | Null` =:= `String` would share the decoder of `String`)
    val nullable = nullableUnionUnderlying[Field].isDefined
    val cacheKey = if (nullable) "nullable-field-decode-method" else "field-decode-method"
    // 1. Check if already cached via setHelper (case class/enum rules)
    (if (nullable) MIO.pure(None) else ctx.getHelper[Field]).flatMap {
      case Some(helperCall) =>
        Log.info(s"Found cached decoder helper for field ${Type[Field].prettyPrint}") >>
          MIO.pure((reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config))
      case None =>
        // 2. Check if cached via previous deriveFieldDecoder call
        ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field](cacheKey).flatMap {
          case Some(helperCall) =>
            Log.info(s"Found field decode cache for ${Type[Field].prettyPrint}") >>
              MIO.pure((reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config))
          case None =>
            // 3. Forward-declare, derive body via traverse, then build
            val defBuilder = ValDefBuilder.ofDef2[UBJsonReader, UBJsonConfig, Field](
              s"decode_field_${Type[Field].shortName}"
            )
            for {
              _ <- Log.info(s"Building decoder for ${Type[Field].prettyPrint} via cached def")
              _ <- ctx.cache.forwardDeclare(cacheKey, defBuilder)
              builtBuilder <- defBuilder.traverse { case (_, (reader, config)) =>
                val nestedCtx =
                  DecoderCtx
                    .from[Field](reader, config, ctx.cache, ctx.derivedType, ctx.evaluatedConfig, ctx.rootConfig)
                deriveDecoderRecursively[Field](using nestedCtx)
              }
              currentCache <- ctx.cache.get
              _ <- ctx.cache.set(builtBuilder.buildCached(currentCache, cacheKey))
              fn <- ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field](cacheKey)
            } yield {
              val helperCall = fn.get
              (reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config)
            }
        }
    }
  }

}
