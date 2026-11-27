package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  private def deriveZeroValue[A: Type]: Expr[A] =
    if (Type[A] <:< Type.of[AnyRef]) Expr.quote(null.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Boolean]) Expr.quote(false.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Byte]) Expr.quote(0.toByte.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Short]) Expr.quote(0.toShort.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Int]) Expr.quote(0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Long]) Expr.quote(0L.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Float]) Expr.quote(0.0f.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Double]) Expr.quote(0.0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Char]) Expr.quote(' '.asInstanceOf[A])
    else Expr.quote(null.asInstanceOf[A])

  @scala.annotation.nowarn("msg=is never used")
  private def inlineBuiltInDecode[Field: Type](readerExpr: Expr[JsonReader])(implicit
      JsonReaderT: Type[JsonReader]
  ): Option[Expr[Field]] =
    if (literalKey[Field].isDefined || nullableUnionUnderlying[Field].isDefined)
      None // nullable unions and literal types are checked by their own rule
    else if (Type[Field] =:= Type.of[Int]) Some(Expr.quote(Expr.splice(readerExpr).readInt().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Long]) Some(Expr.quote(Expr.splice(readerExpr).readLong().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Double]) Some(Expr.quote(Expr.splice(readerExpr).readDouble().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Float]) Some(Expr.quote(Expr.splice(readerExpr).readFloat().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Boolean])
      Some(Expr.quote(Expr.splice(readerExpr).readBoolean().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[String])
      Some(Expr.quote(Expr.splice(readerExpr).readString(null).asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Byte]) Some(Expr.quote(Expr.splice(readerExpr).readByte().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Short]) Some(Expr.quote(Expr.splice(readerExpr).readShort().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Char]) Some(Expr.quote(Expr.splice(readerExpr).readChar().asInstanceOf[Field]))
    else None

  /** Fold a list of Expr[Unit] into a single sequenced Expr[Unit]. */
  @scala.annotation.nowarn("msg=is never used")
  private def foldExprUnits(exprs: List[Expr[Unit]]): Expr[Unit] = {
    implicit val ut: Type[Unit] = CTypes.Unit
    exprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
      Expr.quote {
        Expr.splice(acc)
        Expr.splice(step)
      }
    }
  }

  /** Build map of transient field default values for the constructor. Defaults of non-first parameter lists may depend
    * on the preceding parameters (`nonTransientValues` and the preceding transient defaults).
    */
  private def buildTransientDefaults[A: DecoderCtx](
      fieldsList: List[(String, Parameter)],
      nonTransientValues: Map[String, Expr_??]
  ): Map[String, Expr_??] =
    fieldsList.foldLeft(Map.empty[String, Expr_??]) { case (acc, (fName, param)) =>
      if (!isTransientParam[A](param, dctx.evaluatedConfig)) acc
      else
        parameterDefault(param, nonTransientValues ++ acc).fold(acc)(expr => acc + (fName -> expr))
    }

  /** Constructs a case class without non-transient fields (zero-param or all-`@transientField`) from the transient
    * defaults.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def constructWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[A]] =
    foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
      onTypes = _ => Map.empty,
      onValues = _ => buildTransientDefaults(fieldsList, Map.empty)
    ) match {
      case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
      case Left(error)          =>
        val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
        Log.error(err.message) >> MIO.fail(err)
    }

  @scala.annotation.nowarn("msg=is never used")
  private def skipUnexpectedExpr[A: DecoderCtx]: Expr[Boolean] = {
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    dctx.evaluatedConfig match {
      case Some(c) => Expr(c.skipUnexpectedFields)
      case None    => Expr.quote(Expr.splice(dctx.config).skipUnexpectedFields)
    }
  }

  /** Decodes a case class without non-transient fields (zero-param or all-`@transientField`): constructs it from the
    * transient defaults and reads the object, skipping unknown keys unless `skipUnexpectedFields = false` (as
    * jsoniter-scala does).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[A]] = {
    implicit val StringT: Type[String] = CTypes.String
    constructWithoutFields[A](caseClass, fieldsList).map { constructed =>
      val allowedKey: Expr[String] =
        alwaysEmittedDiscriminatorKey[A](dctx.config, dctx.evaluatedConfig).getOrElse(Expr.quote(null: String))
      Expr.quote {
        JsoniterDerivationUtils.readObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpectedExpr[A]),
          Expr.splice(allowedKey),
          false
        )
        Expr.splice(constructed)
      }
    }
  }

  /** [[decodeObjectWithoutFields]] for an ADT leaf in discriminator mode (see
    * `JsoniterDerivationUtils.readInlineObjectWithoutFields`).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeInlineObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[Boolean] => Expr[A]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    constructWithoutFields[A](caseClass, fieldsList).map { constructed => (afterOpen: Expr[Boolean]) =>
      val checkDuplication: Expr[Boolean] = dctx.evaluatedConfig match {
        case Some(c) => Expr(c.checkFieldDuplication)
        case None    => Expr.quote(Expr.splice(dctx.config).checkFieldDuplication)
      }
      Expr.quote {
        JsoniterDerivationUtils.readInlineObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpectedExpr[A]),
          (Expr.splice(dctx.config).discriminatorFieldName match { case Some(name) => name; case _ => null }),
          Expr.splice(checkDuplication),
          Expr.splice(afterOpen)
        )
        Expr.splice(constructed)
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])

  @scala.annotation.nowarn("msg=is never used")
  private def ifThenElse[F: Type](condition: Expr[Boolean], ifTrue: Expr[F], ifFalse: Expr[F]): Expr[F] =
    Expr.quote(if (Expr.splice(condition)) Expr.splice(ifTrue) else Expr.splice(ifFalse))

  /** A `lazy val` of the codec instance (built once per key and type) holding `make(<array of the names>)`. */
  @scala.annotation.nowarn("msg=is never used")
  private def cachedKeysOf[A: DecoderCtx, K: Type](kind: String, names: List[Expr[String]])(
      make: Expr[Array[String]] => Expr[K]
  ): MIO[Expr[K]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val ArrayStringT: Type[Array[String]] = CTypes.ArrayString
    val cacheKey = s"jsoniter-$kind:${Type[A].plainPrint}"
    val builder = ValDefBuilder.ofLazy[K](s"${kind}_${Type[A].shortName}")
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
        else dctx.cache.buildCachedWith(cacheKey, builder)(_ => make(keysExpr))
      value <- dctx.cache.get0Ary[K](cacheKey)
    } yield value.get
  }

  /** [[JsoniterDerivationUtils.KeyTable]] of the keys (known at compile time), a `lazy val` of the codec instance. */
  @scala.annotation.nowarn("msg=is never used")
  private def cachedKeyTable[A: DecoderCtx](keys: List[String]): MIO[Expr[JsoniterDerivationUtils.KeyTable]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val KeyTableT: Type[JsoniterDerivationUtils.KeyTable] = CTypes.KeyTable
    cachedKeysOf[A, JsoniterDerivationUtils.KeyTable]("keyTable", keys.map(Expr(_)))(names =>
      Expr.quote(JsoniterDerivationUtils.keyTable(Expr.splice(names)))
    )
  }

  /** The parts of a case class decoder that collects the field values into an `Array[Any]` (absent slots hold
    * `JsoniterDerivationUtils.AbsentField`) and matches keys by their runtime names: the generic decoders, used when
    * the config is only known at runtime, and the discriminator-mode decoders of ADT leaves.
    *
    * @param fieldIndex
    *   the keys of the fields, a `lazy val` of the codec instance (the names are computed once, not per key)
    * @param fieldCount
    *   the size of the field array
    * @param construct
    *   fills the absent fields (in parameter order: defaults may depend on preceding parameters) and constructs the
    *   value
    * @param readKnownField
    *   `(index of a known key, field array, reader) => statement` reading the field's value into the array
    */
  final protected case class ArrayFieldsDecoding[A](
      fieldIndex: Expr[JsoniterDerivationUtils.FieldIndex],
      fieldCount: Int,
      construct: Expr[Array[Any]] => Expr[A],
      readKnownField: (Expr[Int], Expr[Array[Any]], Expr[JsonReader]) => Expr[Unit]
  )

  /** Builds [[ArrayFieldsDecoding]]. The generated code stays linear in the number of fields: each field is read and
    * each absent field filled by its own local `def`, a known key reaches its field through a binary search on its
    * index (no chain of name comparisons, no `fieldNameMapper` call per key), and the values are cast to the field
    * types (no decoder expression evaluated as a type witness).
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
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val FieldIndexT: Type[JsoniterDerivationUtils.FieldIndex] = CTypes.FieldIndex
    implicit val stringifiedT: Type[stringified] = CTypes.Stringified

    val indexed = fields.toList.zipWithIndex
    val fieldCount = indexed.size

    indexed
      .parTraverse { case ((fName, param), index) =>
        import param.tpe.Underlying as Field
        val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
        Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
          deriveFieldDecodeCall[Field](using Type[Field], dctx.copy(stringified = Some(hasStringifiedAnnotation)))
            .map { rawDecode =>
              val decode = withFieldDefault[Field](param, dctx.config, dctx.evaluatedConfig)(rawDecode)
              val readField: Expr[JsonReader] => Expr[Any] =
                if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) { (reader: Expr[JsonReader]) =>
                  Expr.quote {
                    JsoniterDerivationUtils.readUnlessTransient(
                      Expr.splice(reader),
                      Expr.splice(dctx.config).scalaTransientSupport,
                      (r: JsonReader) => Expr.splice(decode(Expr.quote(r))).asInstanceOf[Any]
                    )
                  }
                } else (reader: Expr[JsonReader]) => Expr.quote(Expr.splice(decode(reader)).asInstanceOf[Any])
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
        val fieldIndexMIO: MIO[Expr[JsoniterDerivationUtils.FieldIndex]] =
          cachedKeysOf[A, JsoniterDerivationUtils.FieldIndex]("fieldIndex", names)(keys =>
            Expr.quote(JsoniterDerivationUtils.fieldIndex(Expr.splice(keys)))
          )

        // Fills the absent fields (in parameter order: defaults may depend on preceding parameters) of the field array
        // and constructs the value.
        def construct(decodedValues: Expr[Array[Any]]): Either[String, Expr[A]] = {
          val nonTransientValues: Map[String, Expr_??] = fieldDataList.map { case (fName, param, index, _) =>
            import param.tpe.Underlying as Field
            fName -> castTo[Field](Expr.quote(Expr.splice(decodedValues)(Expr.splice(Expr(index))))).as_??
          }.toMap
          val fieldMap = nonTransientValues ++ buildTransientDefaults(fieldsList, nonTransientValues)
          val fills: List[ValDefs[Expr[Unit]]] = fieldDataList.map { case (fName, param, index, _) =>
            import param.tpe.Underlying as Field
            val fallbacks = absentFieldFallbacks[Field](param, fieldMap)
            // (only computed when a required field is missing)
            val mappedName: Expr[String] = fieldNameOverride(param) match {
              case Some(customName) => Expr(customName)
              case None             => Expr.quote(Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(fName))))
            }
            val normalFallback = runtimeAbsentFallback[Field](fallbacks, dctx.config, dctx.reader, mappedName)
            // A `scala.transient` field (runtime config): its default when `scalaTransientSupport` is on
            val fallback: Expr[Field] = fallbacks.default match {
              case Some(default) if isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig) =>
                ifThenElse[Field](Expr.quote(Expr.splice(dctx.config).scalaTransientSupport), default, normalFallback)
              case _ => normalFallback
            }
            ValDefs
              .createDef[Any](Expr.quote(Expr.splice(fallback).asInstanceOf[Any]), FreshName.FromPrefix("absent"))
              .map { fill =>
                Expr.quote {
                  if (JsoniterDerivationUtils.isAbsentField(Expr.splice(decodedValues)(Expr.splice(Expr(index)))))
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

        val readKnownField: (Expr[Int], Expr[Array[Any]], Expr[JsonReader]) => Expr[Unit] =
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
            fieldIndexMIO.map { fieldIndex =>
              ArrayFieldsDecoding(fieldIndex, fieldCount, arr => construct(arr).toOption.get, readKnownField)
            }
        }
      }
  }

  /** The most keys whose hashes the decoder searches itself (see `decodeCaseClassFieldsOptimized`). */
  private val MaxKeysHashedInDecoder = 64

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) => decodeProduct[A](caseClass).map(Rule.matched)
          case Left(reason)     => MIO.pure(Rule.yielded(reason))
        }
      }

    /** Decodes a product as a JSON object, with the optimized codegen when the config is known at compile time. */
    private[compiletime] def decodeProduct[A: DecoderCtx](caseClass: ProductType[A]): MIO[Expr[A]] =
      if (dctx.evaluatedConfig.isDefined) decodeCaseClassFieldsOptimized[A](caseClass)
      else decodeCaseClassFields[A](caseClass)

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Separate transient from non-transient fields
      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          arrayFieldsDecoding[A](caseClass, fieldsList, fields).map { decoding =>
            implicit val BooleanT: Type[Boolean] = CTypes.Boolean
            implicit val IntT: Type[Int] = CTypes.Int
            implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
            val fieldCount = decoding.fieldCount
            // The object is read here (no dispatch function allocated per decode) and the value is constructed by a
            // local def.
            Expr.quote {
              val reader = Expr.splice(dctx.reader)
              val fieldIndex = Expr.splice(decoding.fieldIndex) // once per decode, not per key
              val decodedValues = JsoniterDerivationUtils.absentFields(Expr.splice(Expr(fieldCount)))
              val _seen: Array[Boolean] =
                if (Expr.splice(dctx.config).checkFieldDuplication)
                  new Array[Boolean](Expr.splice(Expr(fieldCount + 1)))
                else null
              if (!reader.isNextToken('{'.toByte)) reader.decodeError("expected '{'")
              if (!reader.isNextToken('}'.toByte)) {
                reader.rollbackToken()
                var first = true
                while (first || reader.isNextToken(','.toByte)) {
                  first = false
                  val fieldName = reader.readKeyAsString()
                  val index = fieldIndex.indexOf(fieldName)
                  if (index >= 0) {
                    if (_seen != null) {
                      if (_seen(index)) JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                      _seen(index) = true
                    }
                    Expr.splice(
                      decoding.readKnownField(Expr.quote(index), Expr.quote(decodedValues), Expr.quote(reader))
                    )
                  } else
                    Expr.splice {
                      val unexpected: Expr[Unit] = Expr.quote {
                        if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                        else JsoniterDerivationUtils.unexpectedField(reader, fieldName)
                      }
                      // `alwaysEmitDiscriminator`: ADT leaf codecs accept (and skip) the discriminator they write
                      alwaysEmittedDiscriminatorKey[A](dctx.config, None) match {
                        case Some(discriminatorKey) =>
                          Expr.quote {
                            if (fieldName == Expr.splice(discriminatorKey)) {
                              if (_seen != null) {
                                if (_seen(Expr.splice(Expr(fieldCount))))
                                  JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                _seen(Expr.splice(Expr(fieldCount))) = true
                              }
                              reader.skip()
                            } else Expr.splice(unexpected)
                          }
                        case None => unexpected
                      }
                    }
                }
                if (!reader.isCurrentToken('}'.toByte)) reader.decodeError("expected '}' or ','")
              }
              Expr.splice(
                ValDefs
                  .createDef[A](decoding.construct(Expr.quote(decodedValues)), FreshName.FromPrefix("construct"))
                  .use(constructed => constructed)
              )
            }
          }
      }
    }

    /** Optimized case class decoding: readKeyAsCharBuf + isCharBufEqualsTo + no runtime config checks. Used when config
      * is statically evaluable via semiEval.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFieldsOptimized[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val config = dctx.evaluatedConfig.get
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

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          implicit val BooleanT: Type[Boolean] = CTypes.Boolean
          implicit val UnitT: Type[Unit] = CTypes.Unit

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              val mappedName = nameOverride.getOrElse(config.fieldNameMapper(fName))
              Log.namedScope(s"Deriving optimized decoder for field $fName: ${Type[Field].prettyPrint}") {
                val isStringifiedStaticallyFalse = true // fields are stringified only by `@stringified`

                val inlinedDecode: Option[Expr[JsonReader] => Expr[Field]] =
                  if (isStringifiedStaticallyFalse && !hasStringifiedAnnotation)
                    inlineBuiltInDecode[Field](Expr.quote(null.asInstanceOf[JsonReader])).flatMap { _ =>
                      summonJsonValueCodecCached[Field] match {
                        case Right(_) => None
                        case Left(_)  =>
                          Some((readerExpr: Expr[JsonReader]) => inlineBuiltInDecode[Field](readerExpr).get)
                      }
                    }
                  else None

                val fieldCtx = dctx.copy(stringified = Some(hasStringifiedAnnotation))
                // A `scala.Enumeration` value is read in place (not by a call of a helper `def`), as it is written.
                val enumerationMIO: MIO[Option[Expr[JsonReader] => Expr[Field]]] =
                  if (inlinedDecode.isEmpty && Type[Field].isEnumeration && summonJsonValueCodecCached[Field].isLeft)
                    scalaEnumerationFieldDecoder[Field](fieldCtx)
                  else MIO.pure(None)

                lazy val deriveMIO: MIO[Expr[JsonReader] => Expr[Field]] = enumerationMIO.flatMap {
                  case Some(inPlace) => MIO.pure(inPlace)
                  case None          => fieldDeriveMIO
                }
                lazy val fieldDeriveMIO: MIO[Expr[JsonReader] => Expr[Field]] = inlinedDecode match {
                  case Some(inlined) => MIO.pure(inlined)
                  case None          =>
                    // a direct call of the field's decoder (no function value applied per field per decode)
                    Log.namedScope(s"fieldDecoderFn:$fName") {
                      deriveFieldDecodeCall[Field](using
                        Type[Field],
                        dctx.copy(stringified = Some(hasStringifiedAnnotation))
                      )
                    }
                }

                deriveMIO.map { rawDecodeField =>
                  val decodeField = withFieldDefault[Field](param, dctx.config, dctx.evaluatedConfig)(rawDecodeField)
                  // Absent-field handling (see DecoderAbsentFieldSupport), resolved at compile time: an `Option`
                  // without an applicable default starts as `None`; every other field gets a presence flag checked
                  // after the read loop (fill with default/empty, or fail as required). The flag also detects
                  // duplicated keys when `checkFieldDuplication` is on.
                  val readerExpr = dctx.reader
                  val fallbacks = absentFieldFallbacks[Field](param)
                  val startsAsNone = absentIsAlwaysNone(fallbacks, config)
                  val initExpr: Expr[Field] =
                    if (startsAsNone) staticAbsentFallback(fallbacks, config).get else deriveZeroValue[Field]
                  val fieldVar = ValDefs.createVar[Field](initExpr, s"_$fName")
                  if (startsAsNone && !config.checkFieldDuplication)
                    fieldVar.map { case (getter, setter) =>
                      val setFromReader: Expr[JsonReader] => Expr[Unit] = { r => setter(decodeField(r)) }
                      val afterLoop: Map[String, Expr_??] => Expr[Unit] = _ => Expr.quote(())
                      (fName, mappedName, getter.as_??, setFromReader, afterLoop)
                    }
                  else
                    fieldVar.map2(ValDefs.createVar[Boolean](Expr(false), s"_${fName}_present")) {
                      case ((getter, setter), (isPresent, setPresent)) =>
                        val setFromReader: Expr[JsonReader] => Expr[Unit] = { r =>
                          val markPresent: Expr[Unit] =
                            if (config.checkFieldDuplication) Expr.quote {
                              if (Expr.splice(isPresent))
                                JsoniterDerivationUtils.throwDuplicateField(
                                  Expr.splice(r),
                                  Expr.splice(Expr(mappedName))
                                )
                              Expr.splice(setPresent(Expr(true)))
                            }
                            else setPresent(Expr(true))
                          Expr.quote {
                            Expr.splice(markPresent)
                            Expr.splice(setter(decodeField(r)))
                          }
                        }
                        // Built once all field variables exist: defaults may depend on preceding parameters.
                        val afterLoop: Map[String, Expr_??] => Expr[Unit] = values =>
                          if (startsAsNone) Expr.quote(())
                          else {
                            val fallbacksWithArgs = absentFieldFallbacks[Field](param, values)
                            val fill: Expr[Unit] = staticAbsentFallback(fallbacksWithArgs, config) match {
                              case Some(fallback) => setter(fallback)
                              case None           =>
                                setter(missingRequiredFieldOf[Field](readerExpr, Expr(mappedName)))
                            }
                            Expr.quote(if (!Expr.splice(isPresent)) Expr.splice(fill))
                          }
                        (fName, mappedName, getter.as_??, setFromReader, afterLoop)
                    }
                }
              }
            }
            .flatMap { fieldVarDefs =>
              val emptyVarDefs: ValDefs[
                List[(String, String, Expr_??, Expr[JsonReader] => Expr[Unit], Map[String, Expr_??] => Expr[Unit])]
              ] =
                ValDefsTraverse.pure(List.empty)

              val allFieldVars = fieldVarDefs.toList.foldLeft(emptyVarDefs) { (acc, varDef) =>
                acc.map2(varDef) { case (list, entry) => list :+ entry }
              }

              val lenVar = ValDefs.createVar[Int](Expr(-1), "_l")
              // `alwaysEmitDiscriminator`: ADT leaf codecs accept (and skip) the discriminator they write
              val discriminatorKey: Option[String] =
                if (config.alwaysEmitDiscriminator && hasSealedParent[A]) config.discriminatorFieldName else None
              val discriminatorPresentVar: ValDefs[Option[(Expr[Boolean], Expr[Boolean] => Expr[Unit])]] =
                if (discriminatorKey.isDefined && config.checkFieldDuplication)
                  ValDefs.createVar[Boolean](Expr(false), "_discriminator_present").map(Some(_))
                else ValDefsTraverse.pure(None)
              val combined = allFieldVars.map2(lenVar.map2(discriminatorPresentVar)((l, d) => (l, d))) {
                case (fieldInfos, ((lenGet, lenSet), discriminatorPresent)) =>
                  (fieldInfos, lenGet, lenSet, discriminatorPresent)
              }

              // The keys, in the order of `keyEntries` below.
              val keys: List[String] =
                fields.toList.map { case (fName, param) =>
                  fieldNameOverride(param).getOrElse(config.fieldNameMapper(fName))
                } ++ discriminatorKey.toList
              // How a key is looked up, as jsoniter-scala's `JsonCodecMaker` does: compared with each key for a few short
              // keys, otherwise by its hash (`JsonReader.charBufToHashCode`) - here a binary search on the hash in the
              // decoder, a `lookupswitch` upstream. That costs ~40 B per key, so beyond `MaxKeysHashedInDecoder` keys
              // (which would exceed the JIT limit) the hash is looked up by a `JsoniterDerivationUtils.KeyTable` of the
              // codec instance and the field by a binary search on the key's index.
              val compareEachKey = keys.size <= 8 && keys.map(_.length).sum <= 64
              val keyTableMIO: MIO[Option[Expr[JsoniterDerivationUtils.KeyTable]]] =
                if (keys.size <= MaxKeysHashedInDecoder) MIO.pure(None)
                else cachedKeyTable[A](keys).map(Some(_))

              keyTableMIO.map { keyTableOfInstance =>
                // read once per decode into a local (not per key: the instance's lazy val checks its initialization)
                def withKeyTable(use: Option[Expr[JsoniterDerivationUtils.KeyTable]] => Expr[A]): Expr[A] =
                  keyTableOfInstance match {
                    case Some(table) =>
                      implicit val KeyTableT: Type[JsoniterDerivationUtils.KeyTable] = CTypes.KeyTable
                      ValDefs.createVal(table, FreshName.FromPrefix("keyTable")).use(local => use(Some(local)))
                    case None => use(None)
                  }
                withKeyTable { keyTable =>
                  combined.use { case (fieldInfos, lenGetter, lenSetter, discriminatorPresent) =>
                    val nonTransientValues: Map[String, Expr_??] =
                      fieldInfos.map { case (name, _, getter, _, _) => (name, getter) }.toMap
                    val fieldMap: Map[String, Expr_??] =
                      nonTransientValues ++ buildTransientDefaults(fieldsList, nonTransientValues)
                    val constructExpr: Expr[A] = foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                      onTypes = _ => Map.empty,
                      onValues = _ => fieldMap
                    ) match {
                      case Right(expr) => expr.value.asInstanceOf[Expr[A]]
                      case Left(error) => Environment.reportErrorAndAbort(error)
                    }

                    val readerExpr = dctx.reader

                    val unexpectedKey: Expr[Unit] =
                      if (config.skipUnexpectedFields) Expr.quote(Expr.splice(readerExpr).skip())
                      else Expr.quote(Expr.splice(readerExpr).unexpectedKeyError(Expr.splice(lenGetter)))
                    // The keys this decoder reads: the fields, then the discriminator written by `alwaysEmitDiscriminator`
                    // (ADT leaf codecs accept and skip it).
                    val discriminatorEntry: List[(String, Expr[Unit])] = discriminatorKey.toList.map { key =>
                      val markPresent: Expr[Unit] = discriminatorPresent match {
                        case Some((isPresent, setPresent)) =>
                          Expr.quote {
                            if (Expr.splice(isPresent))
                              JsoniterDerivationUtils.throwDuplicateField(
                                Expr.splice(readerExpr),
                                Expr.splice(Expr(key))
                              )
                            Expr.splice(setPresent(Expr(true)))
                          }
                        case None => Expr.quote(())
                      }
                      key -> Expr.quote {
                        Expr.splice(markPresent)
                        Expr.splice(readerExpr).skip()
                      }
                    }
                    val keyEntries: List[(String, Expr[Unit])] =
                      fieldInfos.map { case (_, mappedName, _, setFromReader, _) =>
                        mappedName -> setFromReader(readerExpr)
                      } ++ discriminatorEntry

                    def compareKeys(entries: List[(String, Expr[Unit])]): Expr[Unit] =
                      entries.foldRight(unexpectedKey) { case ((key, read), elseExpr) =>
                        Expr.quote {
                          if (Expr.splice(readerExpr).isCharBufEqualsTo(Expr.splice(lenGetter), Expr.splice(Expr(key))))
                            Expr.splice(read)
                          else Expr.splice(elseExpr)
                        }
                      }
                    val dispatch: Expr[Unit] = keyTable match {
                      case None if compareEachKey => compareKeys(keyEntries)
                      case None                   =>
                        val groups: Vector[(Int, List[(String, Expr[Unit])])] =
                          keyEntries
                            .groupBy { case (k, _) => JsonReader.toHashCode(k.toCharArray, k.length) }
                            .toVector
                            .sortBy(_._1)
                        def search(hash: Expr[Int], from: Int, until: Int): Expr[Unit] =
                          if (until - from == 1) Expr.quote {
                            if (Expr.splice(hash) == Expr.splice(Expr(groups(from)._1)))
                              Expr.splice(compareKeys(groups(from)._2))
                            else Expr.splice(unexpectedKey)
                          }
                          else {
                            val middle = (from + until) / 2
                            Expr.quote {
                              if (Expr.splice(hash) < Expr.splice(Expr(groups(middle)._1)))
                                Expr.splice(search(hash, from, middle))
                              else Expr.splice(search(hash, middle, until))
                            }
                          }
                        Expr.quote {
                          val hash = Expr.splice(readerExpr).charBufToHashCode(Expr.splice(lenGetter))
                          Expr.splice(search(Expr.quote(hash), 0, groups.size))
                        }
                      case Some(table) =>
                        val reads = keyEntries.map(_._2).toVector
                        def search(index: Expr[Int], from: Int, until: Int): Expr[Unit] =
                          if (until - from == 1) reads(from)
                          else {
                            val middle = (from + until) / 2
                            Expr.quote {
                              if (Expr.splice(index) < Expr.splice(Expr(middle)))
                                Expr.splice(search(index, from, middle))
                              else Expr.splice(search(index, middle, until))
                            }
                          }
                        Expr.quote {
                          val index = Expr.splice(table).indexOf(Expr.splice(readerExpr), Expr.splice(lenGetter))
                          if (index < 0) Expr.splice(unexpectedKey)
                          else Expr.splice(search(Expr.quote(index), 0, reads.size))
                        }
                    }

                    val afterLoopAll: Expr[Unit] = foldExprUnits(fieldInfos.map(_._5(fieldMap)))

                    Expr.quote {
                      if (!Expr.splice(readerExpr).isNextToken('{'.toByte))
                        Expr.splice(readerExpr).decodeError("expected '{'")
                      if (!Expr.splice(readerExpr).isNextToken('}'.toByte)) {
                        Expr.splice(readerExpr).rollbackToken()
                        while (Expr.splice(lenGetter) < 0 || Expr.splice(readerExpr).isNextToken(','.toByte)) {
                          Expr.splice(lenSetter(Expr.quote(Expr.splice(readerExpr).readKeyAsCharBuf())))
                          Expr.splice(dispatch)
                        }
                        if (!Expr.splice(readerExpr).isCurrentToken('}'.toByte))
                          Expr.splice(readerExpr).objectEndOrCommaError()
                      }
                      Expr.splice(afterLoopAll)
                      Expr.splice(constructExpr)
                    }
                  }
                }
              }
            }
      }
    }

    /** Decode case class fields from an already-opened JSON object (for discriminator mode). The object's `{` and
      * discriminator key-value have already been read. Returns Expr[A] that reads remaining fields via
      * readObjectInline.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[Boolean] => Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

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

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeInlineObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          arrayFieldsDecoding[A](caseClass, fieldsList, fields).map { decoding => (afterOpen: Expr[Boolean]) =>
            implicit val BooleanT: Type[Boolean] = CTypes.Boolean
            implicit val IntT: Type[Int] = CTypes.Int
            implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
            val fieldCount = decoding.fieldCount
            Expr.quote {
              val _seen: Array[Boolean] =
                if (Expr.splice(dctx.config).checkFieldDuplication)
                  new Array[Boolean](Expr.splice(Expr(fieldCount + 1)))
                else null
              JsoniterDerivationUtils.readObjectInline[A](
                Expr.splice(dctx.reader),
                Expr.splice(Expr(fieldCount)),
                (decodedValues: Array[Any]) => Expr.splice(decoding.construct(Expr.quote(decodedValues))),
                Expr.splice(afterOpen),
                (Expr.splice(dctx.config).discriminatorFieldName match {
                  case Some(name) => name; case _ => null
                })
              ) { case (fieldName, arr, reader) =>
                val index = Expr.splice(decoding.fieldIndex).indexOf(fieldName)
                if (index >= 0) {
                  if (_seen != null) {
                    if (_seen(index)) JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                    _seen(index) = true
                  }
                  Expr.splice(decoding.readKnownField(Expr.quote(index), Expr.quote(arr), Expr.quote(reader)))
                }
                // The first discriminator was consumed already: a second one is a duplicate (with
                // checkFieldDuplication) or skipped, as in jsoniter-scala.
                else if (Expr.splice(dctx.config).discriminatorFieldName.contains(fieldName)) {
                  if (_seen != null) JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                  else reader.skip()
                } else if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                else JsoniterDerivationUtils.unexpectedField(reader, fieldName)
              }
            }
          }
      }
    }

  }

  /** Derive a decode function for a case class field. Tries implicit summoning first, falls back to a forward-declared
    * cached `def`.
    *
    * Per project rule 5, [[LambdaBuilder]] is reserved for lambdas passed into collection / Optional iteration helpers.
    * The recursive fallback uses the def-caching pattern (see `docs/contributing/def-caching-skill.md`):
    *
    *   - Forward-declare `def decode_field_X(reader: JsonReader, config: JsoniterConfig): Field` against the shared
    *     [[ValDefsCache]] (the same one populated by the surrounding codec entry point).
    *   - Populate it via `buildCachedWith`, building a *fresh* `DecoderCtx` from the def's own parameter Exprs so the
    *     body never references outer-scope-bound names (which would not resolve once the def is hoisted to the outer
    *     scope by `vals.toValDefs.use`).
    *   - Wrap the resulting helper-call function in a direct cross-quotes lambda so callers still get the expected
    *     `Expr[JsonReader => Field]` shape.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[JsonReader => Field]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    deriveFieldDecodeCall[Field].map { decode =>
      Expr.quote((r: JsonReader) => Expr.splice(decode(Expr.quote(r))))
    }
  }

  /** [[deriveFieldDecoder]] as a function building the decoding expression (a call of the implicit codec or of the
    * cached def), not as a function value applied to the reader.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecodeCall[Field: Type](implicit
      ctx: DecoderCtx[?]
  ): MIO[Expr[JsonReader] => Expr[Field]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig

    Log.namedScope(s"deriveFieldDecoder:${Type[Field].prettyPrint}") {
      summonJsonValueCodecCached[Field] match {
        case Right(codecExpr) =>
          Log.info(s"Found implicit JsonValueCodec[${Type[Field].prettyPrint}]") >> MIO.pure { (r: Expr[JsonReader]) =>
            Expr.quote(Expr.splice(codecExpr).decodeValue(Expr.splice(r), Expr.splice(codecExpr).nullValue))
          }
        case Left(_) =>
          Log.info(s"Building decoder for ${Type[Field].prettyPrint} via cached def") >> {
            val stringifiedSuffix = stringifiedKeySuffix(ctx.stringified)
            val cacheKey = s"jsoniter-field-decode:${Type[Field].plainPrint}$stringifiedSuffix"
            val defBuilder = ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, Field](
              s"decode_field_${stringifiedNamePrefix(ctx.stringified)}${Type[Field].shortName}"
            )
            for {
              // Memoize: case classes can have multiple fields of the same type, and the
              // case-class rule iterates them sequentially. Without this guard, the second
              // field of the same type would re-enter `buildCachedWith` for an already-built
              // key and trigger `HearthRequirementError("key+signature already built")`.
              cacheState <- ctx.cache.get
              _ <-
                if (defBuilder.isBuilt(cacheState, cacheKey)) MIO.pure(())
                else
                  for {
                    _ <- ctx.cache.forwardDeclare(cacheKey, defBuilder)
                    _ <- MIO.scoped { rs =>
                      rs(ctx.cache.buildCachedWith(cacheKey, defBuilder) { case (_, (readerExpr, configExpr)) =>
                        rs(
                          deriveDecoderRecursively[Field](using
                            DecoderCtx
                              .from[Field](
                                readerExpr,
                                configExpr,
                                ctx.cache,
                                ctx.derivedType,
                                evaluatedConfig = ctx.evaluatedConfig,
                                rootConfig = ctx.rootConfig
                              )
                              .copy(stringified = ctx.stringified)
                          )
                        )
                      })
                    }
                  } yield ()
              callerOpt <- ctx.cache.get2Ary[JsonReader, JsoniterConfig, Field](cacheKey)
            } yield {
              val callerFn = callerOpt.get
              val configExpr: Expr[JsoniterConfig] = ctx.config
              (r: Expr[JsonReader]) => callerFn(r, configExpr)
            }
          }
      }
    }
  }

}
