package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

/** Absent-field semantics shared by the case class and named tuple decoders, matching jsoniter-scala's
  * `JsonCodecMaker.make`. A field missing from the JSON object is filled with, in order of precedence:
  *
  *   1. its default value (unless `requireDefaultFields`),
  *   1. `None` for an `Option` (never required),
  *   1. an empty collection/map (unless `requireCollectionFields`; collections that cannot be empty, like `::` or
  *      non-empty collections from integrations, are always required),
  *
  * otherwise decoding fails with `missing required field "<name>"` (a `JsonReaderException`).
  *
  * `transientDefault` / `transientEmpty` / `transientNone` only affect encoding.
  */
trait DecoderAbsentFieldSupport {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** What can stand in for an absent field before the config's `require*` flags are applied. */
  final protected case class AbsentFieldFallbacks[Field](
      default: Option[Expr[Field]],
      isOption: Boolean,
      empty: Option[Expr[Field]],
      hasDefault: Boolean = false,
      // `T | Null` (Scala 3): absent is `null`, never required (as `None` for an `Option`)
      isNullable: Boolean = false
  )

  /** @param argumentValues
    *   the values of the other constructor parameters: the default value of a parameter of a non-first parameter list
    *   may depend on the preceding parameters (without them such a default is not available)
    */
  protected def absentFieldFallbacks[Field: Type](
      param: Parameter,
      argumentValues: Map[String, Expr_??] = Map.empty
  ): AbsentFieldFallbacks[Field] = {
    val default: Option[Expr[Field]] =
      parameterDefault(param, argumentValues).map(ee => ee.value.asInstanceOf[Expr[Field]])
    val isOption = Type[Field] match {
      case IsOption(_) => true
      case _           => false
    }
    val isNullable = nullableUnionUnderlying[Field].isDefined
    AbsentFieldFallbacks(
      default,
      isOption,
      if (isOption || isNullable) None else emptyCollectionOf[Field],
      param.hasDefault,
      isNullable
    )
  }

  /** Fallback for an absent field when the config is known at compile time; `None` means the field is required. */
  protected def staticAbsentFallback[Field: Type](
      fallbacks: AbsentFieldFallbacks[Field],
      config: JsoniterConfig
  ): Option[Expr[Field]] =
    fallbacks.default
      .filter(_ => !config.requireDefaultFields)
      .orElse(if (fallbacks.isOption) Some(noneOf[Field]) else None)
      .orElse(if (fallbacks.isNullable) Some(nullOf[Field]) else None)
      .orElse(fallbacks.empty.filter(_ => !config.requireCollectionFields))

  /** Whether the absent value is `None` regardless of the `require*` flags (so a variable can start as `None`). */
  protected def absentIsAlwaysNone[Field](fallbacks: AbsentFieldFallbacks[Field], config: JsoniterConfig): Boolean =
    fallbacks.isOption && (!fallbacks.hasDefault || config.requireDefaultFields)

  /** Fallback for an absent field when the config is only known at runtime (fails for a required field). */
  @scala.annotation.nowarn("msg=is never used")
  protected def runtimeAbsentFallback[Field: Type](
      fallbacks: AbsentFieldFallbacks[Field],
      config: Expr[JsoniterConfig],
      reader: Expr[JsonReader],
      mappedName: Expr[String]
  ): Expr[Field] = {
    val required: Expr[Field] = missingRequiredFieldOf[Field](reader, mappedName)
    val withEmpty: Expr[Field] = fallbacks.empty.fold(required) { empty =>
      Expr.quote {
        if (Expr.splice(config).requireCollectionFields) Expr.splice(required) else Expr.splice(empty)
      }
    }
    val withOption: Expr[Field] =
      if (fallbacks.isOption) noneOf[Field] else if (fallbacks.isNullable) nullOf[Field] else withEmpty
    fallbacks.default.fold(withOption) { default =>
      Expr.quote {
        if (Expr.splice(config).requireDefaultFields) Expr.splice(withOption) else Expr.splice(default)
      }
    }
  }

  /** For decoders collecting field values into an `Array[Any]` (absent slots hold
    * `JsoniterDerivationUtils.AbsentField`): replace each absent slot with the field's runtime fallback.
    *
    * @param fields
    *   (field name, parameter, array index, `@fieldName` override)
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def absentFieldFillExprs(
      fields: List[(String, Parameter, Int, Option[String])],
      decodedValuesExpr: Expr[Array[Any]],
      config: Expr[JsoniterConfig],
      reader: Expr[JsonReader],
      argumentValues: Map[String, Expr_??] = Map.empty,
      runtimeOnlyTransient: String => Boolean = _ => false
  ): List[Expr[Unit]] = {
    implicit val AnyT: Type[Any] = CTypes.Any
    implicit val StringT: Type[String] = CTypes.String
    fields.map { case (fName, param, idx, nameOverride) =>
      import param.tpe.Underlying as Field
      val mappedName: Expr[String] = nameOverride match {
        case Some(customName) => Expr(customName)
        case None             => Expr.quote(Expr.splice(config).fieldNameMapper(Expr.splice(Expr(fName))))
      }
      val fallbacks = absentFieldFallbacks[Field](param, argumentValues)
      val normalFallback = runtimeAbsentFallback[Field](fallbacks, config, reader, mappedName)
      // A `scala.transient` field (runtime config): its default when `scalaTransientSupport` is on
      val fallback: Expr[Field] = fallbacks.default match {
        case Some(default) if runtimeOnlyTransient(fName) =>
          Expr.quote {
            if (Expr.splice(config).scalaTransientSupport) Expr.splice(default) else Expr.splice(normalFallback)
          }
        case _ => normalFallback
      }
      Expr.quote {
        if (JsoniterDerivationUtils.isAbsentField(Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx)))))
          Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(fallback).asInstanceOf[Any]
      }
    }
  }

  /** The value a JSON `null` decodes to for a field with a default value: the default (jsoniter-scala passes the field's
    * default as `default` to its decoder, and reference-typed decoders return it for `null`), also for `Option`s with a
    * default. `None` for nullable `T | Null` fields, primitives and value classes (`null` is rejected), and fields
    * without a default.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def nullAsDefaultValue[Field: Type](param: Parameter): Option[Expr[Field]] = {
    val fallbacks = absentFieldFallbacks[Field](param)
    // (as jsoniter-scala's Defaults2, Spec ~2530: `"oc":null` with `oc: Option[Char] = Some('X')` is the default)
    // (not for `Option[Option[_]]`: with skipNestedOptionValues, null is `Some(None)`)
    val isNestedOption = Type[Field] match {
      case IsOption(isOption) =>
        import isOption.Underlying as Inner
        Inner match {
          case IsOption(_) => true
          case _           => false
        }
      case _ => false
    }
    if (fallbacks.isNullable || isNestedOption || !(Type[Field] <:< Type.of[AnyRef])) None else fallbacks.default
  }

  /** Wraps a field decoder with [[nullAsDefaultValue]], if it applies. */
  @scala.annotation.nowarn("msg=is never used")
  protected def withNullAsDefault[Field: Type](param: Parameter)(
      decode: Expr[JsonReader] => Expr[Field]
  ): Expr[JsonReader] => Expr[Field] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    nullAsDefaultValue[Field](param) match {
      case Some(default) =>
        (reader: Expr[JsonReader]) =>
          Expr.quote {
            JsoniterDerivationUtils.readNullAsDefault[Field](Expr.splice(reader), Expr.splice(default)) {
              (r: JsonReader) => Expr.splice(decode(Expr.quote(r)))
            }
          }
      case None => decode
    }
  }

  /** The field's own default value (not one depending on preceding parameters), as jsoniter-scala initializes the
    * field's variable with it and passes that variable as `default` to the field's decoder.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def independentDefault[Field: Type](param: Parameter): Option[Expr[Field]] =
    parameterDefault(param, Map.empty).map(_.value.asInstanceOf[Expr[Field]])

  /** Whether the field's type is a collection or a map (decoded as a JSON array/object), not a `String`. */
  @scala.annotation.nowarn("msg=is never used")
  private def isCollectionField[Field: Type]: Boolean =
    !(Type[Field] =:= CTypes.String) && (Type[Field] match {
      case IsCollection(_) => true
      case _               => false
    })

  /** jsoniter-scala's `genReadVal(fieldType, default = <field variable>)` for a field of a class:
    *
    *   - a user-provided codec receives the field's default value (else its `nullValue`) as the `default` argument of
    *     `decodeValue`, and decides itself what a JSON `null` is (Spec ~674);
    *   - an empty JSON array/object for a collection/map field with a default value decodes as that default, as a JSON
    *     `null` does (Defaults2, Spec ~2530: `"l":[]` with `l: List[Int] = List(0)` is `List(0)`);
    *   - otherwise [[withNullAsDefault]].
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def withFieldDefault[Field: Type](param: Parameter)(
      decode: Expr[JsonReader] => Expr[Field]
  ): Expr[JsonReader] => Expr[Field] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    val default = independentDefault[Field](param)
    summonJsonValueCodecCached[Field] match {
      case Right(codec) =>
        default match {
          case Some(d) =>
            (reader: Expr[JsonReader]) =>
              Expr.quote(Expr.splice(codec).decodeValue(Expr.splice(reader), Expr.splice(d)))
          case None => decode
        }
      case Left(_) =>
        val emptyAsDefault: Expr[JsonReader] => Expr[Field] = default match {
          case Some(d) if isCollectionField[Field] =>
            (reader: Expr[JsonReader]) =>
              Expr.quote(JsoniterDerivationUtils.emptyAsDefault[Field](Expr.splice(decode(reader)), Expr.splice(d)))
          case _ => decode
        }
        withNullAsDefault[Field](param)(emptyAsDefault)
    }
  }

  /** [[withFieldDefault]] for decode functions. */
  @scala.annotation.nowarn("msg=is never used")
  protected def withFieldDefaultFn[Field: Type](param: Parameter)(
      decodeFn: Expr[JsonReader => Field]
  ): Expr[JsonReader => Field] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    val unchanged = independentDefault[Field](param).isEmpty && nullAsDefaultValue[Field](param).isEmpty
    if (unchanged) decodeFn
    else {
      val wrapped = withFieldDefault[Field](param)(r => Expr.quote(Expr.splice(decodeFn).apply(Expr.splice(r))))
      Expr.quote((r: JsonReader) => Expr.splice(wrapped(Expr.quote(r))))
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def missingRequiredFieldOf[Field: Type](reader: Expr[JsonReader], mappedName: Expr[String]): Expr[Field] =
    Expr.quote(JsoniterDerivationUtils.missingRequiredField[Field](Expr.splice(reader), Expr.splice(mappedName)))

  @scala.annotation.nowarn("msg=is never used")
  private def noneOf[Field: Type]: Expr[Field] = Expr.quote(scala.None.asInstanceOf[Field])

  @scala.annotation.nowarn("msg=is never used")
  private def nullOf[Field: Type]: Expr[Field] = Expr.quote(null.asInstanceOf[Field])

  /** An empty instance of a collection/map type, if it has one (`String` and `::` are not treated as collections). */
  @scala.annotation.nowarn("msg=is never used")
  protected def emptyCollectionOf[Field: Type]: Option[Expr[Field]] =
    if (Type[Field] =:= CTypes.String) None
    else if (Type[Field] <:< Type.of[scala.collection.immutable.::[Any]]) None
    else
      Type[Field] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          val coll = isCollection.value
          import coll.CtorResult
          coll.build match {
            case _: CtorLikeOf.PlainValue[?, ?] =>
              val emptyBuilder: Expr[scala.collection.mutable.Builder[Item, CtorResult]] =
                Expr.quote(Expr.splice(coll.factory).newBuilder)
              Some(coll.build.ctor(emptyBuilder).asInstanceOf[Expr[Field]])
            case _ => None
          }
        case _ => None
      }
}
