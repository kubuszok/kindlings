package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

trait EncoderHandleAsEnumRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            // Note: caching is handled by deriveEncoderRecursively — do NOT call setHelper here.
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val JsonWriterT: Type[JsonWriter] = CTypes.JsonWriter
      implicit val StringT: Type[String] = CTypes.String

      // Check at compile time if all children are singletons (case objects with no fields)
      val childrenList = enumm.directChildren.toList
      val isEnumerationOrJavaEnum = Type[A].isEnumeration || Type[A].isJavaEnum
      val allCaseObjects = isEnumerationOrJavaEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      enumm
        .parMatchOn[MIO, Unit](ectx.value) { matched =>
          import matched.{value as enumCaseValue, Underlying as EnumCase}
          Log.namedScope(s"Encoding enum case ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
            val caseName: String = childrenList
              .find { case (_, child) =>
                import child.Underlying as ChildType
                Type[EnumCase] <:< Type[ChildType]
              }
              .map(_._1)
              .getOrElse(Type[EnumCase].shortName)

            // For discriminator mode, we need fields-only encoding to avoid double wrapping.
            // Parse child as case class to get field-level access.
            val fieldsOnlyMIO: MIO[Expr[Unit]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else
                SingletonValue.unapply(Type[EnumCase]) match {
                  case Some(_) =>
                    // Singleton — no fields
                    MIO.pure(Expr.quote(()): Expr[Unit])
                  case None =>
                    CaseClass.parse[EnumCase].toOption match {
                      case Some(caseClass) =>
                        EncoderHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using
                          ectx.nest(enumCaseValue)
                        )
                      case None =>
                        // Not a case class (e.g. case object) — no fields
                        MIO.pure(Expr.quote(()): Expr[Unit])
                    }
                }

            // Also derive the full encoding for wrapper mode
            val fullEncMIO: MIO[Expr[Unit]] =
              if (isEnumerationOrJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

            for {
              fieldsOnly <- fieldsOnlyMIO
              fullEnc <- fullEncMIO
            } yield
              if (allCaseObjects) {
                if (isEnumerationOrJavaEnum)
                  Expr.quote {
                    val config = Expr.splice(ectx.config)
                    if (config.useScalaEnumValueId)
                      JsoniterDerivationUtils.writeScalaEnumValueId(
                        Expr.splice(ectx.writer),
                        Expr.splice(enumCaseValue)
                      )
                    else {
                      val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                      if (config.enumAsStrings)
                        JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                      else
                        config.discriminatorFieldName match {
                          case Some(discriminatorField) =>
                            Expr.splice(ectx.writer).writeObjectStart()
                            Expr.splice(ectx.writer).writeKey(discriminatorField)
                            Expr.splice(ectx.writer).writeVal(name)
                            Expr.splice(ectx.writer).writeObjectEnd()
                          case None =>
                            JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {}
                        }
                    }
                  }
                else
                  Expr.quote {
                    val config = Expr.splice(ectx.config)
                    val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                    if (config.enumAsStrings) {
                      JsoniterDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                    } else {
                      config.discriminatorFieldName match {
                        case Some(discriminatorField) =>
                          Expr.splice(ectx.writer).writeObjectStart()
                          Expr.splice(ectx.writer).writeKey(discriminatorField)
                          Expr.splice(ectx.writer).writeVal(name)
                          Expr.splice(fieldsOnly)
                          Expr.splice(ectx.writer).writeObjectEnd()
                        case None =>
                          JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                            Expr.splice(fullEnc)
                          }
                      }
                    }
                  }
              } else {
                val isSingletonCase = SingletonValue.unapply(Type[EnumCase]).isDefined
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  if (config.circeLikeObjectEncoding && Expr.splice(Expr(isSingletonCase)))
                    JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                      Expr.splice(ectx.writer).writeObjectStart()
                      Expr.splice(ectx.writer).writeObjectEnd()
                    }
                  else
                    config.discriminatorFieldName match {
                      case Some(discriminatorField) =>
                        Expr.splice(ectx.writer).writeObjectStart()
                        Expr.splice(ectx.writer).writeKey(discriminatorField)
                        Expr.splice(ectx.writer).writeVal(name)
                        Expr.splice(fieldsOnly)
                        Expr.splice(ectx.writer).writeObjectEnd()
                      case None =>
                        JsoniterDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                          Expr.splice(fullEnc)
                        }
                    }
                }
              }
          }
        }
        .flatMap {
          case Some(result) => MIO.pure(result)
          case None         =>
            val err = CodecDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
    }
  }
}
