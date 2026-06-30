package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonWriter
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait EncoderHandleAsEnumRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            // Note: caching is handled by deriveEncoderRecursively
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
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val StringT: Type[String] = CTypes.String

      val childrenList = enumm.directChildren.toList
      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum || childrenList.forall { case (_, child) =>
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

            // Fields-only encoding for discriminator mode
            val fieldsOnlyMIO: MIO[Expr[Unit]] =
              if (Type[A].isEnumeration || Type[A].isJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else
                SingletonValue.unapply(Type[EnumCase]) match {
                  case Some(_) => MIO.pure(Expr.quote(()): Expr[Unit])
                  case None    =>
                    CaseClass.parse[EnumCase].toOption match {
                      case Some(caseClass) =>
                        EncoderHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](caseClass)(using
                          ectx.nest(enumCaseValue)
                        )
                      case None => MIO.pure(Expr.quote(()): Expr[Unit])
                    }
                }

            // Full encoding for wrapper mode
            val fullEncMIO: MIO[Expr[Unit]] =
              if (Type[A].isEnumeration || Type[A].isJavaEnum) MIO.pure(Expr.quote(()): Expr[Unit])
              else
                SingletonValue.unapply(Type[EnumCase]) match {
                  case Some(_) => MIO.pure(Expr.quote(()): Expr[Unit])
                  case None    => deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
                }

            for {
              fieldsOnly <- fieldsOnlyMIO
              fullEnc <- fullEncMIO
            } yield
              if (allCaseObjects) {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  if (config.enumAsStrings) {
                    UBJsonDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
                  } else {
                    config.discriminatorFieldName match {
                      case Some(discriminatorField) =>
                        Expr.splice(ectx.writer).writeObjectStart()
                        Expr.splice(ectx.writer).writeFieldName(discriminatorField)
                        Expr.splice(ectx.writer).writeString(name)
                        Expr.splice(fieldsOnly)
                        Expr.splice(ectx.writer).writeObjectEnd()
                      case None =>
                        UBJsonDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                          Expr.splice(fullEnc)
                        }
                    }
                  }
                }
              } else {
                Expr.quote {
                  val config = Expr.splice(ectx.config)
                  val name = config.adtLeafClassNameMapper(Expr.splice(Expr(caseName)))
                  config.discriminatorFieldName match {
                    case Some(discriminatorField) =>
                      Expr.splice(ectx.writer).writeObjectStart()
                      Expr.splice(ectx.writer).writeFieldName(discriminatorField)
                      Expr.splice(ectx.writer).writeString(name)
                      Expr.splice(fieldsOnly)
                      Expr.splice(ectx.writer).writeObjectEnd()
                    case None =>
                      UBJsonDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
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
