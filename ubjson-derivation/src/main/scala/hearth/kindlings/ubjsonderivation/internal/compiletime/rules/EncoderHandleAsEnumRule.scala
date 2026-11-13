package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
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

      // As in jsoniter-scala, nested sealed hierarchies are flattened: only their leaves are encoded (dispatching on the
      // leaves, not on the direct children, so intermediate sealed traits are not nested wrappers).
      val childrenList: List[(String, ??<:[A])] = adtLeaves[A](enumm)
      val isValueEnum = Type[A].isEnumeration || isJavaEnumType[A]
      val allCaseObjects = isValueEnum || childrenList.forall { case (_, child) =>
        SingletonValue.unapply(child.Underlying).isDefined
      }

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = CodecDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)
        case Some(children) =>
          // ADT leaves (Java enum values) mapping to the same name fail the derivation (names known at compile time)
          val collisionCheck: MIO[Unit] = ectx.evaluatedConfig match {
            case Some(config) if !Type[A].isEnumeration =>
              checkDiscriminatorValueCollisions[A](childrenList.map { case (childName, child) =>
                import child.Underlying as Child
                if (isJavaEnumType[A]) config.javaEnumValueNameMapper(childName)
                else
                  namedAnnotationOf[Child].getOrElse(
                    config.adtLeafClassNameMapper(adtLeafFullName[A, Child](childName))
                  )
              })
            case _ => MIO.pure(())
          }

          collisionCheck >> children
            .parTraverse { case (caseName, child) =>
              import child.Underlying as EnumCase
              MIO.scoped { runSafe =>
                val mc: MatchCase[Expr[EnumCase]] = MatchCase.typeMatch[EnumCase](caseName)
                mc.map { (enumCaseValue: Expr[EnumCase]) =>
                  runSafe {
                    Log.namedScope(s"Encoding enum case $caseName: ${Type[EnumCase].prettyPrint}") {
                      encodeLeaf[A, EnumCase](caseName, enumCaseValue, isValueEnum, allCaseObjects)
                    }
                  }
                }
              }
            }
            .map(matchCases => ectx.value.matchOn[Unit](matchCases.toNonEmptyVector))
      }
    }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeLeaf[A: EncoderCtx, EnumCase: Type](
        caseName: String,
        enumCaseValue: Expr[EnumCase],
        isValueEnum: Boolean,
        allCaseObjects: Boolean
    ): MIO[Expr[Unit]] = {
      implicit val UnitT: Type[Unit] = CTypes.Unit
      implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
      implicit val StringT: Type[String] = CTypes.String
      val isSingleton = SingletonValue.unapply(Type[EnumCase]).isDefined

      // Fields-only encoding for discriminator mode
      val fieldsOnlyMIO: MIO[Expr[Unit]] =
        if (isValueEnum || isSingleton) MIO.pure(Expr.quote(()): Expr[Unit])
        else
          ProductType.asLeaf[EnumCase] match {
            case Some(product) =>
              EncoderHandleAsCaseClassRule.encodeCaseClassFieldsOnly[EnumCase](product)(using ectx.nest(enumCaseValue))
            case None => MIO.pure(Expr.quote(()): Expr[Unit])
          }

      // Full encoding for wrapper mode
      val fullEncMIO: MIO[Expr[Unit]] =
        if (isValueEnum || isSingleton) MIO.pure(Expr.quote(()): Expr[Unit])
        else if (Type[EnumCase] =:= Type[A])
          // A non-abstract sealed base as its own leaf: deriving it recursively would be this codec.
          ProductType.asLeaf[EnumCase] match {
            case Some(product) =>
              EncoderHandleAsCaseClassRule.encodeCaseClassFields[EnumCase](product)(using ectx.nest(enumCaseValue))
            case None => MIO.pure(Expr.quote(()): Expr[Unit])
          }
        else deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))

      for {
        fieldsOnly <- fieldsOnlyMIO
        fullEnc <- fullEncMIO
      } yield {
        val nameExpr = enumLeafNameExpr[A, EnumCase](caseName, ectx.config, ectx.evaluatedConfig)
        if (isValueEnum) {
          // As in jsoniter-scala: Java enum and Enumeration values are always strings (Enumeration values are their
          // ids with useScalaEnumValueId)
          val asName: Expr[Unit] = Expr.quote(Expr.splice(ectx.writer).writeString(Expr.splice(nameExpr)))
          if (!Type[A].isEnumeration) asName
          else
            ectx.evaluatedConfig.map(_.useScalaEnumValueId) match {
              case Some(false) => asName
              case Some(true)  =>
                Expr.quote(
                  UBJsonDerivationUtils.writeScalaEnumValueId(Expr.splice(ectx.writer), Expr.splice(enumCaseValue))
                )
              case None =>
                Expr.quote {
                  if (Expr.splice(ectx.config).useScalaEnumValueId)
                    UBJsonDerivationUtils.writeScalaEnumValueId(Expr.splice(ectx.writer), Expr.splice(enumCaseValue))
                  else Expr.splice(asName)
                }
            }
        } else if (isSingleton) {
          // As in jsoniter-scala: a case object is `"Leaf"` without a discriminator (`{"Leaf":{}}` with
          // circeLikeObjectEncoding) and `{"type":"Leaf"}` with one; enumAsStrings writes ADTs of case objects
          // only as strings with a discriminator too.
          val enumAsStrings: Expr[Boolean] =
            if (allCaseObjects) Expr.quote(Expr.splice(ectx.config).enumAsStrings) else Expr(false)
          Expr.quote {
            val config = Expr.splice(ectx.config)
            val name = Expr.splice(nameExpr)
            if (Expr.splice(enumAsStrings))
              UBJsonDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
            else
              config.discriminatorFieldName match {
                case Some(discriminatorField) =>
                  Expr.splice(ectx.writer).writeObjectStart()
                  Expr.splice(ectx.writer).writeFieldName(discriminatorField)
                  Expr.splice(ectx.writer).writeString(name)
                  Expr.splice(ectx.writer).writeObjectEnd()
                case None =>
                  if (config.circeLikeObjectEncoding)
                    UBJsonDerivationUtils.writeWrapped(Expr.splice(ectx.writer), name) {
                      Expr.splice(ectx.writer).writeObjectStart()
                      Expr.splice(ectx.writer).writeObjectEnd()
                    }
                  else UBJsonDerivationUtils.writeEnumAsString(Expr.splice(ectx.writer), name)
              }
          }
        } else
          Expr.quote {
            val config = Expr.splice(ectx.config)
            val name = Expr.splice(nameExpr)
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
}
