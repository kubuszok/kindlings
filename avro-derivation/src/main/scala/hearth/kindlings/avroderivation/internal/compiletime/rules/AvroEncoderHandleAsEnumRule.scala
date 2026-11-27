package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroEncoderHandleAsEnumRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsEnumRule extends EncoderDerivationRule("handle as enum when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            encodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeEnumCases[A: EncoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[Any]] = {
      implicit val AnyT: Type[Any] = EncTypes.Any
      implicit val SchemaT: Type[Schema] = EncTypes.Schema

      val childrenList = enumm.directChildren.toList

      val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
        childrenList.forall { case (_, child) =>
          SingletonValue.unapply(child.Underlying).isDefined
        }

      if (allCaseObjects) {
        // Pure enum → encode as GenericData.EnumSymbol
        cachedEncodeSchema[A].flatMap { schemaExpr =>
          enumm
            .parMatchOn[MIO, Any](ectx.value) { matched =>
              import matched.Underlying as EnumCase
              val nameExpr: Expr[String] = childrenList.find { case (_, child) =>
                import child.Underlying as ChildType
                Type[EnumCase] <:< Type[ChildType]
              } match {
                case Some((childName, child)) =>
                  import child.Underlying as ChildType
                  enumSymbolExpr[ChildType](childName, ectx.config, ectx.evaluatedConfig)
                case None =>
                  enumSymbolExpr[EnumCase](Type[EnumCase].shortName, ectx.config, ectx.evaluatedConfig)
              }
              MIO.pure(
                Expr.quote(AvroDerivationUtils.encodeEnumSymbol(Expr.splice(schemaExpr), Expr.splice(nameExpr)): Any)
              )
            }
            .flatMap {
              case Some(result) => MIO.pure(result)
              case None         =>
                val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
                Log.error(err.message) >> MIO.fail(err)
            }
        }
      } else {
        // Mixed sealed trait → encode as the appropriate record subtype
        enumm
          .parMatchOn[MIO, Any](ectx.value) { matched =>
            import matched.{value as enumCaseValue, Underlying as EnumCase}
            Log.namedScope(s"Encoding enum case ${Type[EnumCase].prettyPrint}") {
              deriveEncoderRecursively[EnumCase](using ectx.nest(enumCaseValue))
            }
          }
          .flatMap {
            case Some(result) => MIO.pure(result)
            case None         =>
              val err = EncoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
              Log.error(err.message) >> MIO.fail(err)
          }
      }
    }
  }
}
