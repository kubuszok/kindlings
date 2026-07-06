package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.generic.GenericRecord

trait AvroDecoderHandleAsEnumRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsEnumRule extends DecoderDerivationRule("handle as enum when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            decodeEnumCases[A](enumm).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeEnumCases[A: DecoderCtx](
        enumm: Enum[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val AnyT: Type[Any] = DecTypes.Any

      val childrenList = enumm.directChildren.toList

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = DecoderDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)

        case Some(children) =>
          val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
            children.toList.forall { case (_, child) =>
              SingletonValue.unapply(child.Underlying).isDefined
            }

          if (allCaseObjects) {
            // Pure enum → decode from GenericData.EnumSymbol string
            val knownNames: List[String] = children.toList.map(_._1)

            // Build dispatch chain: if name matches → return case object singleton
            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving decoder for enum case $childName") {
                  SingletonValue.unapply(Type[ChildType]) match {
                    case Some(sv) =>
                      MIO.pure((childName, sv.singletonExpr.asInstanceOf[Expr[A]]))
                    case None =>
                      // Fallback to construct for non-singleton zero-arg case classes
                      CaseClass.parse[ChildType].toOption match {
                        case Some(cc) =>
                          cc.construct[MIO](new CaseClass.ConstructField[MIO] {
                            def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                              val err =
                                DecoderDerivationError.EnumChildError(childName, "Unexpected parameter in case object")
                              Log.error(err.message) >> MIO.fail(err)
                            }
                          }).flatMap {
                            case Some(expr) => MIO.pure((childName, expr.asInstanceOf[Expr[A]]))
                            case None       =>
                              val err = DecoderDerivationError.EnumChildError(childName, "Cannot construct")
                              Log.error(err.message) >> MIO.fail(err)
                          }
                        case None =>
                          val err = DecoderDerivationError.EnumChildError(childName, "is not parseable as a case class")
                          Log.error(err.message) >> MIO.fail(err)
                      }
                  }
                }
              }
              .map { dispatchers =>
                val errorExpr: Expr[A] = Expr.quote {
                  AvroDerivationUtils.failedToMatchSubtype(
                    AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue)),
                    Expr.splice(Expr(knownNames))
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((childName, childExpr), elseExpr) =>
                  Expr.quote {
                    val name = AvroDerivationUtils.decodeEnumSymbol(Expr.splice(dctx.avroValue))
                    if (Expr.splice(dctx.config).transformConstructorNames(Expr.splice(Expr(childName))) == name)
                      Expr.splice(childExpr)
                    else
                      Expr.splice(elseExpr)
                  }
                }
              }
          } else {
            // Mixed sealed trait → dispatch based on record schema name.
            //
            // The record name written by the encoder for each child is derived from the child's own
            // schema via `computeAvroNameExpr`. For a *generic* case (e.g. `Set[Content]`) that name
            // embeds the applied type parameters ("Set__Content"); it also honours @avroName /
            // @avroErasedName / @avroFqnParamNames. We MUST recompute the expected name here with the
            // exact same logic — a plain simple class name ("Set") never matches "Set__Content".
            // Note: the schema record name is NOT passed through `transformConstructorNames` (only
            // enum-of-case-object symbols are), so we don't apply it here either — doing so would
            // reintroduce a mismatch against the encoder.
            children
              .parTraverse { case (childName, child) =>
                import child.Underlying as ChildType
                implicit val childSchemaCtx: SchemaForCtx[ChildType] =
                  SchemaForCtx.from[ChildType](
                    dctx.config,
                    derivedType = dctx.derivedType,
                    evaluatedConfig = dctx.evaluatedConfig
                  )
                val matchNameExpr: Expr[String] = computeAvroNameExpr[ChildType]
                Log.namedScope(s"Deriving decoder for enum case $childName: ${Type[ChildType].prettyPrint}") {
                  deriveDecoderRecursively[ChildType](using dctx.nest[ChildType](dctx.avroValue)).flatMap {
                    decodedExpr =>
                      dctx.getHelper[ChildType].map { helperOpt =>
                        val dispatch: (Expr[Any], Expr[A]) => Expr[A] = (valueExpr, elseExpr) => {
                          val thenExpr: Expr[A] = helperOpt match {
                            case Some(helper) => Expr.quote(Expr.splice(helper(valueExpr, dctx.config)).asInstanceOf[A])
                            case None         => Expr.quote(Expr.splice(decodedExpr).asInstanceOf[A])
                          }
                          Expr.quote {
                            val record = Expr.splice(valueExpr).asInstanceOf[GenericRecord]
                            if (Expr.splice(matchNameExpr) == record.getSchema.getName)
                              Expr.splice(thenExpr)
                            else
                              Expr.splice(elseExpr)
                          }
                        }
                        (matchNameExpr, dispatch)
                      }
                  }
                }
              }
              .map { dispatchers =>
                val knownNamesExpr: Expr[List[String]] =
                  dispatchers.toList.map(_._1).foldRight(Expr.quote(List.empty[String])) { (nameExpr, acc) =>
                    Expr.quote(Expr.splice(nameExpr) :: Expr.splice(acc))
                  }
                val errorExpr: Expr[A] = Expr.quote {
                  val record = Expr.splice(dctx.avroValue).asInstanceOf[GenericRecord]
                  AvroDerivationUtils.failedToMatchSubtype(
                    record.getSchema.getName,
                    Expr.splice(knownNamesExpr)
                  )
                }

                dispatchers.toList.foldRight(errorExpr) { case ((_, dispatch), elseExpr) =>
                  dispatch(dctx.avroValue, elseExpr)
                }
              }
          }
      }
    }
  }
}
