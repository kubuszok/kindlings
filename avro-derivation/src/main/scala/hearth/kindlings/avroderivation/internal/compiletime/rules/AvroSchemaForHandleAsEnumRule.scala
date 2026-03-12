package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.annotations.{avroEnumDefault, avroSortPriority}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsEnumRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsEnumRule extends SchemaDerivationRule("handle as enum when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              schemaExpr <- deriveEnumSchema[A](enumm)
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveEnumSchema[A: SchemaForCtx](
        enumm: Enum[A]
    ): MIO[Expr[Schema]] = {
      implicit val SchemaT: Type[Schema] = SfTypes.Schema
      implicit val avroSortPriorityT: Type[avroSortPriority] = SfTypes.AvroSortPriority
      implicit val avroEnumDefaultT: Type[avroEnumDefault] = SfTypes.AvroEnumDefault
      implicit val StringT: Type[String] = SfTypes.String

      val childrenList = enumm.directChildren.toList
      val typeName = Type[A].shortName
      val enumDefault: Option[String] = getTypeAnnotationStringArg[avroEnumDefault, A]

      NonEmptyList.fromList(childrenList) match {
        case None =>
          val err = SchemaDerivationError.NoChildrenInSealedTrait(Type[A].prettyPrint)
          Log.error(err.message) >> MIO.fail(err)

        case Some(children) =>
          val allCaseObjects = Type[A].isEnumeration || Type[A].isJavaEnum ||
            children.toList.forall { case (_, child) =>
              SingletonValue.unapply(child.Underlying).isDefined
            }

          if (allCaseObjects) {
            // Pure enum of case objects → Avro ENUM schema
            // Sort by @avroSortPriority
            val symbolsWithPriority = children.toList.map { case (name, child) =>
              import child.Underlying as ChildType
              val priority = getTypeAnnotationIntArg[avroSortPriority, ChildType]
              (name, priority.getOrElse(0))
            }
            val sortedSymbolNames = symbolsWithPriority.sortBy(-_._2).map(_._1)
            val symbolsListExpr = sortedSymbolNames.foldRight(
              Expr.quote(List.empty[String])
            ) { (name, acc) =>
              Expr.quote {
                Expr.splice(sfctx.config).transformConstructorNames(Expr.splice(Expr(name))) ::
                  Expr.splice(acc)
              }
            }
            val createEnumExpr: Expr[Schema] = enumDefault match {
              case Some(default) =>
                Expr.quote {
                  val symbols = Expr.splice(symbolsListExpr)
                  val javaSymbols = new java.util.ArrayList[String](symbols.size)
                  symbols.foreach(javaSymbols.add)
                  AvroDerivationUtils.createEnumWithDefault(
                    Expr.splice(Expr(typeName)),
                    Expr.splice(sfctx.config).namespace.getOrElse(""),
                    javaSymbols,
                    Expr.splice(Expr(default))
                  )
                }
              case None =>
                Expr.quote {
                  val symbols = Expr.splice(symbolsListExpr)
                  val javaSymbols = new java.util.ArrayList[String](symbols.size)
                  symbols.foreach(javaSymbols.add)
                  AvroDerivationUtils.createEnum(
                    Expr.splice(Expr(typeName)),
                    Expr.splice(sfctx.config).namespace.getOrElse(""),
                    javaSymbols
                  )
                }
            }
            MIO.pure(createEnumExpr)
          } else {
            // Mixed sealed trait → Avro UNION of record schemas
            // Sort children by @avroSortPriority
            val childrenWithPriority = children.toList.map { case (name, child) =>
              import child.Underlying as ChildType
              val priority = getTypeAnnotationIntArg[avroSortPriority, ChildType]
              (name, child, priority.getOrElse(0))
            }
            val sortedList = childrenWithPriority.sortBy(-_._3).map(t => (t._1, t._2))
            val sortedChildren = NonEmptyList(sortedList.head, sortedList.tail)
            sortedChildren
              .parTraverse { case (_, child) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving schema for enum case ${Type[ChildType].prettyPrint}") {
                  deriveSchemaRecursively[ChildType](using sfctx.nest[ChildType])
                }
              }
              .map { childSchemas =>
                val schemasListExpr = childSchemas.toList.foldRight(
                  Expr.quote(List.empty[Schema])
                ) { (childSchema, acc) =>
                  Expr.quote(Expr.splice(childSchema) :: Expr.splice(acc))
                }
                Expr.quote {
                  val schemas = Expr.splice(schemasListExpr)
                  val javaSchemas = new java.util.ArrayList[Schema](schemas.size)
                  schemas.foreach(javaSchemas.add)
                  Schema.createUnion(javaSchemas)
                }
              }
          }
      }
    }
  }
}
