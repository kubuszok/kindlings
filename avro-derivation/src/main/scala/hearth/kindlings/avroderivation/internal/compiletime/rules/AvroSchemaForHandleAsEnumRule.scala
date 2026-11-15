package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.AvroConfig
import hearth.kindlings.avroderivation.annotations.{avroEnumDefault, avroNamespace, avroSortPriority}
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema
import hearth.kindlings.derivation.compiletime.StableChildrenOrder

trait AvroSchemaForHandleAsEnumRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsEnumRule extends SchemaDerivationRule("handle as enum when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
        Enum.parse[A].toEither match {
          case Right(enumm) =>
            for {
              schemaExpr <- deriveEnumSchema[A](enumm)
              // A recursive sealed trait (`Branch(left: Node, ...) extends Node`) is met again while deriving its
              // subtypes: a union cannot be referenced by name, so that inner occurrence was derived (and cached) as
              // a union of the subtypes' (self-)records. The outer union - built from the completed subtype schemas -
              // is then returned as it is: caching it again would define the same key twice.
              alreadyCached <- sfctx.getCachedSchema[A]
              result <- alreadyCached match {
                case Some(_) => MIO.pure(Rule.matched(schemaExpr))
                case None    =>
                  // just cached, so present (as in the case class rule)
                  sfctx
                    .setCachedSchema[A](schemaExpr) >> sfctx.getCachedSchema[A].map(cached => Rule.matched(cached.get))
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
      implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
      implicit val StringT: Type[String] = SfTypes.String
      implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig

      val childrenList = enumm.directChildren.toList
      val typeNameExpr = computeAvroNameExpr[A]
      val enumDefault: Option[String] = getTypeAnnotationStringArg[avroEnumDefault, A]

      // Build the namespace expression: field-level @avroNamespace > type-level @avroNamespace > config.namespace > package name > ""
      val namespaceExpr: Expr[String] = computeNamespaceExpr[A]

      // After consuming namespaceOverride for this enum, clear it so it does not leak into nested child derivations.
      val cleanCtx: SchemaForCtx[A] = sfctx[A].copy(namespaceOverride = None)

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
            // Sealed traits' case objects: on Scala 3 in declaration order (as avro4s 5, Mirror order); on Scala 2 in
            // Hearth's `ChildrenOrder.ByName` (avro4s 4 sorts by full name - the same order for case objects declared
            // in one scope), which keeps the order the same in every build (see `StableChildrenOrder`).
            val childrenInOrder =
              StableChildrenOrder.inStableOrder(AvroSchemaForHandleAsEnumRuleImpl.this)(children.toList)
            val symbolsWithPriority = childrenInOrder.map { case (name, child) =>
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
                    Expr.splice(typeNameExpr),
                    Expr.splice(namespaceExpr),
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
                    Expr.splice(typeNameExpr),
                    Expr.splice(namespaceExpr),
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
            val childrenNel = NonEmptyList(childrenWithPriority.head, childrenWithPriority.tail)
            childrenNel
              .parTraverse { case (_, child, priority) =>
                import child.Underlying as ChildType
                Log.namedScope(s"Deriving schema for enum case ${Type[ChildType].prettyPrint}") {
                  deriveSchemaRecursively[ChildType](using cleanCtx.nest[ChildType]).map(_ -> priority)
                }
              }
              .map { childSchemas =>
                val schemasListExpr = childSchemas.toList.foldRight(
                  Expr.quote(List.empty[Schema])
                ) { case ((childSchema, _), acc) =>
                  Expr.quote(Expr.splice(childSchema) :: Expr.splice(acc))
                }
                val prioritiesListExpr = childSchemas.toList.foldRight(
                  Expr.quote(List.empty[Int])
                ) { case ((_, priority), acc) =>
                  Expr.quote(Expr.splice(Expr(priority)) :: Expr.splice(acc))
                }
                // as avro4s: by descending @avroSortPriority, then by the subtypes' full names (known only at runtime,
                // since they depend on the config's namespace and on @avroName/@avroNamespace)
                Expr.quote {
                  AvroDerivationUtils.createSortedUnion(Expr.splice(schemasListExpr), Expr.splice(prioritiesListExpr))
                }
              }
          }
      }
    }
  }
}
