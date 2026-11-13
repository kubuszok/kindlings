package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import hearth.kindlings.tapirschemaderivation.internal.runtime.TapirSchemaUtils
import sttp.tapir.Schema
import sttp.tapir.Schema.SName

/** The schema of a sum whose nested sealed hierarchies the JSON library flattens (see [[SchemaSubtypeFlattening]]). */
trait SchemaFlattenedEnumImpl extends SchemaSubtypeFlattening {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  protected def deriveFlattenedEnumSchema[A: SchemaCtx](e: Enum[A])(
      sNameExpr: Expr[SName],
      typeAnnsExpr: Expr[List[Any]]
  ): MIO[Expr[Schema[A]]] = {
    implicit val SchemaA: Type[Schema[A]] = TsTypes.TapirSchemaOf[A]
    implicit val Utils: Type[TapirSchemaUtils.type] = TsTypes.SchemaTypeUtils
    implicit val stringT: Type[String] = TsTypes.StringType

    val discriminatorExpr: Expr[Option[String]] = sctx.jsonCfg.discriminatorFieldName
    val enumAsStringsExpr: Expr[Boolean] = sctx.jsonCfg.enumAsStrings
    val wrapNonObjects: Boolean = sctx.jsonCfg.wrapsNonObjectSubtypesWithDiscriminator
    val wrappedExpr: Expr[Boolean] = sctx.jsonCfg.wrapsSubtypesWithoutDiscriminator

    def schemaOf(tpe: ??): MIO[Expr[Schema[Any]]] = {
      import tpe.Underlying as T
      implicit val SchemaT: Type[Schema[T]] = TsTypes.TapirSchemaOf[T]
      deriveSchemaRecursively[T](using sctx.nest[T]).map { schema =>
        Expr.quote(Expr.splice(schema).asInstanceOf[Schema[Any]])
      }
    }

    // The subtypes' schemas and names, an intermediate with its own instance as one subtype or (when its instance is a
    // derived sum instance, known at runtime) as its own subtypes.
    def segments(entries: List[SubtypeEntry]): MIO[(Expr[List[Schema[Any]]], Expr[List[String]])] =
      entries.foldRight(MIO.pure((Expr.quote(Nil: List[Schema[Any]]), Expr.quote(Nil: List[String])))) {
        case (SubtypeEntry.Leaf(name, tpe), accMIO) =>
          for {
            acc <- accMIO
            schema <- schemaOf(tpe)
          } yield {
            val nameExpr = sctx.jsonCfg.resolveConstructorName(name)
            val accSchemas = acc._1
            val accNames = acc._2
            (
              Expr.quote(Expr.splice(schema) :: Expr.splice(accSchemas)),
              Expr.quote(Expr.splice(nameExpr) :: Expr.splice(accNames))
            )
          }
        case (SubtypeEntry.Intermediate(name, tpe, flattened, inner), accMIO) =>
          for {
            acc <- accMIO
            innerSegments <- segments(inner)
            schema <- schemaOf(tpe)
          } yield {
            val nameExpr = sctx.jsonCfg.resolveConstructorName(name)
            val accSchemas = acc._1
            val accNames = acc._2
            val innerSchemas = innerSegments._1
            val innerNames = innerSegments._2
            (
              Expr.quote {
                if (Expr.splice(flattened)) Expr.splice(innerSchemas) ::: Expr.splice(accSchemas)
                else Expr.splice(schema) :: Expr.splice(accSchemas)
              },
              Expr.quote {
                if (Expr.splice(flattened)) Expr.splice(innerNames) ::: Expr.splice(accNames)
                else Expr.splice(nameExpr) :: Expr.splice(accNames)
              }
            )
          }
      }

    // With string encoding, the leaves are written by name, whatever the intermediates' instances (like circe's
    // ConfiguredEnumCodec).
    val leaves = subtypeLeaves(e)
    val leafSingletons: Option[Expr[List[A]]] =
      leaves.foldRight(Option(Expr.quote(Nil: List[A]))) { case ((_, leaf), accOpt) =>
        import leaf.Underlying as L
        for {
          acc <- accOpt
          sv <- SingletonValue.parse[L].toEither.toOption
        } yield Expr.quote(Expr.splice(sv.singletonExpr).asInstanceOf[A] :: Expr.splice(acc))
      }
    val leafNamesExpr: Expr[List[String]] = leaves.foldRight(Expr.quote(Nil: List[String])) { case ((name, _), acc) =>
      val nameExpr = sctx.jsonCfg.resolveConstructorName(name)
      Expr.quote(Expr.splice(nameExpr) :: Expr.splice(acc))
    }

    segments(flattenedSubtypes(e, sctx.jsonCfg)).flatMap { segs =>
      val subtypesListExpr = segs._1
      val resolvedNamesListExpr = segs._2
      val coproductExpr: Expr[Schema[A]] = Expr.quote {
        TapirSchemaUtils.coproductSchema[A](
          Expr.splice(sNameExpr),
          Expr.splice(subtypesListExpr),
          Expr.splice(discriminatorExpr),
          Expr.splice(resolvedNamesListExpr),
          Expr.splice(Expr(wrapNonObjects)),
          Expr.splice(wrappedExpr)
        )
      }
      val rawSchemaExpr: Expr[Schema[A]] = leafSingletons match {
        case Some(singletonValuesExpr) =>
          Expr.quote {
            if (Expr.splice(enumAsStringsExpr))
              TapirSchemaUtils.stringEnumSchema[A](
                Expr.splice(sNameExpr),
                Expr.splice(singletonValuesExpr),
                Expr.splice(leafNamesExpr)
              )
            else Expr.splice(coproductExpr)
          }
        case None => coproductExpr
      }
      setCachedAndGet[A](
        sctx.cache,
        Expr.quote(TapirSchemaUtils.enrichSchema[A](Expr.splice(rawSchemaExpr), Expr.splice(typeAnnsExpr)))
      )
    }
  }
}
