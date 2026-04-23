package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.AvroConfig
import hearth.kindlings.avroderivation.annotations.avroNamespace
import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsSingletonRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsSingletonRule extends SchemaDerivationRule("handle as singleton when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            implicit val SchemaT: Type[Schema] = SfTypes.Schema
            implicit val StringT: Type[String] = SfTypes.String
            implicit val AvroConfigT: Type[AvroConfig] = SfTypes.AvroConfig
            implicit val avroNamespaceT: Type[avroNamespace] = SfTypes.AvroNamespace
            val typeName = Type[A].shortName
            val classNamespace: Option[String] = getTypeAnnotationStringArg[avroNamespace, A]
            val namespaceExpr: Expr[String] = classNamespace match {
              case Some(ns) => Expr(ns)
              case None     => Expr.quote(Expr.splice(sfctx.config).namespace.getOrElse(""))
            }
            val schemaExpr = Expr.quote {
              AvroDerivationUtils.createRecord(
                Expr.splice(Expr(typeName)),
                Expr.splice(namespaceExpr),
                java.util.Collections.emptyList[Schema.Field]()
              )
            }
            for {
              _ <- sfctx.setCachedSchema[A](schemaExpr)
              result <- sfctx.getCachedSchema[A].flatMap {
                case Some(cachedSchema) => MIO.pure(Rule.matched(cachedSchema))
                case None               => MIO.pure(Rule.yielded(s"Failed to cache schema for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
