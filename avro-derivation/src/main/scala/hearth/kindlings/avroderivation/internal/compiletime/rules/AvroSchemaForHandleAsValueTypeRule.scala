package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

import org.apache.avro.Schema

trait AvroSchemaForHandleAsValueTypeRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsValueTypeRule extends SchemaDerivationRule("handle as value type when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        if (Type[A].isNamedTuple)
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is a named tuple, not a value type"))
        else
          Type[A] match {
            case IsValueType(isValueType) =>
              import isValueType.Underlying as Inner
              fixedValueTypeSize[A, Inner] match {
                // as avro4s: a value class of bytes or a String annotated with @avroFixed is a FIXED type named after
                // the class
                case Some(size) =>
                  implicit val SchemaT: Type[Schema] = SfTypes.Schema
                  val nameExpr = computeAvroNameExpr[A]
                  val namespaceExpr = computeNamespaceExpr[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    AvroDerivationUtils.createFixed(
                      Expr.splice(nameExpr),
                      Expr.splice(namespaceExpr),
                      Expr.splice(Expr(size))
                    )
                  }))
                case None =>
                  for {
                    innerResult <- deriveSchemaRecursively[Inner](using sfctx.nest[Inner])
                  } yield Rule.matched(innerResult)
              }

            case _ =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
          }
      }
  }
}
