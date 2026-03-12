package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils
import org.apache.avro.Schema

trait AvroSchemaForHandleAsLiteralTypeRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object AvroSchemaForHandleAsLiteralTypeRule extends SchemaDerivationRule("handle as literal type when possible") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        implicit val SchemaT: Type[Schema] = SfTypes.Schema
        extractLiteralSchema[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralSchema[A: SchemaForCtx](implicit SchemaT: Type[Schema]): Option[Expr[Schema]] =
      Type.StringCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.stringSchema)
      } orElse Type.IntCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.LongCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.longSchema)
      } orElse Type.DoubleCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.doubleSchema)
      } orElse Type.FloatCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.floatSchema)
      } orElse Type.BooleanCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.booleanSchema)
      } orElse Type.ShortCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.ByteCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.intSchema)
      } orElse Type.CharCodec.fromType(Type[A]).map { _ =>
        Expr.quote(AvroDerivationUtils.stringSchema)
      }
  }
}
