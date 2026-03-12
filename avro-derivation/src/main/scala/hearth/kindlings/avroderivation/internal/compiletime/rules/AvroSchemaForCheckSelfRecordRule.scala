package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.apache.avro.Schema

trait AvroSchemaForCheckSelfRecordRuleImpl {
  this: SchemaForMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Returns the self-record reference for recursive types. During case class field derivation, if a field type matches
    * the type currently being derived, this rule returns the eagerly-initialized empty record Schema. The main lazy val
    * body will call setFields on it after field derivation completes.
    */
  object AvroSchemaForCheckSelfRecordRule extends SchemaDerivationRule("use self-record for recursive type") {

    def apply[A: SchemaForCtx]: MIO[Rule.Applicability[Expr[Schema]]] =
      sfctx.getSelfRecordSchema[A].flatMap {
        case Some(selfRecord) =>
          Log.info(s"Found self-record for ${Type[A].prettyPrint} (recursive reference)") >>
            MIO.pure(Rule.matched(selfRecord))
        case None =>
          MIO.pure(Rule.yielded(s"No self-record for ${Type[A].prettyPrint}"))
      }
  }
}
