package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs
import sttp.tapir.Schema

/** Tuples encoded by the JSON library as arrays (`[1,"a"]`, see `JsonSchemaConfig.tuplesAsArrays`). Otherwise tuples
  * are handled as case classes (objects with `_1`, `_2`, ... fields).
  */
trait SchemaHandleAsTupleRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  object SchemaHandleAsTupleRule extends SchemaDerivationRule("handle as tuple when possible") {

    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Expr[Schema[A]]]] =
      if (!Type[A].isTuple || Type[A].isNamedTuple)
        MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
      else if (!sctx.jsonCfg.tuplesAsArrays)
        MIO.pure(Rule.yielded(s"Tuples are not JSON arrays with ${sctx.jsonCfg.libraryName}"))
      else
        CaseClass.parse[A].toEither match {
          case Right(cc) =>
            Log.info(s"Deriving Schema for tuple ${Type[A].prettyPrint} as an array") >>
              deriveTupleSchema[A](cc).map(Rule.matched)
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
  }
}
