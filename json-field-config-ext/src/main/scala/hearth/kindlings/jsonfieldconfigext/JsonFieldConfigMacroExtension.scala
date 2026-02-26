package hearth.kindlings.jsonfieldconfigext

import hearth.{MacroCommons, MacroExtension}
import hearth.std.StdExtensions

/** Abstract macro extension for registering JSON field configuration.
  *
  * Concrete implementations in library-specific modules (e.g., circe-derivation, jsoniter-derivation) extend this class
  * and register their configuration via `ctx.registerJsonFieldConfig(...)`.
  *
  * Extensions are discovered at macro expansion time via `java.util.ServiceLoader` when a consumer (e.g.,
  * tapir-schema-derivation) calls `Environment.loadMacroExtensions[JsonFieldConfigMacroExtension]`.
  *
  * The type parameter is `MacroCommons & StdExtensions` (not `& JsonFieldConfigSupport`) because `ClassTag` erasure for
  * intersection types only preserves the first component. The `extend` method performs a runtime check for
  * `JsonFieldConfigSupport` and silently skips contexts that don't support it.
  */
abstract class JsonFieldConfigMacroExtension extends MacroExtension[MacroCommons & StdExtensions] {

  final override def extend(ctx: MacroCommons & StdExtensions): Unit = ctx match {
    case _: JsonFieldConfigSupport =>
      extendJsonConfig(ctx.asInstanceOf[MacroCommons & StdExtensions & JsonFieldConfigSupport])
    case _ => () // silently skip — not a JSON-config-aware context
  }

  /** Called when the macro context supports JSON field configuration discovery.
    *
    * Implementations should:
    *   1. Try to summon their library's configuration from implicit scope
    *   1. If found, register a `JsonFieldConfigProvider` via `ctx.registerJsonFieldConfig(...)`
    *   1. If not found, do nothing (return without registering)
    */
  protected def extendJsonConfig(ctx: MacroCommons & StdExtensions & JsonFieldConfigSupport): Unit
}
