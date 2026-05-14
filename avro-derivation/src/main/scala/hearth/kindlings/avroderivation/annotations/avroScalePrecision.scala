package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Overrides the decimal logical type configuration for a `BigDecimal` field, taking precedence over the global
  * `DecimalConfig` in `AvroConfig`. Without this annotation (or `DecimalConfig`), `BigDecimal` maps to STRING.
  *
  * Example: `case class Invoice(@avroScalePrecision(10, 2) amount: BigDecimal)`
  */
final class avroScalePrecision(precision: Int, scale: Int) extends StaticAnnotation
