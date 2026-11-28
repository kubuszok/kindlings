package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Overrides the decimal logical type configuration for a `BigDecimal` field, taking precedence over the global
  * `DecimalConfig` in `AvroConfig`. Without this annotation (or `DecimalConfig`), `BigDecimal` maps to STRING.
  *
  * A value with more decimal places than `scale` is rounded with `AvroConfig.roundingMode` (default `UNNECESSARY`:
  * rejected), or with the annotation's own `roundingMode` when it names one.
  *
  * Example: `case class Invoice(@avroScalePrecision(10, 2) amount: BigDecimal)`
  */
final class avroScalePrecision(precision: Int, scale: Int) extends StaticAnnotation {

  /** A decimal of `precision` and `scale` rounded with `roundingMode` (a constant such as
    * `java.math.RoundingMode.HALF_UP`), whatever `AvroConfig.roundingMode` is.
    *
    * Example: `case class Invoice(@avroScalePrecision(10, 2, java.math.RoundingMode.HALF_EVEN) amount: BigDecimal)`
    */
  def this(precision: Int, scale: Int, roundingMode: java.math.RoundingMode) = this(precision, scale)
}
