package hearth.kindlings.avroderivation

// $COVERAGE-OFF$
package object debug {

  implicit val logDerivationForAvroSchemaFor: AvroSchemaFor.LogDerivation =
    AvroSchemaFor.LogDerivation

  implicit val logDerivationForAvroEncoder: AvroEncoder.LogDerivation =
    AvroEncoder.LogDerivation

  implicit val logDerivationForAvroDecoder: AvroDecoder.LogDerivation =
    AvroDecoder.LogDerivation
}
// $COVERAGE-ON$
