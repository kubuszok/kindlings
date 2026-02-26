package hearth.kindlings.avroderivation.annotations

import scala.annotation.StaticAnnotation

/** Marks a record type as an Avro error record. Error records are identical to regular records
  * except that `Schema.isError` returns `true`, which is used by Avro RPC protocols.
  *
  * Example: `@avroError case class MyError(code: Int, message: String)`
  */
final class avroError extends StaticAnnotation
