package hearth.kindlings.jsoniterjson

/** What the companion of the former `case class JsonNumber(value: String)` had on Scala 2 (binary compatibility). */
private[jsoniterjson] trait JsonNumberCompanionCompat {

  private[jsoniterjson] def unsafeFromJsonText(text: String): JsonNumber

  private[jsoniterjson] def apply(value: String): JsonNumber = unsafeFromJsonText(value)

  def unapply(number: JsonNumber): Option[String] = Some(number.value)
}
