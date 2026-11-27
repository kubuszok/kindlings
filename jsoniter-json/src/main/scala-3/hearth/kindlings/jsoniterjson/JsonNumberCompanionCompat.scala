package hearth.kindlings.jsoniterjson

/** What the companion of the former `case class JsonNumber(value: String)` had on Scala 3 (binary compatibility). */
private[jsoniterjson] trait JsonNumberCompanionCompat extends scala.deriving.Mirror.Product {

  type MirroredMonoType = JsonNumber

  private[jsoniterjson] def unsafeFromJsonText(text: String): JsonNumber

  def unapply(number: JsonNumber): JsonNumber = number

  def fromProduct(product: Product): JsonNumber = unsafeFromJsonText(product.productElement(0).asInstanceOf[String])
}
