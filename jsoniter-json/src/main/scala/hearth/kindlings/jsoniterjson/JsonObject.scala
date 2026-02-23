package hearth.kindlings.jsoniterjson

/** Ordered collection of key-value pairs representing a JSON object.
  *
  * Preserves insertion order. Duplicate keys are allowed but only the last value for a given key is returned by
  * `apply`.
  */
final case class JsonObject(fields: Vector[(String, Json)]) {

  def apply(key: String): Option[Json] = {
    var i = fields.length - 1
    while (i >= 0) {
      if (fields(i)._1 == key) return Some(fields(i)._2)
      i -= 1
    }
    None
  }

  def add(key: String, value: Json): JsonObject = JsonObject(fields :+ (key -> value))

  def remove(key: String): JsonObject = JsonObject(fields.filterNot(_._1 == key))

  def keys: Vector[String] = fields.map(_._1)

  def values: Vector[Json] = fields.map(_._2)

  def size: Int = fields.size

  def isEmpty: Boolean = fields.isEmpty

  def nonEmpty: Boolean = fields.nonEmpty

  def toMap: Map[String, Json] = fields.toMap

  def mapValues(f: Json => Json): JsonObject = JsonObject(fields.map { case (k, v) => (k, f(v)) })
}
object JsonObject {

  val empty: JsonObject = JsonObject(Vector.empty)

  def apply(entries: (String, Json)*): JsonObject = JsonObject(entries.toVector)

  def fromIterable(entries: Iterable[(String, Json)]): JsonObject = JsonObject(entries.toVector)
}
