package hearth.kindlings.jsoniterjson.optics

import hearth.kindlings.jsoniterjson.Json

/** Chained field/index access for JSON navigation.
  *
  * Example: `JsonPath.root.field("users").index(0).field("name").get(json)`
  */
final class JsonPath private (private val optic: JsonOptic) {

  def field(name: String): JsonPath = new JsonPath(optic.andThen(JsonOptic.field(name)))
  def index(i: Int): JsonPath = new JsonPath(optic.andThen(JsonOptic.index(i)))

  def get(json: Json): Option[Json] = optic.get(json)
  def modify(f: Json => Json)(json: Json): Json = optic.modify(f)(json)
  def set(value: Json)(json: Json): Json = optic.set(value)(json)
}
object JsonPath {

  /** Starting point for building JSON paths. */
  val root: JsonPath = new JsonPath(new JsonOptic {
    def get(json: Json): Option[Json] = Some(json)
    def modify(f: Json => Json)(json: Json): Json = f(json)
  })
}
