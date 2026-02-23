package hearth.kindlings.jsoniterjson.optics

import hearth.kindlings.jsoniterjson.Json

/** A traversal that can access multiple JSON values. */
trait JsonTraversal {
  def getAll(json: Json): Vector[Json]
  def modify(f: Json => Json)(json: Json): Json
  def set(value: Json)(json: Json): Json = modify(_ => value)(json)
}
