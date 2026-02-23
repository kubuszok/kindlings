package hearth.kindlings.jsoniterjson.optics

import hearth.kindlings.jsoniterjson.{Json, JsonObject}

/** Minimal lens/prism-like accessor for JSON values. */
trait JsonOptic { self =>

  def get(json: Json): Option[Json]
  def modify(f: Json => Json)(json: Json): Json
  def set(value: Json)(json: Json): Json = modify(_ => value)(json)

  def andThen(that: JsonOptic): JsonOptic = new JsonOptic {
    def get(json: Json): Option[Json] = self.get(json).flatMap(that.get)
    def modify(f: Json => Json)(json: Json): Json = self.modify(that.modify(f))(json)
  }
}
object JsonOptic {

  /** Access a field in a JSON object. */
  def field(name: String): JsonOptic = new JsonOptic {
    def get(json: Json): Option[Json] = json.asObject.flatMap(_(name))
    def modify(f: Json => Json)(json: Json): Json = json match {
      case Json.Obj(obj) =>
        val newFields = obj.fields.map { case (k, v) =>
          if (k == name) (k, f(v)) else (k, v)
        }
        Json.Obj(JsonObject(newFields))
      case other => other
    }
  }

  /** Access an element in a JSON array by index. */
  def index(i: Int): JsonOptic = new JsonOptic {
    def get(json: Json): Option[Json] = json.asArray.flatMap(vs => if (i >= 0 && i < vs.size) Some(vs(i)) else None)
    def modify(f: Json => Json)(json: Json): Json = json match {
      case Json.Arr(vs) if i >= 0 && i < vs.size => Json.Arr(vs.updated(i, f(vs(i))))
      case other                                 => other
    }
  }

  /** Traverse all elements of a JSON array. */
  val each: JsonTraversal = new JsonTraversal {
    def getAll(json: Json): Vector[Json] = json.asArray.getOrElse(Vector.empty)
    def modify(f: Json => Json)(json: Json): Json = json match {
      case Json.Arr(vs) => Json.Arr(vs.map(f))
      case other        => other
    }
  }
}
