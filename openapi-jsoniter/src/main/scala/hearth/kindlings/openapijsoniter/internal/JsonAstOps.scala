package hearth.kindlings.openapijsoniter
package internal

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, WriterConfig}
import hearth.kindlings.jsoniterjson.{Json, JsonObject}
import hearth.kindlings.jsoniterjson.codec.JsonCodec.jsonValueCodec

/** Operations on the kindlings [[Json]] AST that mirror the circe combinators used by the sttp-apispec codecs
  * (`dropNulls`, `dropNullsExpandExtensions`, `dropNullValues`, `deepMerge`, `spaces2`, JSON parse/print).
  *
  * Keeping these here (rather than in `jsoniter-json`) confines OpenAPI-specific encoding rules to this module.
  */
private[openapijsoniter] object JsonAstOps {

  /** Pretty-printed JSON with two-space indentation, matching circe's `Json#spaces2` (used by the `ExtensionValue`
    * decoder to round-trip arbitrary JSON through a `String`).
    */
  private val spaces2Config: WriterConfig = WriterConfig.withIndentionStep(2)

  def spaces2(json: Json): String = writeToString(json, spaces2Config)(jsonValueCodec)

  def printCompact(json: Json): String = writeToString(json)(jsonValueCodec)

  def parse(s: String): Either[Throwable, Json] =
    try Right(readFromString[Json](s)(jsonValueCodec))
    catch { case t: Throwable => Left(t) }

  /** Removes all top-level fields whose value is JSON `null`. Mirrors circe's `dropNullValues` /
    * `JsonObject.filter(!_._2.isNull)`.
    */
  def dropNulls(obj: JsonObject): JsonObject =
    JsonObject(obj.fields.filterNot { case (_, v) => v.isNull })

  def dropNullValues(json: Json): Json = json match {
    case Json.Obj(obj) => Json.Obj(dropNulls(obj))
    case other         => other
  }

  def filterKeys(obj: JsonObject, p: String => Boolean): JsonObject =
    JsonObject(obj.fields.filter { case (k, _) => p(k) })

  /** Hoists the `extensions` object's entries to the parent object level, preserving insertion order and letting
    * extension entries override same-named non-extension fields. Mirrors `JsonSchemaCirceEncoders.expandExtensions`.
    */
  def expandExtensions(obj: JsonObject): JsonObject = {
    val withoutExt = filterKeys(obj, _ != "extensions")
    obj("extensions").flatMap(_.asObject) match {
      case None            => withoutExt
      case Some(extObject) =>
        val allKeys = (withoutExt.keys ++ extObject.keys).distinct
        allKeys.foldLeft(JsonObject.empty) { (acc, key) =>
          extObject(key).orElse(withoutExt(key)) match {
            case Some(value) => acc.add(key, value)
            case None        => acc
          }
        }
    }
  }

  def dropNullsExpandExtensions(obj: JsonObject): JsonObject =
    expandExtensions(dropNulls(obj))

  /** Recursive deep merge mirroring circe's `Json#deepMerge` exactly, including key ordering: the accumulator starts as
    * the right-hand object and folds over the left-hand object's keys, so right-hand-only keys appear first and
    * conflicting keys recurse with `left.value.deepMerge(right.value)`.
    */
  def deepMerge(left: Json, right: Json): Json = (left, right) match {
    case (Json.Obj(lhs), Json.Obj(rhs)) =>
      val merged = lhs.fields.foldLeft(rhs) { case (acc, (key, lv)) =>
        acc(key) match {
          case None     => acc.add(key, lv)
          case Some(rv) => setKey(acc, key, deepMerge(lv, rv))
        }
      }
      Json.Obj(merged)
    case _ => right
  }

  /** Sets `key` to `value`, replacing the existing entry in place (preserving its position) or appending. */
  private def setKey(obj: JsonObject, key: String, value: Json): JsonObject =
    if (obj(key).isDefined)
      JsonObject(obj.fields.map { case (k, v) => if (k == key) (k, value) else (k, v) })
    else obj.add(key, value)
}
