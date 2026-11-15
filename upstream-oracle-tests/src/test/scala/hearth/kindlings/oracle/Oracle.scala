package hearth.kindlings.oracle

import sttp.apispec.Schema as ASchema
import sttp.tapir.Schema
import sttp.tapir.docs.apispec.schema.TapirSchemaToJsonSchema

/** Helpers for differential tests: a structural diff of two values (case classes, maps, sequences) that reports every
  * differing path, and the conversion of tapir schemas to the JSON Schema users see in OpenAPI documents.
  */
object Oracle {

  /** The JSON Schema tapir's docs interpreter renders for `schema` (with `$defs` for named schemas). */
  def jsonSchema(schema: Schema[?]): ASchema =
    TapirSchemaToJsonSchema(schema, markOptionsAsNullable = false, failOnDuplicateSchemaName = false)

  /** Every path at which `kindlings` and `upstream` differ, rendered as `path: kindlings=... upstream=...`. */
  def diff(kindlings: Any, upstream: Any): List[String] = diffAt(kindlings, upstream, "$")

  private def diffAt(k: Any, u: Any, path: String): List[String] = (k, u) match {
    case _ if k == u                                          => Nil
    case (km: collection.Map[?, ?], um: collection.Map[?, ?]) =>
      val kmA = km.asInstanceOf[collection.Map[Any, Any]]
      val umA = um.asInstanceOf[collection.Map[Any, Any]]
      val keys = (kmA.keys.toList ++ umA.keys.toList).distinct
      val keyOrder =
        if (kmA.keys.toList == umA.keys.toList || kmA.keySet != umA.keySet) Nil
        else List(s"$path: key order kindlings=${kmA.keys.mkString(",")} upstream=${umA.keys.mkString(",")}")
      keyOrder ++ keys.flatMap { key =>
        (kmA.get(key), umA.get(key)) match {
          case (Some(a), Some(b)) => diffAt(a, b, s"$path[$key]")
          case (Some(a), None)    => List(s"$path[$key]: only in kindlings: $a")
          case (None, Some(b))    => List(s"$path[$key]: only in upstream: $b")
          case (None, None)       => Nil
        }
      }
    case (ks: collection.Seq[?], us: collection.Seq[?]) =>
      if (ks.size != us.size) List(s"$path: kindlings=$ks upstream=$us")
      else ks.zip(us).zipWithIndex.toList.flatMap { case ((a, b), i) => diffAt(a, b, s"$path[$i]") }
    case (kp: Product, up: Product) if kp.getClass == up.getClass && kp.productArity > 0 =>
      (0 until kp.productArity).toList.flatMap { i =>
        diffAt(kp.productElement(i), up.productElement(i), s"$path.${kp.productElementName(i)}")
      }
    case _ => List(s"$path: kindlings=$k upstream=$u")
  }
}
