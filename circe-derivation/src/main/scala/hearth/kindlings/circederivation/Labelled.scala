package hearth.kindlings.circederivation

/** A value supplied later for the field named `Name` (a `String` literal type) of an incomplete decoder, like
  * circe-generic's shapeless `FieldType[Witness.`'j`.T, V]` in `deriveFor[FieldType[..] => C].incomplete`:
  * {{{
  * case class Qux(i: Int, a: String, j: Int)
  * val decoder: Decoder[Labelled["j", Int] => Qux] = KindlingsDecoder.incomplete[Labelled["j", Int] => Qux]
  * decoder.decodeJson(json).map(_(Labelled["j", Int](3))) // `j` is supplied, `i` and `a` are decoded
  * }}}
  * Unlike a plain `Int` parameter (which stands for the first remaining `Int` field), it selects the field by its Scala
  * name, whatever its position.
  */
final case class Labelled[Name, V](value: V)
