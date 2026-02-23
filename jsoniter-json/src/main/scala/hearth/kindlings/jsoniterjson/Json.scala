package hearth.kindlings.jsoniterjson

/** Minimal JSON AST providing the same structural capabilities as jsoniter-scala-circe but without Circe/Cats
  * dependencies.
  */
sealed trait Json extends Product with Serializable {

  def isNull: Boolean = this.isInstanceOf[Json.Null.type]
  def isBoolean: Boolean = this.isInstanceOf[Json.Bool]
  def isNumber: Boolean = this.isInstanceOf[Json.Num]
  def isString: Boolean = this.isInstanceOf[Json.Str]
  def isArray: Boolean = this.isInstanceOf[Json.Arr]
  def isObject: Boolean = this.isInstanceOf[Json.Obj]

  def asBoolean: Option[Boolean] = this match {
    case Json.Bool(b) => Some(b)
    case _            => None
  }

  def asNumber: Option[JsonNumber] = this match {
    case Json.Num(n) => Some(n)
    case _           => None
  }

  def asString: Option[String] = this match {
    case Json.Str(s) => Some(s)
    case _           => None
  }

  def asArray: Option[Vector[Json]] = this match {
    case Json.Arr(vs) => Some(vs)
    case _            => None
  }

  def asObject: Option[JsonObject] = this match {
    case Json.Obj(obj) => Some(obj)
    case _             => None
  }

  def fold[A](
      onNull: => A,
      onBoolean: Boolean => A,
      onNumber: JsonNumber => A,
      onString: String => A,
      onArray: Vector[Json] => A,
      onObject: JsonObject => A
  ): A = this match {
    case Json.Null    => onNull
    case Json.Bool(b) => onBoolean(b)
    case Json.Num(n)  => onNumber(n)
    case Json.Str(s)  => onString(s)
    case Json.Arr(vs) => onArray(vs)
    case Json.Obj(o)  => onObject(o)
  }
}
object Json {
  case object Null extends Json
  final case class Bool(value: Boolean) extends Json
  final case class Num(value: JsonNumber) extends Json
  final case class Str(value: String) extends Json
  final case class Arr(values: Vector[Json]) extends Json
  final case class Obj(fields: JsonObject) extends Json

  val True: Json = Bool(true)
  val False: Json = Bool(false)

  def fromBoolean(b: Boolean): Json = Bool(b)
  def fromInt(n: Int): Json = Num(JsonNumber.fromInt(n))
  def fromLong(n: Long): Json = Num(JsonNumber.fromLong(n))
  def fromDouble(n: Double): Option[Json] = JsonNumber.fromDouble(n).map(Num(_))
  def fromFloat(n: Float): Option[Json] = JsonNumber.fromFloat(n).map(Num(_))
  def fromBigDecimal(n: BigDecimal): Json = Num(JsonNumber.fromBigDecimal(n))
  def fromBigInt(n: BigInt): Json = Num(JsonNumber.fromBigInt(n))
  def fromString(s: String): Json = Str(s)
  def fromValues(vs: Iterable[Json]): Json = Arr(vs.toVector)
  def fromJsonObject(obj: JsonObject): Json = Obj(obj)

  def arr(values: Json*): Json = Arr(values.toVector)
  def obj(fields: (String, Json)*): Json = Obj(JsonObject(fields.toVector))
}
