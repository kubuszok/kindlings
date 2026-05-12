package hearth.kindlings.benchmarks

import io.circe.{Json, JsonObject}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter}
import scala.util.hashing.MurmurHash3

object HandWrittenImpls {

  // --- Show implementations ---

  def showSimpleCC(a: SimpleCC): String =
    "SimpleCC(name = " + a.name + ", age = " + a.age.toString + ", active = " + a.active.toString + ")"

  def showAddress(a: Address): String =
    "Address(street = " + a.street + ", city = " + a.city + ", zip = " + a.zip + ", country = " + a.country + ")"

  def showPerson(a: Person): String = {
    val sb = new StringBuilder("Person(name = ")
    sb.append(a.name)
    sb.append(", age = ")
    sb.append(a.age)
    sb.append(", email = ")
    a.email match {
      case Some(v) => sb.append("Some(").append(v).append(")")
      case None    => sb.append("None")
    }
    sb.append(", addresses = List(")
    var first = true
    a.addresses.foreach { addr =>
      if (!first) sb.append(", ")
      sb.append(showAddress(addr))
      first = false
    }
    sb.append("), tags = Map(")
    first = true
    a.tags.foreach { case (k, v) =>
      if (!first) sb.append(", ")
      sb.append(k).append(" -> ").append(v)
      first = false
    }
    sb.append("), scores = Vector(")
    first = true
    a.scores.foreach { s =>
      if (!first) sb.append(", ")
      sb.append(s.toString)
      first = false
    }
    sb.append("))")
    sb.toString
  }

  def showEvent(a: Event): String = a match {
    case Event.UserCreated(person, timestamp) =>
      "UserCreated(person = " + showPerson(person) + ", timestamp = " + timestamp.toString + ")"
    case Event.UserUpdated(personId, changes) =>
      val changesStr = changes.map { case (k, v) => k + " -> " + v }.mkString(", ")
      "UserUpdated(personId = " + personId + ", changes = Map(" + changesStr + "))"
    case Event.UserDeleted(personId, reason) =>
      val reasonStr = reason match { case Some(r) => "Some(" + r + ")"; case None => "None" }
      "UserDeleted(personId = " + personId + ", reason = " + reasonStr + ")"
  }

  // --- Hash implementations ---

  def hashSimpleCC(a: SimpleCC): Int = {
    var h = MurmurHash3.productSeed
    h = MurmurHash3.mix(h, a.name.hashCode)
    h = MurmurHash3.mix(h, a.age)
    h = MurmurHash3.mix(h, java.lang.Boolean.hashCode(a.active))
    MurmurHash3.finalizeHash(h, 3)
  }

  def eqvSimpleCC(x: SimpleCC, y: SimpleCC): Boolean =
    x.name == y.name && x.age == y.age && x.active == y.active

  def hashPerson(a: Person): Int = {
    var h = MurmurHash3.productSeed
    h = MurmurHash3.mix(h, a.name.hashCode)
    h = MurmurHash3.mix(h, a.age)
    h = MurmurHash3.mix(h, a.email.hashCode)
    h = MurmurHash3.mix(h, MurmurHash3.orderedHash(a.addresses.map(addr => hashAddress(addr))))
    h = MurmurHash3.mix(h, MurmurHash3.unorderedHash(a.tags))
    h = MurmurHash3.mix(h, MurmurHash3.orderedHash(a.scores.map(java.lang.Double.hashCode)))
    MurmurHash3.finalizeHash(h, 6)
  }

  private def hashAddress(a: Address): Int = {
    var h = MurmurHash3.productSeed
    h = MurmurHash3.mix(h, a.street.hashCode)
    h = MurmurHash3.mix(h, a.city.hashCode)
    h = MurmurHash3.mix(h, a.zip.hashCode)
    h = MurmurHash3.mix(h, a.country.hashCode)
    MurmurHash3.finalizeHash(h, 4)
  }

  def eqvPerson(x: Person, y: Person): Boolean =
    x.name == y.name &&
      x.age == y.age &&
      x.email == y.email &&
      x.addresses == y.addresses &&
      x.tags == y.tags &&
      x.scores == y.scores

  // --- Encoder implementations ---

  def encodeSimpleCC(a: SimpleCC): Json = Json.fromJsonObject(
    JsonObject(
      "name" -> Json.fromString(a.name),
      "age" -> Json.fromInt(a.age),
      "active" -> Json.fromBoolean(a.active)
    )
  )

  private def encodeAddress(a: Address): Json = Json.fromJsonObject(
    JsonObject(
      "street" -> Json.fromString(a.street),
      "city" -> Json.fromString(a.city),
      "zip" -> Json.fromString(a.zip),
      "country" -> Json.fromString(a.country)
    )
  )

  def encodePerson(a: Person): Json = Json.fromJsonObject(
    JsonObject(
      "name" -> Json.fromString(a.name),
      "age" -> Json.fromInt(a.age),
      "email" -> a.email.fold(Json.Null)(Json.fromString),
      "addresses" -> Json.fromValues(a.addresses.map(encodeAddress)),
      "tags" -> Json.fromJsonObject(
        JsonObject.fromIterable(a.tags.map { case (k, v) => k -> Json.fromString(v) })
      ),
      "scores" -> Json.fromValues(a.scores.map(Json.fromDoubleOrNull))
    )
  )

  // --- Jsoniter codec implementations (SimpleCC only) ---

  def encodeSimpleCCJsoniter(x: SimpleCC, out: JsonWriter): Unit = {
    out.writeObjectStart()
    out.writeNonEscapedAsciiKey("name")
    out.writeVal(x.name)
    out.writeNonEscapedAsciiKey("age")
    out.writeVal(x.age)
    out.writeNonEscapedAsciiKey("active")
    out.writeVal(x.active)
    out.writeObjectEnd()
  }

  // --- Functor implementations (polymorphic, for SimpleCC wrapper) ---

  def mapSimpleCCBox[A, B](fa: SimpleCCBox[A], f: A => B): SimpleCCBox[B] =
    SimpleCCBox(fa.name, fa.age, f(fa.value))

  def decodeSimpleCCJsoniter(in: JsonReader, default: SimpleCC): SimpleCC = {
    var name: String = if (default ne null) default.name else null
    var age: Int = if (default ne null) default.age else 0
    var active: Boolean = if (default ne null) default.active else false
    if (in.isNextToken('{')) {
      if (!in.isNextToken('}')) {
        in.rollbackToken()
        var continue = true
        while (continue) {
          val key = in.readKeyAsString()
          if (key == "name") name = in.readString(name)
          else if (key == "age") age = in.readInt()
          else if (key == "active") active = in.readBoolean()
          else in.skip()
          continue = in.isNextToken(',')
        }
        if (!in.isCurrentToken('}')) in.objectEndOrCommaError()
      }
    } else in.readNullOrTokenError(default, '{')
    SimpleCC(name, age, active)
  }
}
