package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString,
  writeToString,
  JsonReader,
  JsonValueCodec,
  JsonWriter
}
import hearth.MacroSuite

// A user's generic codec for a generic case class, whose JSON is deliberately not the structural one: it is only used
// when Kindlings passes it the codec of the type argument (a codec Kindlings has to derive itself).
final case class JnItem(n: Int)
final case class JnPage[A](items: List[A])
object JnPage {
  implicit def codec[A](implicit a: JsonValueCodec[A]): JsonValueCodec[JnPage[A]] = new JsonValueCodec[JnPage[A]] {
    def nullValue: JnPage[A] = null
    def encodeValue(x: JnPage[A], out: JsonWriter): Unit = {
      out.writeObjectStart()
      out.writeNonEscapedAsciiKey("DATA")
      out.writeArrayStart()
      x.items.foreach(a.encodeValue(_, out))
      out.writeArrayEnd()
      out.writeObjectEnd()
    }
    def decodeValue(in: JsonReader, default: JnPage[A]): JnPage[A] = {
      if (!in.isNextToken('{')) in.decodeError("expected '{'")
      if (in.readKeyAsString() != "DATA") in.decodeError("expected DATA")
      val items = List.newBuilder[A]
      if (!in.isNextToken('[')) in.decodeError("expected '['")
      if (!in.isNextToken(']')) {
        in.rollbackToken()
        items += a.decodeValue(in, a.nullValue)
        while (in.isNextToken(',')) items += a.decodeValue(in, a.nullValue)
        if (!in.isCurrentToken(']')) in.arrayEndOrCommaError()
      }
      if (!in.isNextToken('}')) in.objectEndOrCommaError()
      JnPage(items.result())
    }
  }
}
final case class JnWrapper[A](value: A)

final case class JnHoldsInts(page: JnPage[Int])
final case class JnHoldsItems(page: JnPage[JnItem])
final case class JnHoldsWrapped(page: JnPage[JnWrapper[JnItem]])
final case class JnTree(label: String, children: JnPage[JnTree])

/** A user's generic codec (`implicit def codec[A](implicit a: JsonValueCodec[A]): JsonValueCodec[Page[A]]`) is used for
  * a field even when the codec it needs has to be derived by Kindlings.
  */
final class NestedUserInstanceSpec extends MacroSuite {

  group("a user's generic codec for a field") {

    test("is used when its dependency is a built-in type (Page[Int]: jsoniter-scala has no implicit codec of Int)") {
      val codec = KindlingsJsonValueCodec.derived[JnHoldsInts]
      writeToString(JnHoldsInts(JnPage(List(1))))(codec) ==> """{"page":{"DATA":[1]}}"""
    }

    test("is used when its dependency has to be derived (Page[Item])") {
      val codec = KindlingsJsonValueCodec.derived[JnHoldsItems]
      val value = JnHoldsItems(JnPage(List(JnItem(1), JnItem(2))))
      val json = """{"page":{"DATA":[{"n":1},{"n":2}]}}"""
      writeToString(value)(codec) ==> json
      readFromString(json)(codec) ==> value
    }

    test("is used when its dependency is itself a derived generic type (Page[Wrapper[Item]])") {
      val codec = KindlingsJsonValueCodec.derived[JnHoldsWrapped]
      val value = JnHoldsWrapped(JnPage(List(JnWrapper(JnItem(1)))))
      val json = """{"page":{"DATA":[{"value":{"n":1}}]}}"""
      writeToString(value)(codec) ==> json
      readFromString(json)(codec) ==> value
    }

    test("is used with the codec being derived (a recursive type)") {
      val codec = KindlingsJsonValueCodec.derived[JnTree]
      val value = JnTree("root", JnPage(List(JnTree("leaf", JnPage(Nil)))))
      val json = """{"label":"root","children":{"DATA":[{"label":"leaf","children":{"DATA":[]}}]}}"""
      writeToString(value)(codec) ==> json
      readFromString(json)(codec) ==> value
    }

    test("is used by the inline writeToString and readFromString") {
      val value = JnHoldsItems(JnPage(List(JnItem(1))))
      val json = """{"page":{"DATA":[{"n":1}]}}"""
      KindlingsJsonValueCodec.writeToString(value) ==> json
      KindlingsJsonValueCodec.readFromString[JnHoldsItems](json) ==> Right(value)
    }

    test("is used when the nested codec is already a value in scope (the workaround)") {
      implicit val item: JsonValueCodec[JnItem] = KindlingsJsonValueCodec.derived[JnItem]
      val codec = KindlingsJsonValueCodec.derived[JnHoldsItems]
      writeToString(JnHoldsItems(JnPage(List(JnItem(1)))))(codec) ==> """{"page":{"DATA":[{"n":1}]}}"""
    }
  }
}
