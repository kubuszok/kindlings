package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, JsonReader, JsonValueCodec, JsonWriter}
import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils

/** The per-instance key lookups of derived case class decoders (`JsoniterDerivationUtils.KeyTable` for keys known at
  * compile time, `JsoniterDerivationUtils.FieldIndex` for keys mapped by a runtime config).
  */
final class KeyLookupSpec extends MacroSuite {

  /** The index `table` finds for each key of a JSON object (whose values are skipped). */
  private def indicesOf(table: JsoniterDerivationUtils.KeyTable, json: String): List[Int] =
    readFromString(json)(new JsonValueCodec[List[Int]] {
      def nullValue: List[Int] = Nil
      def decodeValue(in: JsonReader, default: List[Int]): List[Int] = {
        val found = List.newBuilder[Int]
        if (in.isNextToken('{'.toByte) && !in.isNextToken('}'.toByte)) {
          in.rollbackToken()
          var len = -1
          while (len < 0 || in.isNextToken(','.toByte)) {
            len = in.readKeyAsCharBuf()
            found += table.indexOf(in, len)
            in.skip()
          }
        }
        found.result()
      }
      def encodeValue(x: List[Int], out: JsonWriter): Unit = ()
    })

  private def hashOf(key: String): Int = JsonReader.toHashCode(key.toCharArray, key.length)

  group("KeyTable") {

    test("finds every key by its index and returns -1 for unknown keys") {
      val keys = (0 until 20).map(i => s"field$i").toArray
      val table = JsoniterDerivationUtils.keyTable(keys)
      indicesOf(table, keys.reverse.map(k => s""""$k":1""").mkString("{", ",", "}")) ==> (0 until 20).reverse.toList
      indicesOf(table, """{"unknown":1,"field":2,"field20":3,"":4}""") ==> List(-1, -1, -1, -1)
    }

    test("tells apart keys with the same hash") {
      assert(hashOf("Aa") == hashOf("BB"), "the test needs two keys with the same hash")
      val table = JsoniterDerivationUtils.keyTable(Array("x", "BB", "y", "Aa"))
      indicesOf(table, """{"Aa":1,"BB":2,"x":3,"y":4,"C#":5}""") ==> List(3, 1, 0, 2, -1)
    }

    test("works without keys") {
      indicesOf(JsoniterDerivationUtils.keyTable(Array.empty), """{"a":1}""") ==> List(-1)
    }
  }

  group("FieldIndex") {

    test("finds the index and the name of every key") {
      val index = JsoniterDerivationUtils.fieldIndex(Array("a", "b", "c"))
      List("c", "a", "b", "d").map(index.indexOf) ==> List(2, 0, 1, -1)
      (0 until 3).map(index.name).toList ==> List("a", "b", "c")
    }

    test("gives a key mapped from several fields to the first of them") {
      val index = JsoniterDerivationUtils.fieldIndex(Array("a", "same", "b", "same"))
      index.indexOf("same") ==> 1
    }
  }
}
