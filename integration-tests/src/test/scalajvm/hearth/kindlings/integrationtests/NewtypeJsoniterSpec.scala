package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}

final class NewtypeJsoniterSpec extends MacroSuite {

  private val userJson = """{"id":1,"name":"alice","score":42,"tags":["a","b"]}"""

  group("scala-newtype + Jsoniter") {

    test("case class with newtype fields encodes correctly") {
      val codec = KindlingsJsonValueCodec.derived[NewtypeUser]
      writeToString(NewtypeUser.example)(codec) ==> userJson
    }

    test("case class with newtype fields decodes") {
      val codec = KindlingsJsonValueCodec.derived[NewtypeUser]
      readFromString(userJson)(codec) ==> NewtypeUser.example
    }
  }
}
