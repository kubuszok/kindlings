package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import hearth.MacroSuite

case class ParityIArray(a: IArray[Int], n: Int)

/** `IArray` exists only on Scala 3. */
final class IArrayParitySpec extends MacroSuite {

  group("transientEmpty with IArray") {

    test("omits an empty IArray instead of failing") {
      val codec = KindlingsJsonValueCodec.derived[ParityIArray](using JsoniterConfig.default.withTransientEmpty)
      writeToString(ParityIArray(IArray.empty[Int], 1))(codec) ==> """{"n":1}"""
      writeToString(ParityIArray(IArray(1), 1))(codec) ==> """{"a":[1],"n":1}"""
      val codecRuntime = {
        given JsoniterConfig = ParityRuntimeConfigs.transientEmpty
        KindlingsJsonValueCodec.derived[ParityIArray]
      }
      writeToString(ParityIArray(IArray.empty[Int], 1))(codecRuntime) ==> """{"n":1}"""
    }
  }
}
