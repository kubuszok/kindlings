package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import hearth.MacroSuite

/** `KindlingsJsonCodec.derived` (value + key codec) exists only on Scala 2. */
final class KindlingsJsonCodecScala2Spec extends MacroSuite {

  test("KindlingsJsonCodec honours encodingOnly / decodingOnly and validates runtime configs") {
    val encodingOnly = {
      implicit val c: JsoniterConfig = ContradictoryRuntimeConfigs.encodingOnly
      KindlingsJsonCodec.derived[Int]
    }
    writeToString(1)(encodingOnly) ==> "1"
    val _ = intercept[NotImplementedError](readFromString[Int]("1")(encodingOnly))
    val error = intercept[IllegalArgumentException] {
      implicit val c: JsoniterConfig = ContradictoryRuntimeConfigs.bothOnly
      KindlingsJsonCodec.derived[Int]
    }
    error.getMessage ==> "'decodingOnly' and 'encodingOnly' cannot be 'true' simultaneously"
  }
}
