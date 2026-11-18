package hearth.kindlings.fastshowpretty

import hearth.MacroSuite

/** An instance for a 100-field case class, derived in a class of its own so its bytecode can be measured. */
object FastShowPrettyWideInstances {
  val instance: FastShowPretty[Wide] = FastShowPretty.derived[Wide]
}

/** Guards against generated methods too large to be JIT-compiled and against class files that grow quadratically with
  * the number of fields (see `docs/research/generated-code-audit-2026-11.md`). JVM only: it reads the class files.
  */
final class FastShowPrettyBytecodeSizeSpec extends MacroSuite {

  test("no method of the instance derived for a 100-field case class exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(FastShowPrettyWideInstances.getClass)
    assert(sizes.map(_._3).sum > 1000, s"expected the derived instance's code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the classes of the instance derived for a 100-field case class stay linear in the number of fields") {
    // ~100 B per field is plenty for the flat code; nested appends made it ~5 KB per field on Scala 2.13
    val sizes = BytecodeSizes.classFileSizes(FastShowPrettyWideInstances.getClass)
    val total = sizes.map(_._2).sum
    assert(total < 32 * 1024, s"class files of $total B in total: ${sizes.mkString(", ")}")
  }

  test("the instance renders every field") {
    val rendered =
      FastShowPrettyWideInstances.instance.render(new StringBuilder, RenderConfig.Default, 0)(Wide()).toString
    val expectedFields = (0 until 100).map { i =>
      val value = i % 6 match {
        case 0 => "None"
        case 1 => "7"
        case 2 => "\"d\""
        case 3 => "9L"
        case 4 => "true"
        case _ => "List(\n    1\n  )"
      }
      s"  f$i = $value"
    }
    rendered ==> expectedFields.mkString("Wide(\n", ",\n", "\n)")
  }
}
