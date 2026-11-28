package hearth.kindlings.sconfigderivation

import hearth.MacroSuite

/** Instances for a 30-element tuple (a `TupleXXL`, Scala 3 only), derived in a class of their own so their bytecode can
  * be measured.
  */
object SConfigWideGenericTupleInstances {
  type T30 = (
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String,
      Int,
      String
  )
  val reader: ConfigReader[T30] = ConfigReader.derived[T30]
  val writer: ConfigWriter[T30] = ConfigWriter.derived[T30]

  val value: T30 = Tuple
    .fromArray(Array.tabulate[Object](30)(i => if (i % 2 == 0) Int.box(i) else s"s$i"))
    .asInstanceOf[T30]
}

/** Guards against generated methods too large to be JIT-compiled for tuples with more than 22 elements. JVM only: it
  * reads the class files.
  */
final class SConfigGenericTupleBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 30-element tuple exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(SConfigWideGenericTupleInstances.getClass)
    assert(sizes.map(_._3).sum > 1000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances work") {
    import SConfigWideGenericTupleInstances.*
    reader.from(writer.to(value)) ==> Right(value)
  }
}
