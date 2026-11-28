package hearth.kindlings.pureconfigderivation

import hearth.MacroSuite
import pureconfig.ConfigCursor

/** Instances for a 30-element tuple (a `TupleXXL`, Scala 3 only), derived in a class of their own so their bytecode can
  * be measured.
  */
object PureConfigWideGenericTupleInstances {
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
  val reader: KindlingsConfigReader[T30] = KindlingsConfigReader.derived[T30]
  val writer: KindlingsConfigWriter[T30] = KindlingsConfigWriter.derived[T30]

  val value: T30 = Tuple
    .fromArray(Array.tabulate[Object](30)(i => if (i % 2 == 0) Int.box(i) else s"s$i"))
    .asInstanceOf[T30]
}

/** Guards against generated methods too large to be JIT-compiled for tuples with more than 22 elements. */
final class PureConfigGenericTupleBytecodeSizeSpec extends MacroSuite {

  test("no method of the instances derived for a 30-element tuple exceeds HotSpot's JIT limit") {
    val sizes = BytecodeSizes.methodSizes(PureConfigWideGenericTupleInstances.getClass)
    assert(sizes.map(_._3).sum > 1000, s"expected the derived instances' code, got: $sizes")
    val huge = sizes.filter(_._3 > BytecodeSizes.HugeMethodLimit)
    assert(huge.isEmpty, s"methods larger than ${BytecodeSizes.HugeMethodLimit} B: ${huge.mkString(", ")}")
  }

  test("the instances work") {
    import PureConfigWideGenericTupleInstances.*
    reader.from(ConfigCursor(writer.to(value), Nil)) ==> Right(value)
  }
}
