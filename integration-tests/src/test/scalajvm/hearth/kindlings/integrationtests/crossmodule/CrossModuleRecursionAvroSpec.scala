package hearth.kindlings.integrationtests.crossmodule

import hearth.MacroSuite
import hearth.kindlings.avroderivation.{AvroDecoder, AvroEncoder, AvroIO, AvroSchemaFor}
import hearth.kindlings.integrationtests.{crossmodule as cp, samemodule as sm}

/** Avro counterpart of [[CrossModuleRecursionSpec]] (avro-derivation is JVM-only). */
final class CrossModuleRecursionAvroSpec extends MacroSuite {

  private def normalized(s: String): String = s.replace("samemodule", "crossmodule")

  private def check[A, B](value: A, local: B)(
      cpEncoder: AvroEncoder[A],
      cpDecoder: AvroDecoder[A],
      smEncoder: AvroEncoder[B]
  ): Unit = {
    normalized(cpEncoder.schema.toString) ==> normalized(smEncoder.schema.toString)
    val bytes = AvroIO.toBinary(value)(cpEncoder)
    bytes.toList ==> AvroIO.toBinary(local)(smEncoder).toList
    AvroIO.fromBinary[A](bytes)(cpDecoder) ==> value
    normalized(AvroIO.toJson(value)(cpEncoder)) ==> normalized(AvroIO.toJson(local)(smEncoder))
  }

  group("avro") {

    test("schemas of recursive types from the classpath are the same as of local ones") {
      normalized(AvroSchemaFor.derived[cp.RecTree].schema.toString) ==>
        normalized(AvroSchemaFor.derived[sm.RecTree].schema.toString)
      normalized(AvroSchemaFor.derived[cp.RecNode].schema.toString) ==>
        normalized(AvroSchemaFor.derived[sm.RecNode].schema.toString)
      normalized(AvroSchemaFor.derived[cp.RecDept].schema.toString) ==>
        normalized(AvroSchemaFor.derived[sm.RecDept].schema.toString)
    }

    test("encodes and decodes recursive types from the classpath like local ones") {
      check(cp.RecSamples.tree, sm.RecSamples.tree)(
        AvroEncoder.derived[cp.RecTree],
        AvroDecoder.derived[cp.RecTree],
        AvroEncoder.derived[sm.RecTree]
      )
      check(cp.RecSamples.opt, sm.RecSamples.opt)(
        AvroEncoder.derived[cp.RecOpt],
        AvroDecoder.derived[cp.RecOpt],
        AvroEncoder.derived[sm.RecOpt]
      )
      check(cp.RecSamples.map, sm.RecSamples.map)(
        AvroEncoder.derived[cp.RecMap],
        AvroDecoder.derived[cp.RecMap],
        AvroEncoder.derived[sm.RecMap]
      )
      check(cp.RecSamples.node, sm.RecSamples.node)(
        AvroEncoder.derived[cp.RecNode],
        AvroDecoder.derived[cp.RecNode],
        AvroEncoder.derived[sm.RecNode]
      )
      check(cp.RecSamples.dept, sm.RecSamples.dept)(
        AvroEncoder.derived[cp.RecDept],
        AvroDecoder.derived[cp.RecDept],
        AvroEncoder.derived[sm.RecDept]
      )
      check(cp.RecSamples.gtree, sm.RecSamples.gtree)(
        AvroEncoder.derived[cp.RecGTree[Int]],
        AvroDecoder.derived[cp.RecGTree[Int]],
        AvroEncoder.derived[sm.RecGTree[Int]]
      )
    }
  }
}
