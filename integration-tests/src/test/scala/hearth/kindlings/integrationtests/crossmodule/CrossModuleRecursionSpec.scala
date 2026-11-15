package hearth.kindlings.integrationtests.crossmodule

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}
import hearth.kindlings.integrationtests.{crossmodule as cp, samemodule as sm}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.scalacheckderivation.{DeriveArbitrary, DeriveCogen, DeriveShrink}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import hearth.kindlings.ubjsonderivation.UBJsonValueCodec
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils
import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder, XmlDecoder, XmlEncoder}
import hearth.kindlings.yamlderivation.{KindlingsYamlDecoder, KindlingsYamlEncoder}
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed
import sttp.tapir.{Schema, SchemaType}

/** Tapir schemas of [[CrossModuleRecursionSpec]] (`PreferSchemaConfig` must be defined in an object). */
object CrossModuleRecursionTapirSchemas {
  implicit val config: JsoniterConfig = JsoniterConfig.default
  implicit val prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]

  val cpTree: Schema[cp.RecTree] = KindlingsSchema.derived[cp.RecTree].schema
  val smTree: Schema[sm.RecTree] = KindlingsSchema.derived[sm.RecTree].schema
  val cpOpt: Schema[cp.RecOpt] = KindlingsSchema.derived[cp.RecOpt].schema
  val smOpt: Schema[sm.RecOpt] = KindlingsSchema.derived[sm.RecOpt].schema
  val cpMap: Schema[cp.RecMap] = KindlingsSchema.derived[cp.RecMap].schema
  val smMap: Schema[sm.RecMap] = KindlingsSchema.derived[sm.RecMap].schema
  val cpNode: Schema[cp.RecNode] = KindlingsSchema.derived[cp.RecNode].schema
  val smNode: Schema[sm.RecNode] = KindlingsSchema.derived[sm.RecNode].schema
  val cpDept: Schema[cp.RecDept] = KindlingsSchema.derived[cp.RecDept].schema
  val smDept: Schema[sm.RecDept] = KindlingsSchema.derived[sm.RecDept].schema
  val cpGTree: Schema[cp.RecGTree[Int]] = KindlingsSchema.derived[cp.RecGTree[Int]].schema
  val smGTree: Schema[sm.RecGTree[Int]] = KindlingsSchema.derived[sm.RecGTree[Int]].schema
}

/** Recursive types read from the classpath (`cp`, defined in the main sources) must be derived exactly like the same
  * types compiled together with the derivation (`sm`, defined in the test sources) - like users' recursive types
  * defined in another module than the one deriving their instances.
  *
  * Regression test: on Scala 3 `Type.prettyPrint` colors a type read from TASTy (e.g. a field type of a classpath type)
  * differently than the same type written at the macro call site, and some derivations used `prettyPrint` as the key of
  * their recursion detection/caches: tapir emitted the recursive product once more before the `SRef`, the circe
  * decoder, jsoniter, cats `Hash` and diff generated a duplicated helper for such types.
  */
final class CrossModuleRecursionSpec extends MacroSuite {

  /** Outputs mentioning fully-qualified names differ only by the package. */
  private def normalized(s: String): String = s.replace("samemodule", "crossmodule")

  private def assertSame(fromClasspath: Any, compiledTogether: Any): Unit =
    normalized(fromClasspath.toString) ==> normalized(compiledTogether.toString)

  group("fast-show-pretty") {

    test("renders recursive types from the classpath like local ones") {
      val cfg = RenderConfig.Default
      assertSame(FastShowPretty.render(cp.RecSamples.tree, cfg), FastShowPretty.render(sm.RecSamples.tree, cfg))
      assertSame(FastShowPretty.render(cp.RecSamples.opt, cfg), FastShowPretty.render(sm.RecSamples.opt, cfg))
      assertSame(FastShowPretty.render(cp.RecSamples.map, cfg), FastShowPretty.render(sm.RecSamples.map, cfg))
      assertSame(FastShowPretty.render(cp.RecSamples.node, cfg), FastShowPretty.render(sm.RecSamples.node, cfg))
      assertSame(FastShowPretty.render(cp.RecSamples.dept, cfg), FastShowPretty.render(sm.RecSamples.dept, cfg))
      assertSame(FastShowPretty.render(cp.RecSamples.gtree, cfg), FastShowPretty.render(sm.RecSamples.gtree, cfg))
    }
  }

  group("circe") {

    test("encodes and decodes recursive types from the classpath like local ones") {
      def check[A](cpJson: io.circe.Json, smJson: io.circe.Json)(cpBack: Either[?, A], cpValue: A): Unit = {
        assertSame(cpJson.noSpaces, smJson.noSpaces)
        cpBack ==> Right(cpValue)
      }
      check(KindlingsEncoder.encode(cp.RecSamples.tree), KindlingsEncoder.encode(sm.RecSamples.tree))(
        KindlingsDecoder.decode[cp.RecTree](KindlingsEncoder.encode(cp.RecSamples.tree)),
        cp.RecSamples.tree
      )
      check(KindlingsEncoder.encode(cp.RecSamples.opt), KindlingsEncoder.encode(sm.RecSamples.opt))(
        KindlingsDecoder.decode[cp.RecOpt](KindlingsEncoder.encode(cp.RecSamples.opt)),
        cp.RecSamples.opt
      )
      check(KindlingsEncoder.encode(cp.RecSamples.map), KindlingsEncoder.encode(sm.RecSamples.map))(
        KindlingsDecoder.decode[cp.RecMap](KindlingsEncoder.encode(cp.RecSamples.map)),
        cp.RecSamples.map
      )
      check(KindlingsEncoder.encode(cp.RecSamples.node), KindlingsEncoder.encode(sm.RecSamples.node))(
        KindlingsDecoder.decode[cp.RecNode](KindlingsEncoder.encode(cp.RecSamples.node)),
        cp.RecSamples.node
      )
      check(KindlingsEncoder.encode(cp.RecSamples.dept), KindlingsEncoder.encode(sm.RecSamples.dept))(
        KindlingsDecoder.decode[cp.RecDept](KindlingsEncoder.encode(cp.RecSamples.dept)),
        cp.RecSamples.dept
      )
      check(KindlingsEncoder.encode(cp.RecSamples.gtree), KindlingsEncoder.encode(sm.RecSamples.gtree))(
        KindlingsDecoder.decode[cp.RecGTree[Int]](KindlingsEncoder.encode(cp.RecSamples.gtree)),
        cp.RecSamples.gtree
      )
      // rejected input is reported the same way
      assertSame(
        KindlingsDecoder.decode[cp.RecTree](
          io.circe.parser.parse("""{"value":1,"children":[{"value":"x"}]}""").toOption.get
        ),
        KindlingsDecoder.decode[sm.RecTree](
          io.circe.parser.parse("""{"value":1,"children":[{"value":"x"}]}""").toOption.get
        )
      )
    }
  }

  group("jsoniter") {

    def roundTrip[A](value: A)(implicit codec: JsonValueCodec[A]): (String, A) = {
      val json = writeToString(value)
      (json, readFromString[A](json))
    }

    test("encodes and decodes recursive types from the classpath like local ones") {
      def check[A, B](value: A, local: B)(implicit cpCodec: JsonValueCodec[A], smCodec: JsonValueCodec[B]): Unit = {
        val (cpJson, cpBack) = roundTrip(value)
        val (smJson, _) = roundTrip(local)
        assertSame(cpJson, smJson)
        cpBack ==> value
      }
      check(cp.RecSamples.tree, sm.RecSamples.tree)(
        KindlingsJsonValueCodec.derived[cp.RecTree],
        KindlingsJsonValueCodec.derived[sm.RecTree]
      )
      check(cp.RecSamples.opt, sm.RecSamples.opt)(
        KindlingsJsonValueCodec.derived[cp.RecOpt],
        KindlingsJsonValueCodec.derived[sm.RecOpt]
      )
      check(cp.RecSamples.map, sm.RecSamples.map)(
        KindlingsJsonValueCodec.derived[cp.RecMap],
        KindlingsJsonValueCodec.derived[sm.RecMap]
      )
      check(cp.RecSamples.node, sm.RecSamples.node)(
        KindlingsJsonValueCodec.derived[cp.RecNode],
        KindlingsJsonValueCodec.derived[sm.RecNode]
      )
      check(cp.RecSamples.dept, sm.RecSamples.dept)(
        KindlingsJsonValueCodec.derived[cp.RecDept],
        KindlingsJsonValueCodec.derived[sm.RecDept]
      )
      check(cp.RecSamples.gtree, sm.RecSamples.gtree)(
        KindlingsJsonValueCodec.derived[cp.RecGTree[Int]],
        KindlingsJsonValueCodec.derived[sm.RecGTree[Int]]
      )
    }
  }

  group("ubjson") {

    def roundTrip[A](value: A)(implicit codec: UBJsonValueCodec[A]): (List[Byte], A) = {
      val bytes = UBJsonDerivationUtils.writeToBytes[A](value)(codec)
      (bytes.toList, UBJsonDerivationUtils.readFromBytes[A](bytes)(codec))
    }

    test("encodes and decodes recursive types from the classpath like local ones") {
      def check[A, B](value: A, local: B)(implicit cpCodec: UBJsonValueCodec[A], smCodec: UBJsonValueCodec[B]): Unit = {
        val (cpBytes, cpBack) = roundTrip(value)
        val (smBytes, _) = roundTrip(local)
        cpBytes ==> smBytes
        cpBack ==> value
      }
      check(cp.RecSamples.tree, sm.RecSamples.tree)(
        UBJsonValueCodec.derived[cp.RecTree],
        UBJsonValueCodec.derived[sm.RecTree]
      )
      check(cp.RecSamples.opt, sm.RecSamples.opt)(
        UBJsonValueCodec.derived[cp.RecOpt],
        UBJsonValueCodec.derived[sm.RecOpt]
      )
      check(cp.RecSamples.map, sm.RecSamples.map)(
        UBJsonValueCodec.derived[cp.RecMap],
        UBJsonValueCodec.derived[sm.RecMap]
      )
      check(cp.RecSamples.node, sm.RecSamples.node)(
        UBJsonValueCodec.derived[cp.RecNode],
        UBJsonValueCodec.derived[sm.RecNode]
      )
      check(cp.RecSamples.dept, sm.RecSamples.dept)(
        UBJsonValueCodec.derived[cp.RecDept],
        UBJsonValueCodec.derived[sm.RecDept]
      )
      check(cp.RecSamples.gtree, sm.RecSamples.gtree)(
        UBJsonValueCodec.derived[cp.RecGTree[Int]],
        UBJsonValueCodec.derived[sm.RecGTree[Int]]
      )
    }
  }

  group("yaml") {

    // Decoded from the encoded `Node`, not from the printed YAML: an empty sequence is printed as an empty value, which
    // does not decode back as a sequence (unrelated to recursion).
    test("encodes and decodes recursive types from the classpath like local ones") {
      def check[A](cpYaml: String, smYaml: String, cpBack: Either[?, A], value: A): Unit = {
        assertSame(cpYaml, smYaml)
        cpBack ==> Right(value)
      }
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.tree),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.tree),
        KindlingsYamlDecoder.decode[cp.RecTree](KindlingsYamlEncoder.encode(cp.RecSamples.tree)),
        cp.RecSamples.tree
      )
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.opt),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.opt),
        KindlingsYamlDecoder.decode[cp.RecOpt](KindlingsYamlEncoder.encode(cp.RecSamples.opt)),
        cp.RecSamples.opt
      )
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.map),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.map),
        KindlingsYamlDecoder.decode[cp.RecMap](KindlingsYamlEncoder.encode(cp.RecSamples.map)),
        cp.RecSamples.map
      )
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.node),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.node),
        KindlingsYamlDecoder.decode[cp.RecNode](KindlingsYamlEncoder.encode(cp.RecSamples.node)),
        cp.RecSamples.node
      )
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.dept),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.dept),
        KindlingsYamlDecoder.decode[cp.RecDept](KindlingsYamlEncoder.encode(cp.RecSamples.dept)),
        cp.RecSamples.dept
      )
      check(
        KindlingsYamlEncoder.toYamlString(cp.RecSamples.gtree),
        KindlingsYamlEncoder.toYamlString(sm.RecSamples.gtree),
        KindlingsYamlDecoder.decode[cp.RecGTree[Int]](KindlingsYamlEncoder.encode(cp.RecSamples.gtree)),
        cp.RecSamples.gtree
      )
    }
  }

  group("xml") {

    test("encodes and decodes recursive types from the classpath like local ones") {
      def check[A, B](value: A, local: B)(
          cpEncoder: XmlEncoder[A],
          cpDecoder: XmlDecoder[A],
          smEncoder: XmlEncoder[B]
      ): Unit = {
        val cpXml = cpEncoder.encode(value, "root")
        assertSame(cpXml, smEncoder.encode(local, "root"))
        cpDecoder.decode(cpXml) ==> Right(value)
      }
      check(cp.RecSamples.tree, sm.RecSamples.tree)(
        KindlingsXmlEncoder.derived[cp.RecTree],
        KindlingsXmlDecoder.derived[cp.RecTree],
        KindlingsXmlEncoder.derived[sm.RecTree]
      )
      check(cp.RecSamples.opt, sm.RecSamples.opt)(
        KindlingsXmlEncoder.derived[cp.RecOpt],
        KindlingsXmlDecoder.derived[cp.RecOpt],
        KindlingsXmlEncoder.derived[sm.RecOpt]
      )
      check(cp.RecSamples.map, sm.RecSamples.map)(
        KindlingsXmlEncoder.derived[cp.RecMap],
        KindlingsXmlDecoder.derived[cp.RecMap],
        KindlingsXmlEncoder.derived[sm.RecMap]
      )
      check(cp.RecSamples.node, sm.RecSamples.node)(
        KindlingsXmlEncoder.derived[cp.RecNode],
        KindlingsXmlDecoder.derived[cp.RecNode],
        KindlingsXmlEncoder.derived[sm.RecNode]
      )
      check(cp.RecSamples.dept, sm.RecSamples.dept)(
        KindlingsXmlEncoder.derived[cp.RecDept],
        KindlingsXmlDecoder.derived[cp.RecDept],
        KindlingsXmlEncoder.derived[sm.RecDept]
      )
      check(cp.RecSamples.gtree, sm.RecSamples.gtree)(
        KindlingsXmlEncoder.derived[cp.RecGTree[Int]],
        KindlingsXmlDecoder.derived[cp.RecGTree[Int]],
        KindlingsXmlEncoder.derived[sm.RecGTree[Int]]
      )
    }
  }

  group("tapir schema") {
    import CrossModuleRecursionTapirSchemas.*

    def fields(schema: Schema[?]): Map[String, SchemaType[?]] = schema.schemaType match {
      case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.name -> f.schema.schemaType).toMap
      case other                     => fail(s"Expected SProduct, got: $other")
    }
    def unwrapped(s: Schema[?]): Schema[?] = s.schemaType match {
      case p: SchemaType.SProduct[?] if s.name.isEmpty && p.fields.size == 1 => p.fields.head.schema
      case _                                                                 => s
    }
    def isRef(tpe: SchemaType[?]): Boolean = tpe match {
      case _: SchemaType.SRef[?] => true
      case _                     => false
    }
    def element(tpe: SchemaType[?]): SchemaType[?] = tpe match {
      case arr: SchemaType.SArray[?, ?]            => arr.element.schemaType
      case SchemaType.SOption(inner)               => inner.schemaType
      case SchemaType.SOpenProduct(_, valueSchema) => valueSchema.schemaType
      case other                                   => fail(s"Expected a collection, an Option or a Map, got: $other")
    }

    def assertDirectSelfReferences(
        tree: Schema[?],
        opt: Schema[?],
        map: Schema[?],
        gtree: Schema[?],
        node: Schema[?]
    ) = {
      assert(isRef(element(fields(tree)("children"))), tree.toString)
      assert(isRef(element(fields(opt)("child"))), opt.toString)
      assert(isRef(element(fields(map)("children"))), map.toString)
      assert(isRef(element(fields(gtree)("children"))), gtree.toString)
      node.schemaType match {
        case c: SchemaType.SCoproduct[?] =>
          // jsoniter-scala's default config writes the subtypes wrapped (`{"RecBranch":{..}}`)
          val branch = c.subtypes
            .map(unwrapped)
            .find(_.name.exists(_.fullName.endsWith("RecBranch")))
            .getOrElse(fail(node.toString))
          assert(isRef(fields(branch)("left")), node.toString)
          assert(isRef(fields(branch)("right")), node.toString)
        case other => fail(s"Expected SCoproduct, got: $other")
      }
    }

    test("recursive types from the classpath reference themselves directly with an SRef") {
      assertDirectSelfReferences(cpTree, cpOpt, cpMap, cpGTree, cpNode)
    }

    // Written at the call site with another prefix (`sm.RecTree`) than in the field types (`RecTree`), the types printed
    // differently with the same colors problem even within one compilation run.
    test("recursive types referred to with a different prefix reference themselves directly with an SRef") {
      assertDirectSelfReferences(smTree, smOpt, smMap, smGTree, smNode)
    }

    test("derives recursive types from the classpath like local ones") {
      assertSame(cpTree, smTree)
      assertSame(cpOpt, smOpt)
      assertSame(cpMap, smMap)
      assertSame(cpNode, smNode)
      assertSame(cpDept, smDept)
      assertSame(cpGTree, smGTree)
    }
  }

  group("scalacheck") {

    def sample[A](arb: Arbitrary[A]): List[String] =
      (1 to 20).toList.flatMap(i =>
        arb.arbitrary.apply(Gen.Parameters.default.withSize(5), Seed(i.toLong)).map(_.toString)
      )

    test("generates, perturbs and shrinks recursive types from the classpath like local ones") {
      sample(DeriveArbitrary.derived[cp.RecTree]) ==> sample(DeriveArbitrary.derived[sm.RecTree])
      sample(DeriveArbitrary.derived[cp.RecOpt]) ==> sample(DeriveArbitrary.derived[sm.RecOpt])
      sample(DeriveArbitrary.derived[cp.RecMap]) ==> sample(DeriveArbitrary.derived[sm.RecMap])
      sample(DeriveArbitrary.derived[cp.RecNode]) ==> sample(DeriveArbitrary.derived[sm.RecNode])
      sample(DeriveArbitrary.derived[cp.RecDept]) ==> sample(DeriveArbitrary.derived[sm.RecDept])
      sample(DeriveArbitrary.derived[cp.RecGTree[Int]]) ==> sample(DeriveArbitrary.derived[sm.RecGTree[Int]])

      val cpCogen: Cogen[cp.RecTree] = DeriveCogen.derived[cp.RecTree]
      val smCogen: Cogen[sm.RecTree] = DeriveCogen.derived[sm.RecTree]
      cpCogen.perturb(Seed(1L), cp.RecSamples.tree) ==> smCogen.perturb(Seed(1L), sm.RecSamples.tree)
      val cpNodeCogen: Cogen[cp.RecNode] = DeriveCogen.derived[cp.RecNode]
      val smNodeCogen: Cogen[sm.RecNode] = DeriveCogen.derived[sm.RecNode]
      cpNodeCogen.perturb(Seed(1L), cp.RecSamples.node) ==> smNodeCogen.perturb(Seed(1L), sm.RecSamples.node)

      val cpShrink: Shrink[cp.RecTree] = DeriveShrink.derived[cp.RecTree]
      val smShrink: Shrink[sm.RecTree] = DeriveShrink.derived[sm.RecTree]
      assertSame(
        cpShrink.shrink(cp.RecSamples.tree).take(30).toList,
        smShrink.shrink(sm.RecSamples.tree).take(30).toList
      )
    }
  }

  group("cats") {
    import hearth.kindlings.catsderivation.extensions.*

    test("Show, Eq, Order and Hash of recursive types from the classpath behave like local ones") {
      assertSame(
        cats.Show.derived[cp.RecTree].show(cp.RecSamples.tree),
        cats.Show.derived[sm.RecTree].show(sm.RecSamples.tree)
      )
      assertSame(
        cats.Show.derived[cp.RecNode].show(cp.RecSamples.node),
        cats.Show.derived[sm.RecNode].show(sm.RecSamples.node)
      )
      assertSame(
        cats.Show.derived[cp.RecDept].show(cp.RecSamples.dept),
        cats.Show.derived[sm.RecDept].show(sm.RecSamples.dept)
      )

      val cpEq = cats.kernel.Eq.derived[cp.RecTree]
      assert(cpEq.eqv(cp.RecSamples.tree, cp.RecSamples.tree.copy()))
      assert(!cpEq.eqv(cp.RecSamples.tree, cp.RecTree(1, Nil)))

      val cpOrder = cats.kernel.Order.derived[cp.RecNode]
      val smOrder = cats.kernel.Order.derived[sm.RecNode]
      cpOrder.compare(cp.RecSamples.node, cp.RecLeaf(1)) ==> smOrder.compare(sm.RecSamples.node, sm.RecLeaf(1))

      val cpHash = cats.kernel.Hash.derived[cp.RecTree]
      val smHash = cats.kernel.Hash.derived[sm.RecTree]
      cpHash.hash(cp.RecSamples.tree) ==> smHash.hash(sm.RecSamples.tree)
      assert(cpHash.eqv(cp.RecSamples.tree, cp.RecSamples.tree.copy()))
    }
  }
}
