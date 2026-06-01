package hearth.kindlings.xmlderivation

import hearth.MacroSuite

final class RoundTripSpec extends MacroSuite {

  private def roundTrip[A](value: A, elementName: String)(implicit
      encoder: XmlEncoder[A],
      decoder: XmlDecoder[A]
  ): Either[XmlDecodingError, A] = {
    val elem = encoder.encode(value, elementName)
    decoder.decode(elem)
  }

  group("Round-trip encoding/decoding") {

    test("simple case class") {
      implicit val encoder: XmlEncoder[SimplePerson] = KindlingsXmlEncoder.derived[SimplePerson]
      implicit val decoder: XmlDecoder[SimplePerson] = KindlingsXmlDecoder.derived[SimplePerson]
      val person = SimplePerson("Alice", 30)
      assert(roundTrip(person, "person") == Right(person))
    }

    test("empty case class") {
      implicit val encoder: XmlEncoder[EmptyClass] = KindlingsXmlEncoder.derived[EmptyClass]
      implicit val decoder: XmlDecoder[EmptyClass] = KindlingsXmlDecoder.derived[EmptyClass]
      val empty = EmptyClass()
      assert(roundTrip(empty, "empty") == Right(empty))
    }

    test("nested case class") {
      implicit val encoder: XmlEncoder[PersonWithAddress] = KindlingsXmlEncoder.derived[PersonWithAddress]
      implicit val decoder: XmlDecoder[PersonWithAddress] = KindlingsXmlDecoder.derived[PersonWithAddress]
      val person = PersonWithAddress("Bob", 25, Address("123 Main St", "Springfield"))
      assert(roundTrip(person, "person") == Right(person))
    }

    test("generic types") {
      implicit val encoder: XmlEncoder[Pair[String, Int]] = KindlingsXmlEncoder.derived[Pair[String, Int]]
      implicit val decoder: XmlDecoder[Pair[String, Int]] = KindlingsXmlDecoder.derived[Pair[String, Int]]
      val pair = Pair("hello", 42)
      assert(roundTrip(pair, "pair") == Right(pair))
    }

    test("deeply nested") {
      implicit val encoder: XmlEncoder[PersonFull] = KindlingsXmlEncoder.derived[PersonFull]
      implicit val decoder: XmlDecoder[PersonFull] = KindlingsXmlDecoder.derived[PersonFull]
      val person = PersonFull("Alice", FullAddress("123 Main", "NY", GeoCoordinates(40.7, -74.0)))
      assert(roundTrip(person, "person") == Right(person))
    }

    // Note: XML decoder derivation for recursive types fails on Scala 2 with "not found: type $anonfun"
    // (a Scala 2 macro reification limitation). Encoder-only test:
    test("indirect recursive type encoder compiles") {
      val encoder: XmlEncoder[RecursiveParent] = KindlingsXmlEncoder.derived[RecursiveParent]
      val value = RecursiveParent(
        "root",
        List(
          RecursiveNode("a", List(RecursiveNode("b", Nil))),
          RecursiveNode("c", Nil)
        )
      )
      val elem = encoder.encode(value, "parent")
      assert(elem.label == "parent")
    }

    test("custom field names with @xmlName") {
      implicit val encoder: XmlEncoder[XmlWithFieldName] = KindlingsXmlEncoder.derived[XmlWithFieldName]
      implicit val decoder: XmlDecoder[XmlWithFieldName] = KindlingsXmlDecoder.derived[XmlWithFieldName]
      val value = XmlWithFieldName("John", 30)
      assert(roundTrip(value, "user") == Right(value))
    }
  }

  group("Recursive types - def caching") {

    // Note: XML decoder derivation for recursive types fails on Scala 2 with "not found: type $anonfun"
    // (a Scala 2 macro reification limitation). Encoder-only tests below; round-trip tests require Scala 3.

    test("direct recursive sealed trait encoder compiles") {
      val encoder: XmlEncoder[TreeNode] = KindlingsXmlEncoder.derived[TreeNode]
      val value: TreeNode = Branch(1, Branch(2, Leaf(3), Leaf(4)), Leaf(5))
      val elem = encoder.encode(value, "tree")
      assert(elem.label == "tree")
    }

    test("leaf-only recursive sealed trait encoder compiles") {
      val encoder: XmlEncoder[TreeNode] = KindlingsXmlEncoder.derived[TreeNode]
      val value: TreeNode = Leaf(42)
      val elem = encoder.encode(value, "tree")
      assert(elem.label == "tree")
    }

    test("deeply nested recursive sealed trait encoder compiles") {
      val encoder: XmlEncoder[TreeNode] = KindlingsXmlEncoder.derived[TreeNode]
      val value: TreeNode = Branch(1, Branch(2, Branch(3, Leaf(4), Leaf(5)), Leaf(6)), Branch(7, Leaf(8), Leaf(9)))
      val elem = encoder.encode(value, "tree")
      assert(elem.label == "tree")
    }

    test("mutual recursion encoder compiles") {
      val encoder: XmlEncoder[MutRecA] = KindlingsXmlEncoder.derived[MutRecA]
      val value = MutRecA(1, Some(MutRecB("x", Some(MutRecA(2, None)))))
      val elem = encoder.encode(value, "mutrec")
      assert(elem.label == "mutrec")
    }

    test("mutual recursion encoder from B side compiles") {
      val encoder: XmlEncoder[MutRecB] = KindlingsXmlEncoder.derived[MutRecB]
      val value = MutRecB("root", Some(MutRecA(1, Some(MutRecB("leaf", None)))))
      val elem = encoder.encode(value, "mutrec")
      assert(elem.label == "mutrec")
    }

    test("mutual recursion encoder with no nesting compiles") {
      val encoder: XmlEncoder[MutRecA] = KindlingsXmlEncoder.derived[MutRecA]
      val value = MutRecA(42, None)
      val elem = encoder.encode(value, "mutrec")
      assert(elem.label == "mutrec")
    }
  }

  group("combinatorial: wrapper x inner type") {

    test("CombOuter all fields populated") {
      implicit val encoder: XmlEncoder[CombOuter] = KindlingsXmlEncoder.derived[CombOuter]
      implicit val decoder: XmlDecoder[CombOuter] = KindlingsXmlDecoder.derived[CombOuter]
      val value = CombOuter(
        optPrimitive = Some(42),
        optCaseClass = Some(CombInnerCC("hello", 7)),
        optSealedTrait = Some(CombVariantA(99)),
        listCaseClass = List(CombInnerCC("a", 1), CombInnerCC("b", 2)),
        mapCaseClass = Map("k1" -> CombInnerCC("m", 10))
      )
      assert(roundTrip(value, "comb") == Right(value))
    }

    test("CombOuter None and empty collections") {
      implicit val encoder: XmlEncoder[CombOuter] = KindlingsXmlEncoder.derived[CombOuter]
      implicit val decoder: XmlDecoder[CombOuter] = KindlingsXmlDecoder.derived[CombOuter]
      val value = CombOuter(
        optPrimitive = None,
        optCaseClass = None,
        optSealedTrait = None,
        listCaseClass = Nil,
        mapCaseClass = Map.empty
      )
      assert(roundTrip(value, "comb") == Right(value))
    }

    test("Option[SealedTrait] with second variant") {
      implicit val encoder: XmlEncoder[CombOuter] = KindlingsXmlEncoder.derived[CombOuter]
      implicit val decoder: XmlDecoder[CombOuter] = KindlingsXmlDecoder.derived[CombOuter]
      val value = CombOuter(
        optPrimitive = None,
        optCaseClass = None,
        optSealedTrait = Some(CombVariantB("variant-b")),
        listCaseClass = Nil,
        mapCaseClass = Map.empty
      )
      assert(roundTrip(value, "comb") == Right(value))
    }

    test("@xmlName on sealed trait subtype field round-trips") {
      implicit val encoder: XmlEncoder[CombAnnotatedST] = KindlingsXmlEncoder.derived[CombAnnotatedST]
      @annotation.nowarn("msg=never used")
      implicit val decoder: XmlDecoder[CombAnnotatedST] = KindlingsXmlDecoder.derived[CombAnnotatedST]
      val value: CombAnnotatedST = CombAnnotA("test")
      assert(roundTrip(value, "annot") == Right(value))
    }

    test("@xmlAttribute on sealed trait subtype field round-trips".ignore) {
      implicit val encoder: XmlEncoder[CombAnnotatedST] = KindlingsXmlEncoder.derived[CombAnnotatedST]
      @annotation.nowarn("msg=never used")
      implicit val decoder: XmlDecoder[CombAnnotatedST] = KindlingsXmlDecoder.derived[CombAnnotatedST]
      val value: CombAnnotatedST = CombAnnotB(true)
      assert(roundTrip(value, "annot") == Right(value))
    }

    test("@transientField on sealed trait subtype round-trips (field absent, default used)") {
      implicit val encoder: XmlEncoder[CombTransientST] = KindlingsXmlEncoder.derived[CombTransientST]
      @annotation.nowarn("msg=never used")
      implicit val decoder: XmlDecoder[CombTransientST] = KindlingsXmlDecoder.derived[CombTransientST]
      val value: CombTransientST = CombTransientA("Alice", "should-be-dropped")
      assert(roundTrip(value, "item") == Right(CombTransientA("Alice", "")))
    }

    test("@transientField on sealed trait subtype round-trips (Option field, default None)") {
      implicit val encoder: XmlEncoder[CombTransientST] = KindlingsXmlEncoder.derived[CombTransientST]
      @annotation.nowarn("msg=never used")
      implicit val decoder: XmlDecoder[CombTransientST] = KindlingsXmlDecoder.derived[CombTransientST]
      val value: CombTransientST = CombTransientB(42, Some("should-be-dropped"))
      assert(roundTrip(value, "item") == Right(CombTransientB(42, None)))
    }
  }
}
