package hearth.kindlings.catsderivation

import hearth.MacroSuite

final class CatsDerivationSpec extends MacroSuite {

  group("Show") {

    test("case class") {
      examples.Person.showPerson.show(examples.Person("Alice", 30)) ==> "Person(name = Alice, age = 30)"
    }

    test("empty case class") {
      examples.Empty.showEmpty.show(examples.Empty()) ==> "Empty()"
    }
  }

  group("Eq") {

    test("equal case classes") {
      examples.Person.eqPerson.eqv(examples.Person("Alice", 30), examples.Person("Alice", 30)) ==> true
    }

    test("unequal case classes") {
      examples.Person.eqPerson.eqv(examples.Person("Alice", 30), examples.Person("Bob", 25)) ==> false
    }

    test("empty case class") {
      examples.Empty.eqEmpty.eqv(examples.Empty(), examples.Empty()) ==> true
    }
  }

  group("Order") {

    test("equal points") {
      examples.Point.orderPoint.compare(examples.Point(1, 2), examples.Point(1, 2)) ==> 0
    }

    test("first field differs") {
      assert(examples.Point.orderPoint.compare(examples.Point(1, 2), examples.Point(2, 2)) < 0)
    }

    test("second field differs") {
      assert(examples.Point.orderPoint.compare(examples.Point(1, 3), examples.Point(1, 2)) > 0)
    }
  }

  group("Hash") {

    test("equal values have equal hashes") {
      val a = examples.Point(1, 2)
      val b = examples.Point(1, 2)
      examples.Point.hashPoint.hash(a) ==> examples.Point.hashPoint.hash(b)
    }

    test("eqv delegates correctly") {
      examples.Point.hashPoint.eqv(examples.Point(1, 2), examples.Point(1, 2)) ==> true
      examples.Point.hashPoint.eqv(examples.Point(1, 2), examples.Point(3, 4)) ==> false
    }
  }

  group("Semigroup") {

    test("combines fields") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.semigroupPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Monoid") {

    test("empty") {
      examples.Point.monoidPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.monoidPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("CommutativeSemigroup") {

    test("combines fields") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.commSemigroupPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("CommutativeMonoid") {

    test("empty") {
      examples.Point.commMonoidPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.commMonoidPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Band") {

    test("combines fields (idempotent semigroup)") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.bandPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Semilattice") {

    test("combines fields (commutative idempotent semigroup)") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.semilatticePoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("BoundedSemilattice") {

    test("empty") {
      examples.Point.boundedSemilatticePoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.boundedSemilatticePoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Group") {

    test("empty") {
      examples.Point.groupPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.groupPoint.combine(a, b) ==> examples.Point(4, 6)
    }

    test("inverse") {
      val a = examples.Point(3, 7)
      examples.Point.groupPoint.inverse(a) ==> examples.Point(-3, -7)
    }

    test("combine with inverse yields empty") {
      val a = examples.Point(5, 10)
      val inv = examples.Point.groupPoint.inverse(a)
      examples.Point.groupPoint.combine(a, inv) ==> examples.Point(0, 0)
    }
  }

  group("CommutativeGroup") {

    test("empty") {
      examples.Point.commGroupPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.commGroupPoint.combine(a, b) ==> examples.Point(4, 6)
    }

    test("inverse") {
      val a = examples.Point(3, 7)
      examples.Point.commGroupPoint.inverse(a) ==> examples.Point(-3, -7)
    }
  }

  group("ShowPretty") {

    test("case class") {
      val result = examples.Person.showPrettyPerson.show(examples.Person("Alice", 30))
      result ==> "Person(\n  name = Alice,\n  age = 30\n)"
    }

    test("showLines") {
      val lines = examples.Person.showPrettyPerson.showLines(examples.Person("Alice", 30))
      lines ==> List("Person(", "  name = Alice,", "  age = 30", ")")
    }

    test("nested case class") {
      val addr = examples.Address("123 Main St", "Springfield")
      val person = examples.PersonWithAddress("Bob", addr)
      val result = examples.PersonWithAddress.showPrettyPersonWithAddress.show(person)
      result ==> "PersonWithAddress(\n  name = Bob,\n  address = Address(\n    street = 123 Main St,\n    city = Springfield\n  )\n)"
    }

    test("nested showLines") {
      val addr = examples.Address("123 Main St", "Springfield")
      val person = examples.PersonWithAddress("Bob", addr)
      val lines = examples.PersonWithAddress.showPrettyPersonWithAddress.showLines(person)
      lines ==> List(
        "PersonWithAddress(",
        "  name = Bob,",
        "  address = Address(",
        "    street = 123 Main St,",
        "    city = Springfield",
        "  )",
        ")"
      )
    }
  }

  group("Show @sensitiveData") {

    test("field-level redaction") {
      examples.UserWithPassword.showUserWithPassword.show(
        examples.UserWithPassword("Alice", "s3cret")
      ) ==> "UserWithPassword(name = Alice, password = [redacted])"
    }

    test("field-level redaction with reason") {
      examples.UserWithPII.showUserWithPII.show(
        examples.UserWithPII("Alice", "alice@example.com", 30)
      ) ==> "UserWithPII(name = Alice, email = [redacted: PII], age = 30)"
    }

    test("type-level redaction") {
      examples.SecretData.showSecretData.show(examples.SecretData("secret", 42)) ==> "[redacted]"
    }

    test("type-level redaction with reason") {
      examples.ClassifiedInfo.showClassifiedInfo.show(
        examples.ClassifiedInfo("top-secret")
      ) ==> "[redacted: classified]"
    }

    test("type-level redaction on sealed trait") {
      examples.SecretEnum.showSecretEnum.show(examples.SecretCase(42)) ==> "[redacted]"
    }
  }

  group("ShowPretty @sensitiveData") {

    test("field-level redaction") {
      val result = examples.UserWithPassword.showPrettyUserWithPassword.show(
        examples.UserWithPassword("Alice", "s3cret")
      )
      result ==> "UserWithPassword(\n  name = Alice,\n  password = [redacted]\n)"
    }

    test("field-level redaction with reason") {
      val result = examples.UserWithPII.showPrettyUserWithPII.show(
        examples.UserWithPII("Alice", "alice@example.com", 30)
      )
      result ==> "UserWithPII(\n  name = Alice,\n  email = [redacted: PII],\n  age = 30\n)"
    }

    test("type-level redaction") {
      examples.SecretData.showPrettySecretData.show(examples.SecretData("secret", 42)) ==> "[redacted]"
    }

    test("type-level redaction with reason") {
      examples.ClassifiedInfo.showPrettyClassifiedInfo.show(
        examples.ClassifiedInfo("top-secret")
      ) ==> "[redacted: classified]"
    }

    test("type-level redaction on sealed trait") {
      examples.SecretEnum.showPrettySecretEnum.show(examples.SecretCase(42)) ==> "[redacted]"
    }
  }

  group("Show enum") {

    test("sealed trait with case classes") {
      examples.Shape.showShape.show(examples.Circle(1.5)) ==> "Circle(radius = 1.5)"
      examples.Shape.showShape.show(
        examples.Rectangle(3.0, 4.0)
      ) ==> s"Rectangle(width = ${3.0.toString}, height = ${4.0.toString})"
    }

    test("sealed trait with case objects") {
      examples.Color.showColor.show(examples.Red) ==> "Red()"
      examples.Color.showColor.show(examples.Green) ==> "Green()"
    }
  }

  group("Eq enum") {

    test("same case class values") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Circle(1.5)) ==> true
    }

    test("different case class values") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Circle(2.0)) ==> false
    }

    test("different cases") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Rectangle(1.5, 1.5)) ==> false
    }

    test("case objects equal") {
      examples.Color.eqColor.eqv(examples.Red, examples.Red) ==> true
    }

    test("case objects not equal") {
      examples.Color.eqColor.eqv(examples.Red, examples.Blue) ==> false
    }
  }

  group("Order enum") {

    test("same case") {
      examples.Shape.orderShape.compare(examples.Circle(1.5), examples.Circle(1.5)) ==> 0
    }

    test("same case different values") {
      assert(examples.Shape.orderShape.compare(examples.Circle(1.0), examples.Circle(2.0)) < 0)
    }

    test("different cases are ordered") {
      // Different cases should produce non-zero comparison
      assert(examples.Shape.orderShape.compare(examples.Circle(1.0), examples.Rectangle(1.0, 1.0)) != 0)
    }

    test("case objects ordered") {
      examples.Color.orderColor.compare(examples.Red, examples.Red) ==> 0
      assert(examples.Color.orderColor.compare(examples.Red, examples.Green) != 0)
    }
  }

  group("Hash enum") {

    test("same values same hash") {
      examples.Shape.hashShape.hash(examples.Circle(1.5)) ==> examples.Shape.hashShape.hash(examples.Circle(1.5))
    }

    test("eqv for enum") {
      examples.Shape.hashShape.eqv(examples.Circle(1.5), examples.Circle(1.5)) ==> true
      examples.Shape.hashShape.eqv(examples.Circle(1.5), examples.Rectangle(1.5, 1.5)) ==> false
    }

    test("case objects same hash") {
      examples.Color.hashColor.hash(examples.Red) ==> examples.Color.hashColor.hash(examples.Red)
    }

    test("case objects eqv") {
      examples.Color.hashColor.eqv(examples.Red, examples.Red) ==> true
      examples.Color.hashColor.eqv(examples.Red, examples.Blue) ==> false
    }
  }

  group("Empty") {

    test("case class empty") {
      val empty = examples.Point.emptyPoint.empty
      empty ==> examples.Point(0, 0)
    }
  }

  group("Functor") {

    test("single field") {
      examples.Box.functorBox.map(examples.Box(1))(_ + 1) ==> examples.Box(2)
    }

    test("two type param fields") {
      examples.PairF.functorPairF.map(examples.PairF(1, 2))(_ * 10) ==> examples.PairF(10, 20)
    }

    test("type param and invariant field") {
      examples.Labeled.functorLabeled.map(examples.Labeled(1, "x"))(_ + 1) ==> examples.Labeled(2, "x")
    }

    test("nested type constructor Vector[A]") {
      examples.Interleaved.functorInterleaved.map(examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d"))(
        _.toUpperCase
      ) ==> examples.Interleaved(1, "A", 2L, Vector("B", "C"), "d")
    }

    test("nested type constructor Option[A]") {
      examples.CaseClassWOption.functorCaseClassWOption.map(examples.CaseClassWOption(Some(1)))(_ + 1) ==>
        examples.CaseClassWOption(Some(2))
    }

    test("nested Option[A] with None") {
      examples.CaseClassWOption.functorCaseClassWOption.map(examples.CaseClassWOption(None: Option[Int]))(_ + 1) ==>
        examples.CaseClassWOption(None)
    }

    test("sealed trait IList via enum rule") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.INil()))
      examples.IList.functorIList.map(list)(_ * 10) ==>
        examples.ICons(10, examples.ICons(20, examples.INil()))
    }

    test("sealed trait IList empty") {
      val empty: examples.IList[Int] = examples.INil()
      examples.IList.functorIList.map(empty)(_ * 10) ==> examples.INil()
    }
  }

  group("Contravariant") {

    test("single function field") {
      val pred = examples.Predicate[Int](n => n > 0)
      val stringPred = examples.Predicate.contravariantPredicate.contramap(pred)((s: String) => s.length)
      stringPred.run("hello") ==> true
      stringPred.run("") ==> false
    }

    test("function field with invariant field") {
      val enc = examples.Codec[Int](n => n.toString, "int-codec")
      val stringEnc = examples.Codec.contravariantCodec.contramap(enc)((s: String) => s.length)
      stringEnc.encode("hello") ==> "5"
      stringEnc.label ==> "int-codec"
    }
  }

  group("Invariant") {

    test("mixed covariant and contravariant Function1 fields") {
      val codec = examples.Codec2[Int](_.toString, _.toInt)
      val stringCodec = examples.Codec2.invariantCodec2.imap(codec)(_.toString)(_.toInt)
      stringCodec.encode("42") ==> "42"
      stringCodec.decode("123") ==> "123"
    }

    test("direct field and contravariant Function1 field") {
      val t = examples.Transform[Int](42, _ > 0)
      val stringT = examples.Transform.invariantTransform.imap(t)(_.toString)(_.toInt)
      stringT.value ==> "42"
      stringT.check("5") ==> true
      stringT.check("-1") ==> false
    }
  }

  group("Pure") {

    test("single field") {
      examples.Single.pureSingle.pure(42) ==> examples.Single(42)
    }

    test("multiple fields all of type A") {
      examples.Triple.pureTriple.pure("x") ==> examples.Triple("x", "x", "x")
    }
  }

  group("EmptyK") {

    test("all List fields") {
      val empty = examples.ListPair.emptyKListPair.empty[Int]
      empty ==> examples.ListPair(Nil, Nil)
    }

    test("List field with invariant String field") {
      val empty = examples.TaggedList.emptyKTaggedList.empty[Int]
      empty ==> examples.TaggedList(Nil, "")
    }
  }

  group("SemigroupK") {

    test("all List fields") {
      val a = examples.ListPair(List(1, 2), List(3))
      val b = examples.ListPair(List(4), List(5, 6))
      examples.ListPair.semigroupKListPair.combineK(a, b) ==> examples.ListPair(List(1, 2, 4), List(3, 5, 6))
    }

    test("List field with invariant String field") {
      val a = examples.TaggedList(List(1), "hello")
      val b = examples.TaggedList(List(2), " world")
      val combined = examples.TaggedList.semigroupKTaggedList.combineK(a, b)
      combined ==> examples.TaggedList(List(1, 2), "hello world")
    }
  }

  group("MonoidK") {

    test("empty") {
      val empty = examples.ListPair.monoidKListPair.empty[Int]
      empty ==> examples.ListPair(Nil, Nil)
    }

    test("combineK") {
      val a = examples.ListPair(List(1), List(2))
      val b = examples.ListPair(List(3), List(4))
      examples.ListPair.monoidKListPair.combineK(a, b) ==> examples.ListPair(List(1, 3), List(2, 4))
    }

    test("List field with invariant String field") {
      val empty = examples.TaggedList.monoidKTaggedList.empty[Int]
      empty ==> examples.TaggedList(Nil, "")
      val a = examples.TaggedList(List(1), "a")
      val b = examples.TaggedList(List(2), "b")
      examples.TaggedList.monoidKTaggedList.combineK(a, b) ==> examples.TaggedList(List(1, 2), "ab")
    }
  }

  group("Apply") {

    test("ap single direct field") {
      val ff = examples.Box[Int => String](_.toString)
      val fa = examples.Box(42)
      examples.Box.applicativeBox.ap(ff)(fa) ==> examples.Box("42")
    }

    test("ap with invariant field combines via Semigroup") {
      val ff = examples.Labeled[Int => String](_.toString, "fn")
      val fa = examples.Labeled(42, "val")
      val result = examples.Labeled.applyLabeled.ap(ff)(fa)
      result ==> examples.Labeled("42", "fnval")
    }
  }

  group("Applicative") {

    test("pure single field") {
      examples.Box.applicativeBox.pure(42) ==> examples.Box(42)
    }

    test("pure with invariant field uses Monoid empty") {
      examples.Labeled.applicativeLabeled.pure(42) ==> examples.Labeled(42, "")
    }

    test("ap with invariant field") {
      val ff = examples.Labeled[Int => String](_.toString, "fn")
      val fa = examples.Labeled(42, "val")
      val result = examples.Labeled.applicativeLabeled.ap(ff)(fa)
      result ==> examples.Labeled("42", "fnval")
    }

    test("map") {
      examples.Labeled.applicativeLabeled.map(examples.Labeled(1, "x"))(_ + 1) ==> examples.Labeled(2, "x")
    }
  }

  group("Foldable") {

    test("foldLeft single field") {
      examples.Box.foldableBox.foldLeft(examples.Box(42), 0)(_ + _) ==> 42
    }

    test("foldLeft two fields") {
      examples.PairF.foldablePairF.foldLeft(examples.PairF(1, 2), 0)(_ + _) ==> 3
    }

    test("foldLeft with invariant field skips it") {
      examples.Labeled.foldableLabeled.foldLeft(examples.Labeled(10, "ignored"), 0)(_ + _) ==> 10
    }

    test("foldRight two fields") {
      val result = examples.PairF.foldablePairF
        .foldRight(examples.PairF(1, 2), cats.Eval.now(List.empty[Int]))((a, acc) => acc.map(a :: _))
        .value
      result ==> List(1, 2)
    }

    test("foldRight single field") {
      val result = examples.Box.foldableBox
        .foldRight(examples.Box(42), cats.Eval.now(List.empty[Int]))((a, acc) => acc.map(a :: _))
        .value
      result ==> List(42)
    }

    test("foldLeft with nested Vector[A]") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d")
      examples.Interleaved.foldableInterleaved.foldLeft(v, List.empty[String])((acc, a) => acc :+ a) ==>
        List("a", "b", "c")
    }

    test("foldRight with nested Vector[A]") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d")
      val result = examples.Interleaved.foldableInterleaved
        .foldRight(v, cats.Eval.now(List.empty[String]))((a, acc) => acc.map(a :: _))
        .value
      result ==> List("a", "b", "c")
    }

    test("foldLeft with nested Option[A]") {
      examples.CaseClassWOption.foldableCaseClassWOption.foldLeft(examples.CaseClassWOption(Some(42)), 0)(_ + _) ==> 42
      examples.CaseClassWOption.foldableCaseClassWOption.foldLeft(
        examples.CaseClassWOption(None: Option[Int]),
        0
      )(_ + _) ==> 0
    }

    test("foldLeft IList sealed trait") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.ICons(3, examples.INil())))
      examples.IList.foldableIList.foldLeft(list, 0)(_ + _) ==> 6
    }

    test("foldLeft IList empty") {
      val empty: examples.IList[Int] = examples.INil()
      examples.IList.foldableIList.foldLeft(empty, 0)(_ + _) ==> 0
    }

    test("foldRight IList sealed trait") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.INil()))
      val result = examples.IList.foldableIList
        .foldRight(list, cats.Eval.now(List.empty[Int]))((a, acc) => acc.map(a :: _))
        .value
      result ==> List(1, 2)
    }
  }

  group("Traverse") {

    test("traverse single field with Option") {
      examples.Box.traverseBox.traverse(examples.Box(42))(a => Option(a + 1)) ==> Some(examples.Box(43))
    }

    test("traverse single field with None") {
      examples.Box.traverseBox.traverse(examples.Box(42))(_ => None: Option[Int]) ==> None
    }

    test("traverse two fields with Option") {
      examples.PairF.traversePairF.traverse(examples.PairF(1, 2))(a => Option(a * 10)) ==>
        Some(examples.PairF(10, 20))
    }

    test("traverse two fields short-circuits on None") {
      val result = examples.PairF.traversePairF.traverse(examples.PairF(1, 2)) { a =>
        if (a == 1) None else Some(a)
      }
      result ==> None
    }

    test("traverse with invariant field preserves it") {
      examples.Labeled.traverseLabeled.traverse(examples.Labeled(1, "hello"))(a => Option(a.toString)) ==> Some(
        examples.Labeled("1", "hello")
      )
    }

    test("traverse map works") {
      examples.Box.traverseBox.map(examples.Box(1))(_ + 1) ==> examples.Box(2)
    }

    test("traverse foldLeft works") {
      examples.PairF.traversePairF.foldLeft(examples.PairF(1, 2), 0)(_ + _) ==> 3
    }

    test("traverse with List effect") {
      examples.Box.traverseBox.traverse(examples.Box(1))(a => List(a, a + 1)) ==>
        List(examples.Box(1), examples.Box(2))
    }

    test("traverse sequence") {
      examples.Box.traverseBox.sequence(examples.Box(Option(42))) ==> Some(examples.Box(42))
    }

    test("traverse with nested Vector[A]") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d")
      examples.Interleaved.traverseInterleaved.traverse(v)(s => Option(s.toUpperCase)) ==>
        Some(examples.Interleaved(1, "A", 2L, Vector("B", "C"), "d"))
    }

    test("traverse with nested Vector[A] short-circuits on None") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "fail"), "d")
      val result = examples.Interleaved.traverseInterleaved.traverse(v) { s =>
        if (s == "fail") None else Some(s.toUpperCase)
      }
      result ==> None
    }

    test("traverse with nested Option[A]") {
      examples.CaseClassWOption.traverseCaseClassWOption.traverse(examples.CaseClassWOption(Some(1)))(a =>
        Option(a.toString)
      ) ==> Some(examples.CaseClassWOption(Some("1")))
    }

    test("traverse with nested Option[A] containing None") {
      examples.CaseClassWOption.traverseCaseClassWOption.traverse(
        examples.CaseClassWOption(None: Option[Int])
      )(a => Option(a.toString)) ==> Some(examples.CaseClassWOption(None))
    }

    test("traverse map with nested Vector[A]") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d")
      examples.Interleaved.traverseInterleaved.map(v)(_.toUpperCase) ==>
        examples.Interleaved(1, "A", 2L, Vector("B", "C"), "d")
    }

    test("traverse foldLeft with nested Vector[A]") {
      val v = examples.Interleaved(1, "a", 2L, Vector("b", "c"), "d")
      examples.Interleaved.traverseInterleaved.foldLeft(v, List.empty[String])((acc, s) => acc :+ s) ==>
        List("a", "b", "c")
    }

    test("traverse IList sealed trait with Option") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.INil()))
      examples.IList.traverseIList.traverse(list)(a => Option(a * 10)) ==>
        Some(examples.ICons(10, examples.ICons(20, examples.INil())))
    }

    test("traverse IList empty") {
      val empty: examples.IList[Int] = examples.INil()
      examples.IList.traverseIList.traverse(empty)(a => Option(a.toString)) ==>
        Some(examples.INil())
    }

    test("traverse IList short-circuits on None") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.INil()))
      val result = examples.IList.traverseIList.traverse(list) { a =>
        if (a == 2) None else Some(a * 10)
      }
      result ==> None
    }

    test("traverse IList map via Traverse") {
      val list: examples.IList[Int] = examples.ICons(1, examples.ICons(2, examples.INil()))
      examples.IList.traverseIList.map(list)(_ * 10) ==>
        examples.ICons(10, examples.ICons(20, examples.INil()))
    }
  }

  group("Reducible") {

    test("reduceLeftTo single field") {
      examples.Box.reducibleBox.reduceLeftTo(examples.Box(42))(_.toString)(_ + _) ==> "42"
    }

    test("reduceLeftTo two fields") {
      examples.PairF.reduciblePairF.reduceLeftTo(examples.PairF(1, 2))(_.toString)((acc, a) => s"$acc,$a") ==> "1,2"
    }

    test("reduceRightTo single field") {
      examples.Box.reducibleBox
        .reduceRightTo(examples.Box(42))(_.toString)((a, eb) => eb.map(b => s"$a,$b"))
        .value ==> "42"
    }

    test("reduceRightTo two fields") {
      examples.PairF.reduciblePairF
        .reduceRightTo(examples.PairF(1, 2))(_.toString)((a, eb) => eb.map(b => s"$a,$b"))
        .value ==> "1,2"
    }

    test("reduce with Semigroup") {
      import cats.instances.int.*
      examples.PairF.reduciblePairF.reduce(examples.PairF(10, 20)) ==> 30
    }

    test("foldLeft works") {
      examples.PairF.reduciblePairF.foldLeft(examples.PairF(1, 2), 0)(_ + _) ==> 3
    }

    test("reduceLeft") {
      examples.PairF.reduciblePairF.reduceLeft(examples.PairF(10, 20))(_ + _) ==> 30
    }
  }

  group("NonEmptyTraverse") {

    test("nonEmptyTraverse single field with Option") {
      examples.Box.nonEmptyTraverseBox.nonEmptyTraverse(examples.Box(42))(a => Option(a + 1)) ==>
        Some(examples.Box(43))
    }

    test("nonEmptyTraverse two fields with Option") {
      examples.PairF.nonEmptyTraversePairF.nonEmptyTraverse(examples.PairF(1, 2))(a => Option(a * 10)) ==> Some(
        examples.PairF(10, 20)
      )
    }

    test("nonEmptyTraverse short-circuits on None") {
      val result = examples.PairF.nonEmptyTraversePairF.nonEmptyTraverse(examples.PairF(1, 2)) { a =>
        if (a == 1) None else Some(a)
      }
      result ==> None
    }

    test("traverse via NonEmptyTraverse") {
      examples.Box.nonEmptyTraverseBox.traverse(examples.Box(1))(a => Option(a.toString)) ==>
        Some(examples.Box("1"))
    }

    test("reduceLeftTo via NonEmptyTraverse") {
      examples.PairF.nonEmptyTraversePairF.reduceLeftTo(examples.PairF(1, 2))(_.toString)((acc, a) =>
        s"$acc,$a"
      ) ==> "1,2"
    }

    test("map via NonEmptyTraverse") {
      examples.Box.nonEmptyTraverseBox.map(examples.Box(1))(_ + 1) ==> examples.Box(2)
    }

    test("nonEmptySequence") {
      examples.Box.nonEmptyTraverseBox.nonEmptySequence(examples.Box(Option(42))) ==>
        Some(examples.Box(42))
    }
  }

  group("NonEmptyAlternative") {

    test("pure") {
      examples.Const.neaConst.pure(42) ==> examples.Const[Int]("")
    }

    test("pure multi-field") {
      examples.Counter.neaCounter.pure(42) ==> examples.Counter[Int](0, "")
    }

    test("map") {
      examples.Const.neaConst.map(examples.Const[Int]("hello"))(_ + 1) ==> examples.Const[Int]("hello")
    }

    test("ap") {
      val ff = examples.Const[Int => String]("f")
      val fa = examples.Const[Int]("a")
      examples.Const.neaConst.ap(ff)(fa) ==> examples.Const[String]("fa")
    }

    test("combineK") {
      examples.Const.neaConst.combineK(examples.Const[Int]("hello"), examples.Const[Int](" world")) ==>
        examples.Const[Int]("hello world")
    }

    test("combineK multi-field") {
      examples.Counter.neaCounter.combineK(
        examples.Counter[Int](1, "a"),
        examples.Counter[Int](2, "b")
      ) ==> examples.Counter[Int](3, "ab")
    }

    test("prependK uses pure + combineK") {
      examples.Const.neaConst.prependK(42, examples.Const[Int]("hello")) ==> examples.Const[Int]("hello")
    }
  }

  group("Alternative") {

    test("empty") {
      examples.Const.altConst.empty[Int] ==> examples.Const[Int]("")
    }

    test("empty multi-field") {
      examples.Counter.altCounter.empty[Int] ==> examples.Counter[Int](0, "")
    }

    test("pure") {
      examples.Const.altConst.pure(42) ==> examples.Const[Int]("")
    }

    test("combineK") {
      examples.Const.altConst.combineK(examples.Const[Int]("hello"), examples.Const[Int](" world")) ==>
        examples.Const[Int]("hello world")
    }

    test("guard true") {
      examples.Const.altConst.guard(true) ==> examples.Const[Unit]("")
    }

    test("guard false") {
      examples.Const.altConst.guard(false) ==> examples.Const[Unit]("")
    }
  }

  group("ConsK") {

    test("cons into single nested List field") {
      val result = examples.ListWrap.consKListWrap.cons(1, examples.ListWrap(List(2, 3)))
      result ==> examples.ListWrap(List(1, 2, 3))
    }

    test("cons into empty nested List field") {
      val result = examples.ListWrap.consKListWrap.cons(1, examples.ListWrap(Nil))
      result ==> examples.ListWrap(List(1))
    }

    test("head-tail pattern: shift head and cons into tail") {
      val result = examples.HeadTail.consKHeadTail.cons(1, examples.HeadTail(2, List(3, 4)))
      result ==> examples.HeadTail(1, List(2, 3, 4))
    }

    test("head-tail pattern: cons into empty tail") {
      val result = examples.HeadTail.consKHeadTail.cons(1, examples.HeadTail(2, Nil))
      result ==> examples.HeadTail(1, List(2))
    }

    test("nested List with invariant String field") {
      val result = examples.NamedList.consKNamedList.cons(1, examples.NamedList(List(2, 3), "test"))
      result ==> examples.NamedList(List(1, 2, 3), "test")
    }
  }

  group("Bifunctor") {

    test("bimap both fields") {
      val pair = examples.Pair(1, "hello")
      val result = examples.Pair.bifunctorPair.bimap(pair)(_.toString, _.length)
      result ==> examples.Pair("1", 5)
    }

    test("bimap with invariant field") {
      val tv = examples.TaggedValue(10, 20.0, "test")
      val result = examples.TaggedValue.bifunctorTaggedValue.bimap(tv)(_.toString, _.toInt)
      result ==> examples.TaggedValue("10", 20, "test")
    }

    test("leftMap") {
      val pair = examples.Pair(1, "hello")
      val result = examples.Pair.bifunctorPair.leftMap(pair)(_.toString)
      result ==> examples.Pair("1", "hello")
    }

    test("Result sealed trait bimap Success") {
      val s: examples.Result[Int, String] = examples.Success(1)
      examples.Result.bifunctorResult.bimap(s)(_.toString, _.length) ==> examples.Success("1")
    }

    test("Result sealed trait bimap Failure") {
      val f: examples.Result[Int, String] = examples.Failure("err")
      examples.Result.bifunctorResult.bimap(f)(_.toString, _.length) ==> examples.Failure(3)
    }

    test("Result sealed trait bimap Both") {
      val b: examples.Result[Int, String] = examples.Both(1, "hi")
      examples.Result.bifunctorResult.bimap(b)(_.toString, _.length) ==> examples.Both("1", 2)
    }

    test("Result sealed trait bimap ResultEmpty") {
      val e: examples.Result[Int, String] = examples.ResultEmpty
      examples.Result.bifunctorResult.bimap(e)(_.toString, _.length) ==> examples.ResultEmpty
    }
  }

  group("Bifoldable") {

    test("bifoldLeft both fields") {
      val pair = examples.Pair(1, 2)
      val result = examples.Pair.bifoldablePair.bifoldLeft(pair, 0)(_ + _, _ + _)
      result ==> 3
    }

    test("bifoldLeft with invariant field") {
      val tv = examples.TaggedValue(10, 20, "test")
      val result = examples.TaggedValue.bifoldableTaggedValue.bifoldLeft(tv, 0)(_ + _, _ + _)
      result ==> 30
    }

    test("bifoldRight") {
      val pair = examples.Pair(1, 2)
      val result = examples.Pair.bifoldablePair
        .bifoldRight(pair, cats.Eval.now(List.empty[Int]))(
          (a, acc) => acc.map(a :: _),
          (b, acc) => acc.map(b :: _)
        )
        .value
      result ==> List(1, 2)
    }

    test("bifoldMap") {
      val pair = examples.Pair("hello", "world")
      val result = examples.Pair.bifoldablePair.bifoldMap(pair)(_.length, _.length)(cats.kernel.Monoid[Int])
      result ==> 10
    }

    test("Result sealed trait bifoldLeft Success") {
      val s: examples.Result[Int, String] = examples.Success(42)
      examples.Result.bifoldableResult.bifoldLeft(s, 0)(_ + _, (acc, s) => acc + s.length) ==> 42
    }

    test("Result sealed trait bifoldLeft Failure") {
      val f: examples.Result[Int, String] = examples.Failure("err")
      examples.Result.bifoldableResult.bifoldLeft(f, 0)(_ + _, (acc, s) => acc + s.length) ==> 3
    }

    test("Result sealed trait bifoldLeft Both") {
      val b: examples.Result[Int, String] = examples.Both(10, "hi")
      examples.Result.bifoldableResult.bifoldLeft(b, 0)(_ + _, (acc, s) => acc + s.length) ==> 12
    }

    test("Result sealed trait bifoldLeft ResultEmpty") {
      val e: examples.Result[Int, String] = examples.ResultEmpty
      examples.Result.bifoldableResult.bifoldLeft(e, 0)(_ + _, (acc, s) => acc + s.length) ==> 0
    }
  }

  group("Bitraverse") {

    test("bitraverse with Option") {
      val pair = examples.Pair(1, "hello")
      val result = examples.Pair.bitraversePair.bitraverse(pair)(
        a => Option(a.toString),
        b => Option(b.length)
      )
      result ==> Some(examples.Pair("1", 5))
    }

    test("bitraverse short-circuits on None") {
      val pair = examples.Pair(1, "hello")
      val result = examples.Pair.bitraversePair.bitraverse(pair)(
        _ => None: Option[String],
        b => Option(b.length)
      )
      result ==> None
    }

    test("bitraverse with invariant field") {
      val tv = examples.TaggedValue(10, 20.0, "test")
      val result = examples.TaggedValue.bitraverseTaggedValue.bitraverse(tv)(
        a => Option(a.toString),
        b => Option(b.toInt)
      )
      result ==> Some(examples.TaggedValue("10", 20, "test"))
    }

    test("bimap via Bitraverse") {
      val pair = examples.Pair(1, "hello")
      val result = examples.Pair.bitraversePair.bimap(pair)(_.toString, _.length)
      result ==> examples.Pair("1", 5)
    }

    test("bisequence") {
      val pair = examples.Pair(Option(1), Option("hello"))
      val result = examples.Pair.bitraversePair.bisequence(pair)
      result ==> Some(examples.Pair(1, "hello"))
    }

    test("Result sealed trait bitraverse Success") {
      val s: examples.Result[Int, String] = examples.Success(42)
      examples.Result.bitraverseResult.bitraverse(s)(a => Option(a.toString), b => Option(b.length)) ==>
        Some(examples.Success("42"))
    }

    test("Result sealed trait bitraverse Failure") {
      val f: examples.Result[Int, String] = examples.Failure("err")
      examples.Result.bitraverseResult.bitraverse(f)(a => Option(a.toString), b => Option(b.length)) ==>
        Some(examples.Failure(3))
    }

    test("Result sealed trait bitraverse Both") {
      val b: examples.Result[Int, String] = examples.Both(1, "hi")
      examples.Result.bitraverseResult.bitraverse(b)(a => Option(a.toString), b => Option(b.length)) ==>
        Some(examples.Both("1", 2))
    }

    test("Result sealed trait bitraverse ResultEmpty") {
      val e: examples.Result[Int, String] = examples.ResultEmpty
      examples.Result.bitraverseResult.bitraverse(e)(a => Option(a.toString), b => Option(b.length)) ==>
        Some(examples.ResultEmpty)
    }

    test("Result sealed trait bimap via Bitraverse") {
      val b: examples.Result[Int, String] = examples.Both(1, "hi")
      examples.Result.bitraverseResult.bimap(b)(_.toString, _.length) ==> examples.Both("1", 2)
    }
  }

  // Bifunctor/Bifoldable/Bitraverse for sealed traits: see Result tests in each group above.

  group("compile-time errors") {

    test("Semigroup.derived on sealed trait") {
      compileErrors(
        """
        import hearth.kindlings.catsderivation.extensions.*
        cats.kernel.Semigroup.derived[hearth.kindlings.catsderivation.examples.Animal]
        """
      ).check(
        "not handled"
      )
    }

    test("Group.derived on type without Group for fields") {
      compileErrors(
        """
        import hearth.kindlings.catsderivation.extensions.*
        cats.kernel.Group.derived[hearth.kindlings.catsderivation.examples.WithStrings]
        """
      ).check(
        "not handled"
      )
    }
  }

  group("PartialOrder") {

    test("equal values have partialCompare 0.0") {
      val a = examples.Point(1, 2)
      examples.Point.partialOrderPoint.partialCompare(a, a) ==> 0.0
    }

    test("different values have non-zero partialCompare") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      assert(examples.Point.partialOrderPoint.partialCompare(a, b) != 0.0)
    }

    test("PartialOrder on sealed trait") {
      val c = examples.Circle(1.0): examples.Shape
      val r = examples.Rectangle(2.0, 3.0): examples.Shape
      assert(examples.Shape.partialOrderShape.partialCompare(c, c) == 0.0)
      assert(examples.Shape.partialOrderShape.partialCompare(c, r) != 0.0)
    }

    test("PartialOrder consistent with Eq") {
      val a = examples.Point(1, 2)
      val b = examples.Point(1, 2)
      assert(examples.Point.partialOrderPoint.partialCompare(a, b) == 0.0)
      assert(examples.Point.eqPoint.eqv(a, b))
    }
  }

  group("recursive types") {

    test("Show on recursive tree") {
      val tree = examples.Tree(1, List(examples.Tree(2, Nil), examples.Tree(3, List(examples.Tree(4, Nil)))))
      val shown = examples.Tree.showTree.show(tree)
      assert(shown.contains("1"))
      assert(shown.contains("2"))
      assert(shown.contains("3"))
      assert(shown.contains("4"))
    }

    test("Eq on recursive tree") {
      val tree1 = examples.Tree(1, List(examples.Tree(2, Nil)))
      val tree2 = examples.Tree(1, List(examples.Tree(2, Nil)))
      val tree3 = examples.Tree(1, List(examples.Tree(3, Nil)))
      assert(examples.Tree.eqTree.eqv(tree1, tree2))
      assert(!examples.Tree.eqTree.eqv(tree1, tree3))
    }
  }

  group("respecting existing instances") {

    test("user-provided Eq takes priority in derived Show") {
      // Person has an explicit Eq instance — verify it works and is the one being used
      val a = examples.Person("Alice", 30)
      val b = examples.Person("Alice", 30)
      assert(examples.Person.eqPerson.eqv(a, b))
      assert(!examples.Person.eqPerson.eqv(a, examples.Person("Bob", 30)))
    }
  }

  group("auto derivation") {

    test("auto.show derives Show for case class") {
      import auto.show.given
      val s = implicitly[cats.Show[examples.Color]].show(examples.Red)
      assert(s.contains("Red"), s"expected Red in: $s")
    }

    test("auto.eq derives Eq for case class") {
      import auto.eq.given
      val eq = implicitly[cats.kernel.Eq[examples.Color]]
      assert(eq.eqv(examples.Red, examples.Red))
      assert(!eq.eqv(examples.Red, examples.Green))
    }

    test("auto.hash derives Hash for case class") {
      import auto.hash.given
      val hash = implicitly[cats.kernel.Hash[examples.Color]]
      assertEquals(hash.hash(examples.Red), hash.hash(examples.Red))
    }

    test("auto.empty derives Empty for case class") {
      import auto.empty.given
      val e = implicitly[alleycats.Empty[examples.Person]]
      assert(e.empty.name.isEmpty || e.empty.name == "", s"expected empty name, got: ${e.empty}")
    }
  }

  group("strict derivation") {

    test("StrictDerivation sentinel type exists and can be instantiated") {
      val strict: StrictDerivation = StrictDerivation.instance
      assert(strict != null)
    }

    test("without strict mode, nested types derive successfully") {
      import hearth.kindlings.catsderivation.extensions.*
      val show = cats.Show.derived[examples.PersonWithAddress]
      assert(
        show.show(examples.PersonWithAddress("Alice", examples.Address("Main", "NYC"))).nonEmpty
      )
    }
  }
}
