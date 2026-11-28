package hearth.kindlings.yamlderivation

import hearth.MacroSuite
import org.virtuslab.yaml.{ConstructError, Node}

/** `YamlConfig.accumulatingErrors`: fail fast (the default) vs accumulating every failure with its path. */
final class DecodingErrorModesSpec extends MacroSuite {

  private def node(yaml: String): Node =
    internal.runtime.YamlDerivationUtils.parseAndDecode[Node](yaml, n => Right(n)) match {
      case Right(n)  => n
      case Left(err) => throw new IllegalArgumentException(err.msg)
    }

  private def paths(error: ConstructError): List[String] = YamlDecodingFailures.of(error).map(_.path)

  private def errorOf[A](result: Either[ConstructError, A]): ConstructError = result match {
    case Left(error)  => error
    case Right(value) => fail(s"expected a failure, got $value")
  }

  private val validPerson =
    """name: Alice
      |age: 30
      |address:
      |  street: Main
      |  zip: 12345
      |tags: [1, 2]
      |scores:
      |  a: 1
      |nick: 7
      |""".stripMargin

  private val brokenPerson =
    """name: Alice
      |age: thirty
      |address:
      |  street: Main
      |  zip: abc
      |tags: [1, x, 3, y]
      |scores:
      |  a: one
      |  b: 2
      |nick: seven
      |retries: many
      |""".stripMargin

  private val expectedPersonPaths =
    List(".age", ".address.zip", ".tags[1]", ".tags[3]", ".scores.a", ".nick", ".retries")

  private val person = ErrPerson("Alice", 30, ErrAddress("Main", 12345), List(1, 2), Map("a" -> 1), Some(7))

  group("the configuration") {

    test("fail fast is the default") {
      assert(!YamlConfig.default.accumulatingErrors)
      assert(!YamlConfig().accumulatingErrors)
    }

    test("withAccumulatingErrors / withFailFastErrors, kept by the other options and in equality") {
      // one base: on Scala.js each `YamlConfig()` gets its own `identity` function instances (not equal)
      val base = YamlConfig()
      val accumulating = base.withAccumulatingErrors
      assert(accumulating.accumulatingErrors)
      assert(accumulating.withSnakeCaseMemberNames.withDiscriminator("t").accumulatingErrors)
      assert(accumulating.copy(useDefaults = false).accumulatingErrors)
      assert(!accumulating.withFailFastErrors.accumulatingErrors)
      assert(accumulating != base)
      accumulating ==> base.withAccumulatingErrors
      accumulating.hashCode ==> base.withAccumulatingErrors.hashCode
      accumulating.withFailFastErrors ==> base
      assert(accumulating.toString.contains("accumulatingErrors=true"))
      base.toString ==> "YamlConfig(" + base.productIterator.mkString(",") + ")"
    }
  }

  group("fail fast (the default)") {

    test("valid input decodes") {
      ErrorModeInstances.FailFast.person.construct(node(validPerson)) ==> Right(person)
      ErrorModeInstances.Runtime.person(YamlConfig()).construct(node(validPerson)) ==> Right(person)
    }

    test("only the first failure (in field order) is reported, as it is (no path, no aggregation)") {
      val error = errorOf(ErrorModeInstances.FailFast.person.construct(node(brokenPerson)))
      assert(error.getCause == null, error.getCause)
      assert(error.errorMsg.contains("thirty"), error.errorMsg)
      paths(error) ==> List("")
      ErrorModeInstances.Runtime.person(YamlConfig()).construct(node(brokenPerson)) ==> Left(error)
      KindlingsYamlDecoder.decode[ErrPerson](node(brokenPerson)) ==> Left(error)
    }

    test("the fields after the first failure are not decoded") {
      Counted.calls.set(0)
      val result = ErrorModeInstances.FailFast.counted.construct(node("first: x\nsecond: 2\nthird: 3"))
      assert(result.isLeft)
      Counted.calls.get ==> 0
      Counted.calls.set(0)
      val runtime = ErrorModeInstances.Runtime.counted(YamlConfig()).construct(node("first: x\nsecond: 2\nthird: 3"))
      assert(runtime.isLeft)
      Counted.calls.get ==> 0
      Counted.calls.set(0)
      ErrorModeInstances.FailFast.counted.construct(node("first: 1\nsecond: x\nthird: 3")).isLeft ==> true
      Counted.calls.get ==> 1
    }

    test("nested failure: the first one only") {
      val error =
        errorOf(ErrorModeInstances.FailFast.person.construct(node(brokenPerson.replace("age: thirty", "age: 30"))))
      assert(error.errorMsg.contains("abc"), error.errorMsg)
      paths(error) ==> List("")
    }

    test("collection failure: the first item only") {
      val yaml = validPerson.replace("tags: [1, 2]", "tags: [1, x, 3, y]")
      val error = errorOf(ErrorModeInstances.FailFast.person.construct(node(yaml)))
      assert(error.errorMsg.contains("x"), error.errorMsg)
      paths(error) ==> List("")
    }

    test("sealed trait subtype failure: the first one only") {
      val yaml =
        """title: t
          |shapes:
          |  - type: ErrCircle
          |    radius: big
          |  - type: ErrRect
          |    w: a
          |    h: b
          |""".stripMargin
      val error = errorOf(ErrorModeInstances.FailFast.drawing.construct(node(yaml)))
      assert(error.errorMsg.contains("big"), error.errorMsg)
      paths(error) ==> List("")
    }
  }

  group("accumulating") {

    test("valid input decodes") {
      ErrorModeInstances.Accumulating.person.construct(node(validPerson)) ==> Right(person)
      ErrorModeInstances.Runtime.person(YamlConfig().withAccumulatingErrors).construct(node(validPerson)) ==>
        Right(person)
    }

    test("every failure is reported with its path: fields, nested, collection items, map values, Option, defaults") {
      val error = errorOf(ErrorModeInstances.Accumulating.person.construct(node(brokenPerson)))
      paths(error) ==> expectedPersonPaths
      val message = error.getMessage
      assert(message.startsWith("Decoding failed with 7 errors:"), message)
      expectedPersonPaths.foreach(path => assert(message.contains(s"\n - $path: "), s"$path in:\n$message"))
      assert(message.contains("thirty"), message)
      // the individual failures are the ones the fields' decoders reported
      val failFastAge = errorOf(ErrorModeInstances.FailFast.person.construct(node(brokenPerson)))
      YamlDecodingFailures.of(error).head ==> YamlDecodingFailure(".age", failFastAge)
    }

    test("a configuration passed explicitly (evaluated at compile time) gives the same failures") {
      val static = errorOf(YamlStaticErrorModeInstances.accumulating.construct(node(brokenPerson)))
      paths(static) ==> expectedPersonPaths
      val failFast = errorOf(YamlStaticErrorModeInstances.failFast.construct(node(brokenPerson)))
      failFast ==> errorOf(ErrorModeInstances.FailFast.person.construct(node(brokenPerson)))
      paths(failFast) ==> List("")
    }

    test("a configuration only known at runtime gives the same failures") {
      val static = errorOf(ErrorModeInstances.Accumulating.person.construct(node(brokenPerson)))
      val runtime =
        errorOf(ErrorModeInstances.Runtime.person(YamlConfig().withAccumulatingErrors).construct(node(brokenPerson)))
      YamlDecodingFailures.of(runtime) ==> YamlDecodingFailures.of(static)
      runtime.getMessage ==> static.getMessage
    }

    test("the inline entry points accumulate too") {
      implicit val config: YamlConfig = YamlConfig().withAccumulatingErrors
      paths(errorOf(KindlingsYamlDecoder.decode[ErrPerson](node(brokenPerson)))) ==> expectedPersonPaths
      KindlingsYamlDecoder.fromYamlString[ErrPerson](brokenPerson) match {
        case Left(error: ConstructError) => paths(error) ==> expectedPersonPaths
        case other                       => fail(s"expected a ConstructError, got $other")
      }
    }

    test("a single failure is reported with its path too") {
      val error =
        errorOf(ErrorModeInstances.Accumulating.person.construct(node(validPerson.replace("zip: 12345", "zip: x"))))
      paths(error) ==> List(".address.zip")
      assert(error.getMessage.startsWith("Decoding failed with 1 error:"), error.getMessage)
    }

    test("missing keys are failures of their fields; absent Options and defaults are not") {
      val error =
        errorOf(ErrorModeInstances.Accumulating.person.construct(node("age: 1\naddress: {}\ntags: []\nscores: {}")))
      paths(error) ==> List(".name", ".address.street", ".address.zip")
      assert(error.getMessage.contains("Key name doesn't exist"), error.getMessage)
    }

    test("every field is decoded") {
      Counted.calls.set(0)
      val error = errorOf(ErrorModeInstances.Accumulating.counted.construct(node("first: x\nsecond: y\nthird: 3")))
      Counted.calls.get ==> 2
      paths(error) ==> List(".first", ".second")
      Counted.calls.set(0)
      val runtime = errorOf(
        ErrorModeInstances.Runtime
          .counted(YamlConfig().withAccumulatingErrors)
          .construct(node("first: x\nsecond: y\nthird: 3"))
      )
      Counted.calls.get ==> 2
      paths(runtime) ==> List(".first", ".second")
    }

    test("sealed trait subtypes (with a discriminator) in a collection") {
      val yaml =
        """title: t
          |shapes:
          |  - type: ErrCircle
          |    radius: big
          |  - type: ErrRect
          |    w: a
          |    h: b
          |  - type: ErrRect
          |    w: 1
          |    h: 2
          |""".stripMargin
      val expected = List(".shapes[0].radius", ".shapes[1].w", ".shapes[1].h")
      paths(errorOf(ErrorModeInstances.AccumulatingDiscriminator.drawing.construct(node(yaml)))) ==> expected
      val runtime = ErrorModeInstances.Runtime.drawing(YamlConfig(discriminator = Some("type")).withAccumulatingErrors)
      paths(errorOf(runtime.construct(node(yaml)))) ==> expected
    }

    test("map keys and values") {
      val yaml =
        """byId:
          |  1:
          |    street: s
          |    zip: z
          |  two:
          |    street: s
          |    zip: 2
          |  3:
          |    street: s
          |    zip: 3
          |""".stripMargin
      val error = errorOf(ErrorModeInstances.Accumulating.intKeys.construct(node(yaml)))
      paths(error).sorted ==> List(".byId.1.zip", ".byId.two")
    }

    test("a failure that is not of a field (the node is not a mapping) is reported as it is") {
      val error = errorOf(ErrorModeInstances.Accumulating.person.construct(node("just a string")))
      paths(error) ==> List("")
      assert(error.errorMsg.contains("Expected MappingNode"), error.errorMsg)
    }
  }
}
