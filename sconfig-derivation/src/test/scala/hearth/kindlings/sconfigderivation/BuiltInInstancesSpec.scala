package hearth.kindlings.sconfigderivation

import hearth.MacroSuite
import hearth.fp.data.NonEmptyList
import org.ekrich.config.{ConfigFactory, ConfigValueFactory, ConfigValueType}

final class BuiltInInstancesSpec extends MacroSuite {

  // ---------- ConfigReader built-in instances ----------

  group("ConfigReader built-in instances") {

    group("Boolean") {
      test("reads true") {
        val r = ConfigReader[Boolean]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Boolean.TRUE)) ==> Right(true)
      }
      test("reads false") {
        val r = ConfigReader[Boolean]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Boolean.FALSE)) ==> Right(false)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Boolean]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-boolean"))
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.WrongType])
      }
    }

    group("Int") {
      test("reads integer") {
        val r = ConfigReader[Int]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(42))) ==> Right(42)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Int]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.WrongType])
      }
    }

    group("Long") {
      test("reads long") {
        val r = ConfigReader[Long]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Long.valueOf(123456789L))) ==> Right(123456789L)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Long]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
      }
    }

    group("Short") {
      test("reads short") {
        val r = ConfigReader[Short]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Short.valueOf(123.toShort))) ==> Right(123.toShort)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Short]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
      }
    }

    group("Byte") {
      test("reads byte") {
        val r = ConfigReader[Byte]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Byte.valueOf(7.toByte))) ==> Right(7.toByte)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Byte]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
      }
    }

    group("Float") {
      test("reads float") {
        val r = ConfigReader[Float]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(3.14)))
        assert(result.isRight)
        // Float precision: compare with tolerance
        val Right(value) = result: @unchecked
        assert(math.abs(value - 3.14f) < 0.001f)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Float]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
      }
    }

    group("Double") {
      test("reads double") {
        val r = ConfigReader[Double]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(3.14))) ==> Right(3.14)
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Double]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
      }
    }

    group("Char") {
      test("reads single character") {
        val r = ConfigReader[Char]
        r.from(ConfigValueFactory.fromAnyRef("a")) ==> Right('a')
      }
      test("rejects multi-character string") {
        val r = ConfigReader[Char]
        val result = r.from(ConfigValueFactory.fromAnyRef("abc"))
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.CannotConvert])
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[Char]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(65)))
        assert(result.isLeft)
      }
    }

    group("String") {
      test("reads string") {
        val r = ConfigReader[String]
        r.from(ConfigValueFactory.fromAnyRef("hello")) ==> Right("hello")
      }
      test("reads number as string") {
        val r = ConfigReader[String]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(42))) ==> Right("42")
      }
      test("reads boolean as string") {
        val r = ConfigReader[String]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Boolean.TRUE)) ==> Right("true")
      }
      test("wrong type (list) returns WrongType error") {
        val r = ConfigReader[String]
        val listVal = ConfigFactory.parseString("x = [1, 2]").getValue("x")
        val result = r.from(listVal)
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.WrongType])
      }
    }

    group("BigDecimal") {
      test("reads from number") {
        val r = ConfigReader[BigDecimal]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(3.14)))
        assert(result.isRight)
      }
      test("reads from string") {
        val r = ConfigReader[BigDecimal]
        r.from(ConfigValueFactory.fromAnyRef("123.456")) ==> Right(BigDecimal("123.456"))
      }
      test("invalid string returns CannotConvert error") {
        val r = ConfigReader[BigDecimal]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.CannotConvert])
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[BigDecimal]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Boolean.TRUE))
        assert(result.isLeft)
      }
    }

    group("BigInt") {
      test("reads from number") {
        val r = ConfigReader[BigInt]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(42)))
        assert(result.isRight)
        assert(result == Right(BigInt(42)))
      }
      test("reads from string") {
        val r = ConfigReader[BigInt]
        r.from(ConfigValueFactory.fromAnyRef("999")) ==> Right(BigInt(999))
      }
      test("invalid string returns CannotConvert error") {
        val r = ConfigReader[BigInt]
        val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
        assert(result.isLeft)
        result.left.exists(_.isInstanceOf[ConfigDecodingError.CannotConvert])
      }
      test("wrong type returns WrongType error") {
        val r = ConfigReader[BigInt]
        val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Boolean.TRUE))
        assert(result.isLeft)
      }
    }

    group("Option") {
      test("reads present value as Some") {
        val r = ConfigReader[Option[Int]]
        r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(42))) ==> Right(Some(42))
      }
      test("reads null value as None") {
        val r = ConfigReader[Option[Int]]
        r.from(ConfigValueFactory.fromAnyRef(null)) ==> Right(None)
      }
      test("reads null ConfigValue as None") {
        val r = ConfigReader[Option[Int]]
        r.from(null) ==> Right(None)
      }
    }
  }

  // ---------- ConfigReader combinators ----------

  group("ConfigReader combinators") {

    test("map transforms successful reads") {
      val r = ConfigReader[Int].map(_ * 2)
      r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(21))) ==> Right(42)
    }

    test("map preserves errors") {
      val r = ConfigReader[Int].map(_ * 2)
      val result = r.from(ConfigValueFactory.fromAnyRef("not-a-number"))
      assert(result.isLeft)
    }

    test("emap transforms successfully") {
      val r = ConfigReader[Int].emap { i =>
        if (i > 0) Right(i) else Left("must be positive")
      }
      r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(5))) ==> Right(5)
    }

    test("emap returns CannotConvert on failure") {
      val r = ConfigReader[Int].emap { i =>
        if (i > 0) Right(i) else Left("must be positive")
      }
      val result = r.from(ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(-1)))
      assert(result.isLeft)
      result.left.exists(_.isInstanceOf[ConfigDecodingError.CannotConvert])
    }
  }

  // ---------- ConfigWriter built-in instances ----------

  group("ConfigWriter built-in instances") {

    test("String") {
      val w = ConfigWriter[String]
      val v = w.to("hello")
      assert(v.unwrapped == "hello")
    }

    test("Boolean true") {
      val w = ConfigWriter[Boolean]
      val v = w.to(true)
      assert(v.unwrapped == java.lang.Boolean.TRUE)
    }

    test("Boolean false") {
      val w = ConfigWriter[Boolean]
      val v = w.to(false)
      assert(v.unwrapped == java.lang.Boolean.FALSE)
    }

    test("Int") {
      val w = ConfigWriter[Int]
      val v = w.to(42)
      assert(v.unwrapped == java.lang.Integer.valueOf(42))
    }

    test("Long") {
      val w = ConfigWriter[Long]
      val v = w.to(123456789L)
      assert(v.unwrapped == java.lang.Long.valueOf(123456789L))
    }

    test("Short") {
      val w = ConfigWriter[Short]
      val v = w.to(123.toShort)
      assert(v.unwrapped == java.lang.Short.valueOf(123.toShort))
    }

    test("Byte") {
      val w = ConfigWriter[Byte]
      val v = w.to(7.toByte)
      assert(v.unwrapped == java.lang.Byte.valueOf(7.toByte))
    }

    test("Float") {
      val w = ConfigWriter[Float]
      val v = w.to(3.14f)
      assert(v.unwrapped == java.lang.Float.valueOf(3.14f))
    }

    test("Double") {
      val w = ConfigWriter[Double]
      val v = w.to(3.14)
      assert(v.unwrapped == java.lang.Double.valueOf(3.14))
    }

    test("Char") {
      val w = ConfigWriter[Char]
      val v = w.to('a')
      assert(v.unwrapped == "a")
    }

    test("BigDecimal") {
      val w = ConfigWriter[BigDecimal]
      val v = w.to(BigDecimal("123.456"))
      assert(v.unwrapped == "123.456")
    }

    test("BigInt") {
      val w = ConfigWriter[BigInt]
      val v = w.to(BigInt(999))
      assert(v.unwrapped == "999")
    }

    test("Option Some") {
      val w = ConfigWriter[Option[Int]]
      val v = w.to(Some(42))
      assert(v.unwrapped == java.lang.Integer.valueOf(42))
    }

    test("Option None") {
      val w = ConfigWriter[Option[Int]]
      val v = w.to(None)
      assert(v.valueType == ConfigValueType.NULL)
    }
  }

  // ---------- ConfigWriter combinators ----------

  group("ConfigWriter combinators") {

    test("contramap transforms input") {
      val w = ConfigWriter[String].contramap[Int](_.toString)
      val v = w.to(42)
      assert(v.unwrapped == "42")
    }
  }

  // ---------- Reader/Writer round-trip for primitives ----------

  group("Reader/Writer round-trip") {

    test("String round-trips") {
      val r = ConfigReader[String]
      val w = ConfigWriter[String]
      r.from(w.to("hello")) ==> Right("hello")
    }

    test("Boolean round-trips") {
      val r = ConfigReader[Boolean]
      val w = ConfigWriter[Boolean]
      r.from(w.to(true)) ==> Right(true)
      r.from(w.to(false)) ==> Right(false)
    }

    test("Int round-trips") {
      val r = ConfigReader[Int]
      val w = ConfigWriter[Int]
      r.from(w.to(42)) ==> Right(42)
    }

    test("Double round-trips") {
      val r = ConfigReader[Double]
      val w = ConfigWriter[Double]
      r.from(w.to(3.14)) ==> Right(3.14)
    }

    test("Option round-trips") {
      val r = ConfigReader[Option[Int]]
      val w = ConfigWriter[Option[Int]]
      r.from(w.to(Some(42))) ==> Right(Some(42))
      r.from(w.to(None)) ==> Right(None)
    }
  }

  // ---------- SConfig convenience methods ----------

  group("SConfig convenience methods") {

    test("default config transforms member names with CamelCase to KebabCase") {
      val cfg = SConfig()
      cfg.transformMemberNames("myFieldName") ==> "my-field-name"
    }

    test("withSnakeCaseMemberNames transforms to snake_case") {
      val cfg = SConfig().withSnakeCaseMemberNames
      cfg.transformMemberNames("myFieldName") ==> "my_field_name"
    }

    test("withKebabCaseMemberNames transforms to kebab-case") {
      val cfg = SConfig().withKebabCaseMemberNames
      cfg.transformMemberNames("myFieldName") ==> "my-field-name"
    }

    test("withPascalCaseMemberNames transforms to PascalCase") {
      val cfg = SConfig().withPascalCaseMemberNames
      cfg.transformMemberNames("myFieldName") ==> "MyFieldName"
    }

    test("withScreamingSnakeCaseMemberNames transforms to SCREAMING_SNAKE_CASE") {
      val cfg = SConfig().withScreamingSnakeCaseMemberNames
      cfg.transformMemberNames("myFieldName") ==> "MY_FIELD_NAME"
    }

    test("withCamelCaseMemberNames transforms to camelCase (identity)") {
      val cfg = SConfig().withCamelCaseMemberNames
      cfg.transformMemberNames("myFieldName") ==> "myFieldName"
    }

    test("withDiscriminator sets discriminator field") {
      val cfg = SConfig().withDiscriminator("kind")
      cfg.discriminator ==> Some("kind")
    }

    test("withWrappedSubtypes sets discriminator to None") {
      val cfg = SConfig().withWrappedSubtypes
      cfg.discriminator ==> None
    }

    test("withStrictDecoding disables allowUnknownKeys") {
      val cfg = SConfig().withStrictDecoding
      cfg.allowUnknownKeys ==> false
    }

    test("withAllowUnknownKeys enables allowUnknownKeys") {
      val cfg = SConfig().withStrictDecoding.withAllowUnknownKeys
      cfg.allowUnknownKeys ==> true
    }

    test("withUseDefaults enables useDefaults") {
      val cfg = SConfig().withoutUseDefaults.withUseDefaults
      cfg.useDefaults ==> true
    }

    test("withoutUseDefaults disables useDefaults") {
      val cfg = SConfig().withoutUseDefaults
      cfg.useDefaults ==> false
    }

    test("withSnakeCaseConstructorNames transforms constructor names to snake_case") {
      val cfg = SConfig().withSnakeCaseConstructorNames
      cfg.transformConstructorNames("MyVariant") ==> "my_variant"
    }

    test("withKebabCaseConstructorNames transforms constructor names to kebab-case") {
      val cfg = SConfig().withKebabCaseConstructorNames
      cfg.transformConstructorNames("MyVariant") ==> "my-variant"
    }

    test("withTransformMemberNames accepts custom function") {
      val cfg = SConfig().withTransformMemberNames(_.toUpperCase)
      cfg.transformMemberNames("myField") ==> "MYFIELD"
    }

    test("withTransformConstructorNames accepts custom function") {
      val cfg = SConfig().withTransformConstructorNames(_.toLowerCase)
      cfg.transformConstructorNames("MyVariant") ==> "myvariant"
    }

    test("default SConfig is implicitly available") {
      val cfg = implicitly[SConfig]
      cfg.useDefaults ==> true
      cfg.allowUnknownKeys ==> true
      cfg.discriminator ==> Some("type")
    }
  }

  // ---------- ConfigDecodingError messages ----------

  group("ConfigDecodingError") {

    test("Missing error includes key name") {
      val err = ConfigDecodingError.Missing(Nil, "myKey")
      assert(err.getMessage.contains("myKey"))
      assert(err.getMessage.contains("Missing"))
    }

    test("Missing error with path") {
      val err = ConfigDecodingError.Missing(List("parent", "child"), "myKey")
      assert(err.getMessage.contains("parent.child"))
      assert(err.getMessage.contains("myKey"))
    }

    test("WrongType error includes expected and actual types") {
      val err = ConfigDecodingError.WrongType(Nil, "NUMBER", "STRING")
      assert(err.getMessage.contains("NUMBER"))
      assert(err.getMessage.contains("STRING"))
    }

    test("WrongType error with path") {
      val err = ConfigDecodingError.WrongType(List("foo"), "NUMBER", "STRING")
      assert(err.getMessage.contains("foo"))
    }

    test("CannotConvert error includes message") {
      val err = ConfigDecodingError.CannotConvert(Nil, "bad value")
      assert(err.getMessage.contains("bad value"))
    }

    test("CannotConvert error with path") {
      val err = ConfigDecodingError.CannotConvert(List("a", "b"), "oops")
      assert(err.getMessage.contains("a.b"))
    }

    test("UnknownKey error includes key name") {
      val err = ConfigDecodingError.UnknownKey(Nil, "extraField")
      assert(err.getMessage.contains("extraField"))
      assert(err.getMessage.contains("Unknown"))
    }

    test("UnknownKey error with path") {
      val err = ConfigDecodingError.UnknownKey(List("root"), "extraField")
      assert(err.getMessage.contains("root"))
    }

    test("Multiple error aggregates messages") {
      val e1 = ConfigDecodingError.Missing(Nil, "key1")
      val e2 = ConfigDecodingError.WrongType(Nil, "NUMBER", "STRING")
      val multi = ConfigDecodingError.Multiple(NonEmptyList(e1, List(e2)))
      assert(multi.getMessage.contains("key1"))
      assert(multi.getMessage.contains("NUMBER"))
    }

    test("withParentPath prepends path for Missing") {
      val err = ConfigDecodingError.Missing(List("child"), "key").withParentPath("parent")
      assert(err.path == List("parent", "child"))
    }

    test("withParentPath prepends path for WrongType") {
      val err = ConfigDecodingError.WrongType(Nil, "NUMBER", "STRING").withParentPath("field")
      assert(err.path == List("field"))
    }

    test("withParentPath prepends path for CannotConvert") {
      val err = ConfigDecodingError.CannotConvert(Nil, "msg").withParentPath("x")
      assert(err.path == List("x"))
    }

    test("withParentPath prepends path for UnknownKey") {
      val err = ConfigDecodingError.UnknownKey(Nil, "k").withParentPath("root")
      assert(err.path == List("root"))
    }

    test("withParentPath propagates into Multiple errors") {
      val e1 = ConfigDecodingError.Missing(Nil, "key1")
      val e2 = ConfigDecodingError.WrongType(Nil, "A", "B")
      val multi = ConfigDecodingError.Multiple(NonEmptyList(e1, List(e2))).withParentPath("root")
      multi match {
        case ConfigDecodingError.Multiple(errors) =>
          assert(errors.head.path == List("root"))
          assert(errors.tail.head.path == List("root"))
        case other => fail(s"Expected Multiple but got $other")
      }
    }

    test("merge two single errors into Multiple") {
      val e1 = ConfigDecodingError.Missing(Nil, "a")
      val e2 = ConfigDecodingError.Missing(Nil, "b")
      ConfigDecodingError.merge(e1, e2) match {
        case ConfigDecodingError.Multiple(errors) =>
          assert(errors.toList.size == 2)
        case other => fail(s"Expected Multiple but got $other")
      }
    }

    test("merge Multiple with single error appends") {
      val e1 = ConfigDecodingError.Missing(Nil, "a")
      val e2 = ConfigDecodingError.Missing(Nil, "b")
      val multi = ConfigDecodingError.Multiple(NonEmptyList(e1, List(e2)))
      val e3 = ConfigDecodingError.Missing(Nil, "c")
      ConfigDecodingError.merge(multi, e3) match {
        case ConfigDecodingError.Multiple(errors) =>
          assert(errors.toList.size == 3)
        case other => fail(s"Expected Multiple but got $other")
      }
    }

    test("merge single error with Multiple prepends") {
      val e1 = ConfigDecodingError.Missing(Nil, "a")
      val e2 = ConfigDecodingError.Missing(Nil, "b")
      val multi = ConfigDecodingError.Multiple(NonEmptyList(e1, List(e2)))
      val e3 = ConfigDecodingError.Missing(Nil, "c")
      ConfigDecodingError.merge(e3, multi) match {
        case ConfigDecodingError.Multiple(errors) =>
          assert(errors.toList.size == 3)
          assert(errors.head == e3)
        case other => fail(s"Expected Multiple but got $other")
      }
    }

    test("merge two Multiple errors concatenates") {
      val m1 = ConfigDecodingError.Multiple(
        NonEmptyList(ConfigDecodingError.Missing(Nil, "a"), List(ConfigDecodingError.Missing(Nil, "b")))
      )
      val m2 = ConfigDecodingError.Multiple(
        NonEmptyList(ConfigDecodingError.Missing(Nil, "c"), List(ConfigDecodingError.Missing(Nil, "d")))
      )
      ConfigDecodingError.merge(m1, m2) match {
        case ConfigDecodingError.Multiple(errors) =>
          assert(errors.toList.size == 4)
        case other => fail(s"Expected Multiple but got $other")
      }
    }
  }
}
