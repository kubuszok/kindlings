package hearth.kindlings.catstaglessderivation

import hearth.MacroSuite
import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.tagless.aop.Instrumentation

final class TaglessKTraitSpec extends MacroSuite {

  private val fk = new FunctionK[Option, List] { def apply[A](fa: Option[A]): List[A] = fa.toList }
  private val gk = new FunctionK[List, Option] { def apply[A](fa: List[A]): Option[A] = fa.headOption }

  group("FunctorK for traits") {

    test("trait with direct F[X] methods") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = traitExamples.ServiceAlg.functorK.mapK(optService)(fk)
      listService.getUser(1) ==> List("user-1")
      listService.getAge("Alice") ==> List(42)
    }

    test("trait with mixed F[X] and invariant methods") {
      val optService = new traitExamples.MixedTraitAlg[Option] {
        def fetch(id: Int): Option[String] = Some(s"data-$id")
        def version: String = "v1"
      }
      val listService = traitExamples.MixedTraitAlg.functorK.mapK(optService)(fk)
      listService.fetch(1) ==> List("data-1")
      listService.version ==> "v1"
    }
  }

  group("InvariantK for traits") {

    test("trait with direct F[X] methods") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = traitExamples.ServiceAlg.invariantK.imapK(optService)(fk)(gk)
      listService.getUser(1) ==> List("user-1")
      listService.getAge("Alice") ==> List(42)
    }

    test("trait with mixed F[X] and invariant methods") {
      val optService = new traitExamples.MixedTraitAlg[Option] {
        def fetch(id: Int): Option[String] = Some(s"data-$id")
        def version: String = "v1"
      }
      val listService = traitExamples.MixedTraitAlg.invariantK.imapK(optService)(fk)(gk)
      listService.fetch(1) ==> List("data-1")
      listService.version ==> "v1"
    }
  }

  group("ContravariantK for traits") {

    test("trait with invariant-only methods") {
      val optConfig = new traitExamples.ConfigAlg[Option] {
        def name: String = "myApp"
        def version: Int = 42
      }
      val listConfig = traitExamples.ConfigAlg.contravariantK.contramapK(optConfig)(gk)
      listConfig.name ==> "myApp"
      listConfig.version ==> 42
    }
  }

  group("SemigroupalK for traits") {

    test("trait with direct F[X] methods") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = new traitExamples.ServiceAlg[List] {
        def getUser(id: Int): List[String] = List(s"user-$id-a", s"user-$id-b")
        def getAge(name: String): List[Int] = List(10, 20)
      }
      val result = traitExamples.ServiceAlg.semigroupalK.productK(optService, listService)
      result.getUser(1).first ==> Some("user-1")
      result.getUser(1).second ==> List("user-1-a", "user-1-b")
      result.getAge("Alice").first ==> Some(42)
      result.getAge("Alice").second ==> List(10, 20)
    }

    test("trait with mixed F[X] and invariant methods") {
      val optService = new traitExamples.MixedTraitAlg[Option] {
        def fetch(id: Int): Option[String] = Some(s"data-$id")
        def version: String = "v1"
      }
      val listService = new traitExamples.MixedTraitAlg[List] {
        def fetch(id: Int): List[String] = List(s"data-$id")
        def version: String = "v1"
      }
      val result = traitExamples.MixedTraitAlg.semigroupalK.productK(optService, listService)
      result.fetch(1).first ==> Some("data-1")
      result.fetch(1).second ==> List("data-1")
      result.version ==> "v1"
    }
  }

  group("ApplyK for traits") {

    test("mapK from ApplyK") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = traitExamples.ServiceAlg.applyK.mapK(optService)(fk)
      listService.getUser(1) ==> List("user-1")
      listService.getAge("Alice") ==> List(42)
    }

    test("productK from ApplyK") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = new traitExamples.ServiceAlg[List] {
        def getUser(id: Int): List[String] = List(s"user-$id-a")
        def getAge(name: String): List[Int] = List(10)
      }
      val result = traitExamples.ServiceAlg.applyK.productK(optService, listService)
      result.getUser(1).first ==> Some("user-1")
      result.getUser(1).second ==> List("user-1-a")
      result.getAge("Alice").first ==> Some(42)
      result.getAge("Alice").second ==> List(10)
    }

    test("mixed direct and invariant") {
      val optService = new traitExamples.MixedTraitAlg[Option] {
        def fetch(id: Int): Option[String] = Some(s"data-$id")
        def version: String = "v1"
      }
      val listService = traitExamples.MixedTraitAlg.applyK.mapK(optService)(fk)
      listService.fetch(1) ==> List("data-1")
      listService.version ==> "v1"
    }
  }

  group("Instrument for traits") {

    test("trait methods wrapped in Instrumentation") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val instrumented = traitExamples.ServiceAlg.instrument.instrument(optService)
      instrumented.getUser(1).value ==> Some("user-1")
      instrumented.getUser(1).algebraName ==> "ServiceAlg"
      instrumented.getUser(1).methodName ==> "getUser"
      instrumented.getAge("Alice").value ==> Some(42)
      instrumented.getAge("Alice").methodName ==> "getAge"
    }

    test("mixed methods — invariant preserved") {
      val optService = new traitExamples.MixedTraitAlg[Option] {
        def fetch(id: Int): Option[String] = Some(s"data-$id")
        def version: String = "v1"
      }
      val instrumented = traitExamples.MixedTraitAlg.instrument.instrument(optService)
      instrumented.fetch(1).value ==> Some("data-1")
      instrumented.fetch(1).methodName ==> "fetch"
      instrumented.version ==> "v1"
    }

    test("mapK still works on Instrument") {
      val optService = new traitExamples.ServiceAlg[Option] {
        def getUser(id: Int): Option[String] = Some(s"user-$id")
        def getAge(name: String): Option[Int] = Some(42)
      }
      val listService = traitExamples.ServiceAlg.instrument.mapK(optService)(fk)
      listService.getUser(1) ==> List("user-1")
      listService.getAge("Alice") ==> List(42)
    }
  }

  group("Variance-aware trait derivation") {

    test("InvariantK handles F in both parameter and return position") {
      val optService = new traitExamples.TransformAlg[Option] {
        def transform(input: Option[Int]): Option[String] = input.map(_.toString)
      }
      val listService = traitExamples.TransformAlg.invariantK.imapK(optService)(fk)(gk)
      listService.transform(List(42)) ==> List("42")
      listService.transform(List.empty) ==> List.empty
    }

    test("ContravariantK handles F only in parameter position") {
      var consumed: Any = null
      val optConsumer = new traitExamples.ConsumerAlg[Option] {
        def consume(data: Option[Boolean]): Unit = consumed = data
      }
      val listConsumer = traitExamples.ConsumerAlg.contravariantK.contramapK(optConsumer)(gk)
      listConsumer.consume(List(true, false))
      consumed ==> Some(true)
    }

    test("InvariantK handles F only in parameter position") {
      var consumed: Any = null
      val optConsumer = new traitExamples.ConsumerAlg[Option] {
        def consume(data: Option[Boolean]): Unit = consumed = data
      }
      val listConsumer = traitExamples.ConsumerAlg.invariantK.imapK(optConsumer)(fk)(gk)
      listConsumer.consume(List(true, false))
      consumed ==> Some(true)
    }
  }
}
