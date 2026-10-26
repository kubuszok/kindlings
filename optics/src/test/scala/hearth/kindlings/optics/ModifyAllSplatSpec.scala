package hearth.kindlings.optics

import hearth.MacroSuite

/** `modifyAll`/`modifyAllLens` need their extra paths written out at the call site: a splatted, non-literal sequence
  * (`paths: _*`) only has its elements at runtime, so it is rejected on both Scala 2 and Scala 3 (via Hearth's
  * `VarArgs.toIterableOption`) instead of being silently dropped or misparsed as a path.
  */
final class ModifyAllSplatSpec extends MacroSuite {

  group("modifyAll with splatted (non-literal) varargs") {

    test("modifyAll rejects a splatted path sequence") {
      compileErrors(
        """
        import hearth.kindlings.optics.*
        import hearth.kindlings.optics.ModifyAllSplatSpec.*
        Names("a", "b").modifyAll[String](_.first, Seq((n: Names) => n.second): _*).setTo("x")
        """
      ).check(
        "`modifyAll` expects an explicit list of paths"
      )
    }

    test("modifyAllLens rejects a splatted path sequence") {
      compileErrors(
        """
        import hearth.kindlings.optics.*
        import hearth.kindlings.optics.ModifyAllSplatSpec.*
        modifyAllLens[Names].apply[String](_.first, Seq((n: Names) => n.second): _*)
        """
      ).check(
        "`modifyAllLens` expects an explicit list of paths"
      )
    }
  }
}

object ModifyAllSplatSpec {

  final case class Names(first: String, second: String)
}
