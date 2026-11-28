package hearth.kindlings.di

import hearth.MacroSuite

/** macwire's ranking of the members of an enclosing class/object: the members declared in the class itself come before
  * the inherited ones (and the members of a `@Module` held by a declared member are at the same level as the inherited
  * members), and an ambiguity is reported only among candidates of the same priority.
  */
final class DeclaredMemberPrioritySpec extends MacroSuite {

  import DeclaredMemberPrioritySpec.*

  group("DI.wire: declared members are preferred over inherited ones") {

    test("in a class") {
      val module = new ClassModule
      module.holder.dep ==> module.declaredDep
    }

    test("in an object") {
      ObjectModule.holder.dep ==> ObjectModule.declaredDep
    }

    test("a declared override of an inherited abstract member is a declared member") {
      val module = new OverridingModule
      module.holder.dep ==> module.dep
    }

    test("over a member provided by a self-type") {
      val module = new SelfTyped with DepProviderImpl {}
      module.holder.dep ==> module.declaredDep
    }

    test("the parameters of the enclosing method still come first") {
      val module = new ClassModule
      val dep = new Dep
      module.holderFrom(dep).dep ==> dep
    }

    test("an inherited member is used when nothing of its type is declared") {
      val module = new MixedModule
      module.pair.dep ==> module.declaredDep
      module.pair.other ==> module.inheritedOther
    }
  }

  group("DI.wireRec: declared members are preferred over inherited ones") {

    test("for the dependencies of the recursively built values") {
      val module = new RecModule
      module.outer.holder.dep ==> module.declaredDep
    }
  }

  group("DI.wireSet / DI.wireList: declared and inherited members are all collected") {

    test("wireSet") {
      val module = new CollectionModule
      module.set ==> Set(module.declaredDep, module.inheritedDep)
    }

    test("wireList: declared members first, then the inherited ones") {
      val module = new CollectionModule
      module.list ==> List(module.declaredDep, module.inheritedDep)
    }
  }

  group("DI.wire: ambiguity within the same priority") {

    test("two inherited members of the same type") {
      val errors: String = compileErrors("""
        import hearth.kindlings.di.DeclaredMemberPrioritySpec.*
        trait Left { val leftDep: Dep = new Dep }
        trait Right { val rightDep: Dep = new Dep }
        class Module extends Left with Right {
          lazy val holder: Holder = DI.wire[Holder]
        }
      """)
      assert(errors.contains("Found multiple values of type"), errors)
      assert(errors.contains("leftDep") && errors.contains("rightDep"), errors)
    }

    test("two declared members of the same type (an inherited one does not matter)") {
      val errors: String = compileErrors("""
        import hearth.kindlings.di.DeclaredMemberPrioritySpec.*
        class Module extends Base {
          val firstDep: Dep = new Dep
          val secondDep: Dep = new Dep
          lazy val holder: Holder = DI.wire[Holder]
        }
      """)
      assert(errors.contains("Found multiple values of type"), errors)
      assert(errors.contains("firstDep") && errors.contains("secondDep"), errors)
      assert(!errors.contains("inheritedDep"), errors)
    }

    test("an inherited member and a member of a @Module held by a declared member") {
      val errors: String = compileErrors("""
        import hearth.kindlings.di.DeclaredMemberPrioritySpec.*
        class Module(val depModule: DepModule) extends Base {
          lazy val holder: Holder = DI.wire[Holder]
        }
      """)
      assert(errors.contains("Found multiple values of type"), errors)
      assert(errors.contains("inheritedDep") && errors.contains("moduleDep"), errors)
    }
  }
}

object DeclaredMemberPrioritySpec {
  class Dep
  class Other
  class Holder(val dep: Dep)
  class Pair(val dep: Dep, val other: Other)
  class Outer(val holder: Holder)

  trait Base {
    val inheritedDep: Dep = new Dep
  }

  class ClassModule extends Base {
    val declaredDep: Dep = new Dep
    lazy val holder: Holder = DI.wire[Holder]
    def holderFrom(dep: Dep): Holder = DI.wire[Holder]
  }

  object ObjectModule extends Base {
    val declaredDep: Dep = new Dep
    lazy val holder: Holder = DI.wire[Holder]
  }

  trait AbstractBase {
    def dep: Dep
  }
  class OverridingModule extends AbstractBase with Base {
    lazy val dep: Dep = new Dep
    lazy val holder: Holder = DI.wire[Holder]
  }

  trait DepProvider {
    def providedDep: Dep
  }
  trait DepProviderImpl extends DepProvider {
    lazy val providedDep: Dep = new Dep
  }
  trait SelfTyped { this: DepProvider =>
    lazy val declaredDep: Dep = new Dep
    lazy val holder: Holder = DI.wire[Holder]
  }

  trait OtherBase extends Base {
    val inheritedOther: Other = new Other
  }
  class MixedModule extends OtherBase {
    val declaredDep: Dep = new Dep
    lazy val pair: Pair = DI.wire[Pair]
  }

  class RecModule extends Base {
    val declaredDep: Dep = new Dep
    lazy val outer: Outer = DI.wireRec[Outer]
  }

  class CollectionModule extends Base {
    val declaredDep: Dep = new Dep
    lazy val set: Set[Dep] = DI.wireSet[Dep]
    lazy val list: List[Dep] = DI.wireList[Dep]
  }

  @Module
  class DepModule {
    val moduleDep: Dep = new Dep
  }
}
