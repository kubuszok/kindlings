package hearth.kindlings.di

import hearth.MacroSuite

final class WiredSpec extends MacroSuite {

  group("DI.wiredInModule") {

    test("lookup returns all registered instances assignable to a class (subtype-aware)") {
      val module = new WiredSpec.Module
      val wired = DI.wiredInModule(module)
      wired.lookup(classOf[WiredSpec.Base]).toSet ==> Set[WiredSpec.Base](module.y, module.z)
      wired.lookup(classOf[WiredSpec.X]) ==> List(module.x)
      wired.lookup(classOf[String]) ==> Nil
    }

    test("lookupSingleOrThrow returns the unique instance, or throws on zero / several") {
      val module = new WiredSpec.Module
      val wired = DI.wiredInModule(module)
      wired.lookupSingleOrThrow(classOf[WiredSpec.X]) ==> module.x
      val _ = intercept[IllegalArgumentException](wired.lookupSingleOrThrow(classOf[WiredSpec.Base])) // two matches
      val _ = intercept[IllegalArgumentException](wired.lookupSingleOrThrow(classOf[String])) // zero matches
    }

    test("primitive-typed members are excluded from the registry") {
      val module = new WiredSpec.Module
      val wired = DI.wiredInModule(module)
      // the `count: Int` member is not registered (macwire's `result <:< AnyRef` filter)
      wired.lookup(classOf[Integer]) ==> Nil
    }

    test("withInstances and withInstanceFactory extend the registry; a factory is re-invoked per lookup") {
      val wired = DI.wiredInModule(new WiredSpec.Empty)
      val x = new WiredSpec.X
      wired.withInstances(x).lookup(classOf[WiredSpec.X]) ==> List(x)

      var n = 0
      val w2 = wired.withInstanceFactory(() => { n += 1; new WiredSpec.X })
      val a = w2.lookup(classOf[WiredSpec.X]).head
      val b = w2.lookup(classOf[WiredSpec.X]).head
      (a ne b) ==> true
      n ==> 2
    }
  }
}

object WiredSpec {
  trait Base
  class X
  class Y extends Base
  class Z extends Base

  class Module {
    val x: X = new X
    val y: Y = new Y
    val z: Z = new Z
    val count: Int = 5 // primitive -> excluded from the registry
  }

  class Empty
}
