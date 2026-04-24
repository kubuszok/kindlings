package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Shrink}

object extensions {

  extension (companion: Arbitrary.type) {
    inline def derived[A]: Arbitrary[A] = ${ internal.compiletime.ArbitraryMacros.deriveArbitrary[A] }
  }

  extension (companion: Shrink.type) {
    inline def derived[A]: Shrink[A] = ${ internal.compiletime.ShrinkMacros.deriveShrink[A] }
  }

  extension (companion: Cogen.type) {
    inline def derived[A]: Cogen[A] = ${ internal.compiletime.CogenMacros.deriveCogen[A] }
  }
}
