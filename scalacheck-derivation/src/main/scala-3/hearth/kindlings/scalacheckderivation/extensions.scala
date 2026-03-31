package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary

object extensions {

  extension (companion: Arbitrary.type) {
    inline def derived[A]: Arbitrary[A] = ${ internal.compiletime.ArbitraryMacros.deriveArbitrary[A] }
  }
}
