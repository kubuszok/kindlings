package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary
import scala.language.experimental.macros

trait ScalaCheckDerivationScala2Extensions {

  implicit class ArbitraryDerived(private val companion: Arbitrary.type) {
    def derived[A]: Arbitrary[A] = macro internal.compiletime.ArbitraryMacros.deriveArbitraryImpl[A]
  }
}

object extensions extends ScalaCheckDerivationScala2Extensions
