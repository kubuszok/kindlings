package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.catsderivation.StrictDerivation

trait StrictDerivationSupport { this: MacroCommons & StdExtensions =>

  private def strictDerivationType: Type[StrictDerivation] = Type.of[StrictDerivation]

  protected lazy val isStrictDerivationMode: Boolean = {
    implicit val st: Type[StrictDerivation] = strictDerivationType
    Expr.summonImplicit[StrictDerivation].isDefined
  }
}

final private[compiletime] class StrictDerivationError(val message: String) extends util.control.NoStackTrace {
  override def getMessage(): String = message
}
