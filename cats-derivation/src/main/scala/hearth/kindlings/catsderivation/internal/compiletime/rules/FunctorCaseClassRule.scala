package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorCaseClassRuleImpl {
  this: FunctorMacrosImpl & MacroCommons & StdExtensions =>

  object FunctorCaseClassRule extends FunctorDerivationRule("Functor as case class") {

    def apply[F[_]](implicit FCtor: Type.Ctor1[F]): MIO[Rule.Applicability[FunctorCaseClassResult[F]]] = {
      implicit val IntType: Type[Int] = FunctorTypes.Int
      implicit val StringType: Type[String] = FunctorTypes.String

      val ccInt = CaseClass.parse(using FCtor.apply[Int]).toEither match {
        case Right(cc) => cc
        case Left(e)   => return MIO.pure(Rule.yielded(s"Cannot parse F[Int]: $e"))
      }
      val ccString = CaseClass.parse(using FCtor.apply[String]).toEither match {
        case Right(cc) => cc
        case Left(e)   => return MIO.pure(Rule.yielded(s"Cannot parse F[String]: $e"))
      }

      val fieldsInt = ccInt.primaryConstructor.parameters.flatten.toList
      val fieldsString = ccString.primaryConstructor.parameters.flatten.toList

      val directFields = scala.collection.mutable.Set.empty[String]
      val nestedFields = scala.collection.mutable.ListBuffer.empty[String]

      fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
        val tInt = pInt.tpe.Underlying
        val tString = pString.tpe.Underlying
        if (tInt =:= IntType && tString =:= StringType) {
          directFields += name
        } else if (tInt =:= tString) {
          // Invariant
        } else {
          nestedFields += name
        }
      }

      if (nestedFields.nonEmpty) {
        MIO.pure(
          Rule.yielded(
            s"Fields ${nestedFields.mkString(", ")} contain nested type constructors. " +
              "Only direct type parameter fields (A) and invariant fields are supported."
          )
        )
      } else {
        val directFieldSet: Set[String] = directFields.toSet
        MIO.pure(Rule.matched(FunctorCaseClassResult(FCtor, directFieldSet)))
      }
    }
  }
}
