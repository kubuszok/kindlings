package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait BifunctorCaseClassRuleImpl {
  this: BifunctorMacrosImpl & MacroCommons & StdExtensions =>

  object BifunctorCaseClassRule extends BifunctorDerivationRule("Bifunctor as case class") {

    def apply[F[_, _]](implicit FCtor: Type.Ctor2[F]): MIO[Rule.Applicability[BifunctorCaseClassResult[F]]] = {
      implicit val IntType: Type[Int] = BifunctorTypes.Int
      implicit val StringType: Type[String] = BifunctorTypes.String

      val ccIntInt = CaseClass.parse(using FCtor.apply[Int, Int]).toEither match {
        case Right(cc) => cc
        case Left(e)   => return MIO.pure(Rule.yielded(s"Cannot parse F[Int, Int]: $e"))
      }
      val ccStringInt = CaseClass.parse(using FCtor.apply[String, Int]).toEither match {
        case Right(cc) => cc
        case Left(e)   => return MIO.pure(Rule.yielded(s"Cannot parse F[String, Int]: $e"))
      }
      val ccIntString = CaseClass.parse(using FCtor.apply[Int, String]).toEither match {
        case Right(cc) => cc
        case Left(e)   => return MIO.pure(Rule.yielded(s"Cannot parse F[Int, String]: $e"))
      }

      val fieldsProbe1 = ccIntInt.primaryConstructor.parameters.flatten.toList
      val fieldsProbe2 = ccStringInt.primaryConstructor.parameters.flatten.toList
      val fieldsProbe3 = ccIntString.primaryConstructor.parameters.flatten.toList

      val leftFields = scala.collection.mutable.Set.empty[String]
      val rightFields = scala.collection.mutable.Set.empty[String]
      val nestedFields = scala.collection.mutable.ListBuffer.empty[String]

      fieldsProbe1.zip(fieldsProbe2).zip(fieldsProbe3).foreach { case (((name, p1), (_, p2)), (_, p3)) =>
        val t1 = p1.tpe.Underlying
        val t2 = p2.tpe.Underlying
        val t3 = p3.tpe.Underlying
        val firstChanges = !(t1 =:= t2)
        val secondChanges = !(t1 =:= t3)

        if (firstChanges && !secondChanges) leftFields += name
        else if (!firstChanges && secondChanges) rightFields += name
        else if (firstChanges && secondChanges) nestedFields += name
      // else: invariant — neither changes, skip
      }

      if (nestedFields.nonEmpty) {
        MIO.pure(
          Rule.yielded(
            s"Fields ${nestedFields.mkString(", ")} depend on both type parameters. " +
              "Only fields that depend on at most one type parameter are supported."
          )
        )
      } else {
        MIO.pure(Rule.matched(BifunctorCaseClassResult(FCtor, leftFields.toSet, rightFields.toSet)))
      }
    }
  }
}
