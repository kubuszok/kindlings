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

      val fieldsInt = ccInt.primaryConstructor.totalParameters.flatten.toList
      val fieldsString = ccString.primaryConstructor.totalParameters.flatten.toList

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

      val parentCtor = FCtor.asUntyped

      if (nestedFields.nonEmpty) {
        val nestedFieldFunctors = scala.collection.mutable.Map.empty[String, Expr[Any]]
        val selfRecursive = scala.collection.mutable.Set.empty[String]
        val selfRecursiveOuter = scala.collection.mutable.Map.empty[String, Expr[Any]]
        val unsupported = scala.collection.mutable.ListBuffer.empty[String]
        nestedFields.foreach { name =>
          val param = fieldsInt.find(_._1 == name).get._2
          import param.tpe.Underlying as FieldType
          val fieldType = Type[FieldType].asInstanceOf[Type[Any]]

          // Check for self-recursive nested pattern: e.g. Option[Search[Int]] where Search is the parent.
          // Uses symbol-based comparison for reliable cross-platform equality.
          val isSelfRecursiveNested = isNestedSelfRecursive(fieldType, parentCtor)

          if (isSelfRecursiveNested) {
            // Self-recursive nested: summon the OUTER functor (e.g. Functor[Option])
            summonFunctorForFieldType(fieldType) match {
              case Some(outerFunctorExpr) =>
                selfRecursive += name
                selfRecursiveOuter += (name -> outerFunctorExpr)
              case None =>
                unsupported += name
            }
          } else {
            summonFunctorForFieldType(fieldType) match {
              case Some(functorExpr) => nestedFieldFunctors += (name -> functorExpr)
              case None              =>
                // Also check for direct self-recursion (e.g. IList[Int] where F = IList)
                mkCtor1FromType(fieldType) match {
                  case Some((_, nestedUntyped)) if nestedUntyped == parentCtor =>
                    selfRecursive += name
                  case _ =>
                    unsupported += name
                }
            }
          }
        }
        if (unsupported.nonEmpty) {
          MIO.pure(
            Rule.yielded(
              s"Fields ${unsupported.mkString(", ")} contain nested type constructors " +
                "without Functor instances."
            )
          )
        } else {
          val directFieldSet: Set[String] = directFields.toSet
          MIO.pure(
            Rule.matched(
              FunctorCaseClassResult(
                FCtor,
                directFieldSet,
                nestedFieldFunctors.toMap,
                selfRecursive.toSet,
                selfRecursiveOuter.toMap
              )
            )
          )
        }
      } else {
        val directFieldSet: Set[String] = directFields.toSet
        MIO.pure(Rule.matched(FunctorCaseClassResult(FCtor, directFieldSet, Map.empty)))
      }
    }
  }
}
