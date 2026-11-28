package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

import scala.collection.immutable.ListMap

/** The values of the fields of a case class, `private val` constructor parameters included.
  *
  * Hearth's `caseFieldValuesAt` returns only the fields the generated code can read with an accessor at the expansion
  * site, so a `case class C(a: Int, private val b: String, c: Boolean)` has `a` and `c` only. Derivations which read
  * every field (encoders, `Show`, `Eq`/`Order`/`Hash`, `Semigroup`, `Functor`, `Cogen`, diffs, ...) would then silently
  * leave `b` out (and a decoder of the same type would require it), and those which match the fields with the
  * constructor parameters by position would shift every field after `b` (avro wrote `c` where the schema has `b`).
  *
  * [[allCaseFieldValuesAt]] returns all parameters of the primary constructor's first parameter list, in declaration
  * order: the accessible ones as Hearth reads them, the others with `Product.productElement(index)` (every case class
  * is a `Product`, its elements are those parameters), followed by any other accessible case field. Match the values
  * with constructor parameters by name.
  *
  * Mixed in through [[DerivationPolicy]], which every derivation macro bundle has.
  */
trait CaseFieldValuesSupport { this: MacroCommons =>

  /** `caseClass.caseFieldValuesAt(instance)` with the `private val` constructor parameters (see
    * [[CaseFieldValuesSupport]]).
    */
  protected def allCaseFieldValuesAt[A](caseClass: CaseClass[A], instance: Expr[A]): ListMap[String, Expr_??] = {
    implicit val A: Type[A] = caseClass.tpe
    val accessible = caseClass.caseFieldValuesAt(instance)
    val constructorFields = caseClass.primaryConstructor.totalParameters.headOption.toList.flatMap(_.toList)
    if (constructorFields.forall { case (name, _) => accessible.contains(name) }) accessible
    else {
      val inConstructor = constructorFields.zipWithIndex.map { case ((name, param), index) =>
        accessible.get(name) match {
          case Some(value) => name -> value
          case None        =>
            import param.tpe.Underlying as Field
            name -> productElementOf[A, Field](instance, index).as_??
        }
      }
      val constructorNames = inConstructor.map(_._1).toSet
      ListMap.from(inConstructor ++ accessible.filterNot { case (name, _) => constructorNames(name) })
    }
  }

  private def productElementOf[A: Type, Field: Type](instance: Expr[A], index: Int): Expr[Field] =
    Expr.quote {
      Expr.splice(instance).asInstanceOf[Product].productElement(Expr.splice(Expr(index))).asInstanceOf[Field]
    }
}
