package hearth.kindlings.derivation.compiletime

import scala.quoted.*

// Marker types of the probe in `UserInstancesPlatform.summonIgnoringWithGivens` (Scala 3): the type argument `B` of the
// searched `TC[Page[B]]` is replaced with `B & KindlingsDependencyN[TC]`, whose companion provides a placeholder
// `TC[B & KindlingsDependencyN[TC]]` to the user's instance (`Page.enc[A](implicit a: TC[A])`). They only appear in
// that search, never in generated code.

sealed trait KindlingsDependency0[F[_]]
object KindlingsDependency0 {
  inline given provided[T]: T = KindlingsDependency.provide[T](0)
}
sealed trait KindlingsDependency1[F[_]]
object KindlingsDependency1 {
  inline given provided[T]: T = KindlingsDependency.provide[T](1)
}
sealed trait KindlingsDependency2[F[_]]
object KindlingsDependency2 {
  inline given provided[T]: T = KindlingsDependency.provide[T](2)
}
sealed trait KindlingsDependency3[F[_]]
object KindlingsDependency3 {
  inline given provided[T]: T = KindlingsDependency.provide[T](3)
}
sealed trait KindlingsDependency4[F[_]]
object KindlingsDependency4 {
  inline given provided[T]: T = KindlingsDependency.provide[T](4)
}
sealed trait KindlingsDependency5[F[_]]
object KindlingsDependency5 {
  inline given provided[T]: T = KindlingsDependency.provide[T](5)
}
sealed trait KindlingsDependency6[F[_]]
object KindlingsDependency6 {
  inline given provided[T]: T = KindlingsDependency.provide[T](6)
}
sealed trait KindlingsDependency7[F[_]]
object KindlingsDependency7 {
  inline given provided[T]: T = KindlingsDependency.provide[T](7)
}

object KindlingsDependency {

  // (not in the markers' companions: there the unconstrained given would be found for the macro's own `Type[T]`)
  inline def provide[T](inline index: Int): T = ${ placeholder[T]('index) }

  val markerClassNames: Vector[String] =
    Vector.tabulate(8)(index => s"hearth.kindlings.derivation.compiletime.KindlingsDependency$index")

  /** `null` as `T` when `T` is `F[B & KindlingsDependencyN[F]]` (the instance of the dependency number `index` of the
    * probe), a failed (not applicable) candidate for any other type: the companion of a marker is in the implicit scope
    * of every type mentioning it, also of the searched instance itself.
    */
  def placeholder[T](index: Expr[Int])(using t: Type[T], q: Quotes): Expr[T] = {
    import q.reflect.*
    val marker = Symbol.requiredClass(markerClassNames(index.valueOrAbort))
    def isMarkerFor(tpe: TypeRepr, typeClass: TypeRepr): Boolean = tpe.dealias match {
      case AppliedType(markerCtor, List(f)) => markerCtor.typeSymbol == marker && f.typeSymbol == typeClass.typeSymbol
      case _                                => false
    }
    TypeRepr.of[T].dealias match {
      case AppliedType(typeClass, List(AndType(left, right)))
          if isMarkerFor(right, typeClass) || isMarkerFor(left, typeClass) =>
        '{ null.asInstanceOf[T] }
      case _ => report.errorAndAbort(s"${TypeRepr.of[T].show} is not a Kindlings dependency placeholder")
    }
  }
}
