package hearth.kindlings.derivation.compiletime

import scala.quoted.*

// Marker types of the probe in `UserInstancesPlatform.summonIgnoringWithGivens` (Scala 3): the type argument `B` of the
// searched `TC[Page[B]]` is replaced with `B & KindlingsDependencyN[TC]`, whose companion provides a placeholder
// `TC[B & KindlingsDependencyN[TC]]` to the user's instance (`Page.enc[A](implicit a: TC[A])`). They only appear in
// that search, never in generated code.

sealed trait KindlingsDependency0[F[_]]
object KindlingsDependency0 {
  inline given provided[F[_], X]: F[X & KindlingsDependency0[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency0[F]]](0)
}
sealed trait KindlingsDependency1[F[_]]
object KindlingsDependency1 {
  inline given provided[F[_], X]: F[X & KindlingsDependency1[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency1[F]]](1)
}
sealed trait KindlingsDependency2[F[_]]
object KindlingsDependency2 {
  inline given provided[F[_], X]: F[X & KindlingsDependency2[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency2[F]]](2)
}
sealed trait KindlingsDependency3[F[_]]
object KindlingsDependency3 {
  inline given provided[F[_], X]: F[X & KindlingsDependency3[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency3[F]]](3)
}
sealed trait KindlingsDependency4[F[_]]
object KindlingsDependency4 {
  inline given provided[F[_], X]: F[X & KindlingsDependency4[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency4[F]]](4)
}
sealed trait KindlingsDependency5[F[_]]
object KindlingsDependency5 {
  inline given provided[F[_], X]: F[X & KindlingsDependency5[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency5[F]]](5)
}
sealed trait KindlingsDependency6[F[_]]
object KindlingsDependency6 {
  inline given provided[F[_], X]: F[X & KindlingsDependency6[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency6[F]]](6)
}
sealed trait KindlingsDependency7[F[_]]
object KindlingsDependency7 {
  inline given provided[F[_], X]: F[X & KindlingsDependency7[F]] =
    KindlingsDependency.provide[F[X & KindlingsDependency7[F]]](7)
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
    def components(tpe: TypeRepr): List[TypeRepr] = tpe.dealias match {
      case AndType(left, right) => components(left) ++ components(right)
      case other                => List(other)
    }
    // `B & KindlingsDependencyN[TC]` with the type class itself as the marker's argument, also in the instance a
    // candidate asks for (`Encoder[B & Marker[Encoder]]`); a type lambda inferred for `F` (`Exported[T]` in circe's
    // `importedEncoder`, `C[A] => Iterable[A]` in its `encodeIterable`, through covariance) is not the type class
    def isTypeLambda(tpe: TypeRepr): Boolean = tpe.dealias match {
      case _: TypeLambda => true
      case _             => false
    }
    def isMarkerFor(tpe: TypeRepr, typeClass: TypeRepr): Boolean = tpe match {
      case AppliedType(markerCtor, List(f)) =>
        markerCtor.typeSymbol == marker && typeClass.typeSymbol.isClassDef && !isTypeLambda(f) &&
        f.typeSymbol == typeClass.typeSymbol
      case _ => false
    }
    // `F[X & Marker[F]]` with `F` inferred as a type lambda (`[A] =>> Encoder[A]`) is not reduced by itself
    def reduced(tpe: TypeRepr): TypeRepr = tpe.dealias match {
      case AppliedType(ctor, arguments) if isTypeLambda(ctor) => reduced(ctor.appliedTo(arguments))
      case other                                              => other
    }
    reduced(TypeRepr.of[T]) match {
      case AppliedType(typeClass, List(argument)) if components(argument).exists(isMarkerFor(_, typeClass)) =>
        '{ null.asInstanceOf[T] }
      case _ => report.errorAndAbort(s"${TypeRepr.of[T].show} is not a Kindlings dependency placeholder")
    }
  }
}
