package hearth.kindlings.catstaglessderivation.internal.runtime

import cats.arrow.FunctionK
import cats.data.Tuple2K
import cats.tagless.{ContravariantK, FunctorK, InvariantK, SemigroupalK}
import cats.tagless.aop.Instrumentation

object CatsTaglessRuntime {

  private type ConstAny[F[_]] = Any
  private type IdK[A] = A

  def applyFk(fk: Any, value: Any): Any =
    fk.asInstanceOf[FunctionK[IdK, IdK]].apply(value.asInstanceOf[Any])

  def imapK(invariantK: Any, af: Any, fk: Any, gk: Any): Any =
    invariantK
      .asInstanceOf[InvariantK[ConstAny]]
      .imapK[Option, List](af.asInstanceOf[Any])(
        fk.asInstanceOf[FunctionK[Option, List]]
      )(gk.asInstanceOf[FunctionK[List, Option]])

  def mapK(functorK: Any, af: Any, fk: Any): Any =
    functorK
      .asInstanceOf[FunctorK[ConstAny]]
      .mapK[Option, List](af.asInstanceOf[Any])(
        fk.asInstanceOf[FunctionK[Option, List]]
      )

  def contramapK(contravariantK: Any, af: Any, fk: Any): Any =
    contravariantK
      .asInstanceOf[ContravariantK[ConstAny]]
      .contramapK[Option, List](af.asInstanceOf[Any])(
        fk.asInstanceOf[FunctionK[List, Option]]
      )

  def mkTuple2K(first: Any, second: Any): Any =
    Tuple2K[IdK, IdK, Any](first, second)

  def productK(semigroupalK: Any, af: Any, ag: Any): Any =
    semigroupalK
      .asInstanceOf[SemigroupalK[ConstAny]]
      .productK[Option, List](af.asInstanceOf[Any], ag.asInstanceOf[Any])

  def mkInstrumentation(value: Any, algebraName: String, methodName: String): Any =
    Instrumentation[IdK, Any](value, algebraName, methodName)

  // --- Trait algebras: varargs, Tuple2K projections, instances for types nesting `F` (since 0.2.x) ---
  //
  // A vararg parameter mentioning `F` is transformed element by element (like cats-tagless). The `F ~> G` /
  // `*K` instances are erased: the macro checks the types, these only forward.

  def applyFkAll(fk: Any, values: Seq[Any]): Seq[Any] = {
    val f = fk.asInstanceOf[FunctionK[IdK, IdK]]
    values.map(v => f.apply(v))
  }

  def imapKAll(invariantK: Any, values: Seq[Any], fk: Any, gk: Any): Seq[Any] =
    values.map(v => imapK(invariantK, v, fk, gk))

  def mapKAll(functorK: Any, values: Seq[Any], fk: Any): Seq[Any] =
    values.map(v => mapK(functorK, v, fk))

  def contramapKAll(contravariantK: Any, values: Seq[Any], fk: Any): Seq[Any] =
    values.map(v => contramapK(contravariantK, v, fk))

  def tuple2KFirst(value: Any): Any = value.asInstanceOf[Tuple2K[IdK, IdK, Any]].first

  def tuple2KSecond(value: Any): Any = value.asInstanceOf[Tuple2K[IdK, IdK, Any]].second

  def tuple2KFirstAll(values: Seq[Any]): Seq[Any] = values.map(tuple2KFirst)

  def tuple2KSecondAll(values: Seq[Any]): Seq[Any] = values.map(tuple2KSecond)

  /** `SemigroupalK.firstK`: `Tuple2K[F, G, *] ~> F`. */
  val firstK: Any = cats.tagless.SemigroupalK.firstK[IdK, IdK]

  /** `SemigroupalK.secondK`: `Tuple2K[F, G, *] ~> G`. */
  val secondK: Any = cats.tagless.SemigroupalK.secondK[IdK, IdK]
}
