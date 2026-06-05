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
}
