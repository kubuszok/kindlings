package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

trait MethodFolds { this: MacroCommons =>

  /** Folds a method that should never require an instance (constructors, default-value accessors, companion `apply`s).
    *
    * `Method.fold` forces an `onInstance` branch that can only return an `Expr_??` — for instance-free methods the
    * branch is unreachable, and modules used to put `throw new RuntimeException(...)` there, which would crash the
    * compiler instead of reporting a derivation error. This helper goes through `foldF` with an `Either` so the
    * impossible case becomes a `Left` that callers route into the MIO error channel like any other failure.
    */
  final def foldInstanceFree(method: Method, what: => String)(
      onTypes: Method.ApplyTypes => UntypedTypeArguments,
      onValues: Method.ApplyValues => Arguments
  ): Either[String, Expr_??] = {
    type EitherString[A] = Either[String, A]
    method
      .foldF[EitherString](
        onInstance = _ => Left(s"$what should not require an instance: ${method.prettyPrint}"),
        onTypes = at => Right(onTypes(at)),
        onValues = av => Right(onValues(av))
      )
      .flatten
  }
}
