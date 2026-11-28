package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Builds a value from N per-field decode results as FLAT code: one local `def` per field and one statement per field
  * in the calling method, no nesting.
  *
  * Fail-fast ([[constructFailFast]]) - the first failure in field order is the result and later fields are not decoded:
  * {{{
  *   def field0 = <decode field 0>
  *   val result0 = field0
  *   def field1 = <decode field 1>
  *   val result1 = if (<result0 succeeded>) field1 else result0
  *   ...
  *   if (<resultN succeeded>) <success>(new A(<value of result0>.asInstanceOf[F0], ...)) else <failure>(resultN)
  * }}}
  * (a failure is passed on by every later `resultK`, so the last one is either the first failure or the last success,
  * and when it succeeded every field did.)
  *
  * Accumulating ([[constructAccumulating]]) - every field is decoded and the failures are merged in field order by a
  * runtime helper of the module (`null` stands for "no failure yet"):
  * {{{
  *   def field0 = <decode field 0>
  *   val result0 = field0
  *   val errors0 = merge(null, result0)
  *   def field1 = <decode field 1>
  *   val result1 = field1
  *   val errors1 = merge(errors0, result1)
  *   ...
  *   if (errorsN == null) <success>(new A(<value of result0>.asInstanceOf[F0], ...)) else <failure>(errorsN)
  * }}}
  *
  * Why: the per-field code is in its own method, so the calling method grows by a small constant per field (HotSpot
  * does not JIT-compile methods over 8000 B), the branches are statements (no branch inside an argument list, which
  * makes the compiler spill every evaluated argument and emit a stack-map frame listing them: quadratic on Scala 2.13),
  * and the values are cast to the field types instead of being typed by a field decoder expression (which would be
  * evaluated again). Local `def`s, not local `lazy val`s: those allocate a `LazyRef` and synchronize on every call.
  *
  * The module supplies the per-field decode expressions and how to test, unwrap and merge its results; the names of the
  * generated definitions are fresh (Scala 2 quotes are not hygienic).
  */
trait FlatFieldsConstruct { this: MacroCommons =>

  /** Fail-fast construction (see [[FlatFieldsConstruct]]).
    *
    * @param fieldResults
    *   the per-field decode expressions in constructor-argument order, each put into a local `def`
    * @param isSuccess
    *   whether a (bound) field result succeeded, e.g. `r.isRight`
    * @param valueOf
    *   the value of a successful (bound) field result, e.g. `r.asInstanceOf[Right[E, Any]].value`
    * @param construct
    *   builds the value from the fields' values (in field order), casting each to its field's type
    * @param onSuccess
    *   wraps the constructed value, e.g. `Right(a)`
    * @param onFailure
    *   the result for a failed (bound) field result, e.g. `r.asInstanceOf[Either[E, A]]`
    */
  protected def constructFailFast[R: Type, A: Type, Out: Type](fieldResults: List[Expr[R]])(
      isSuccess: Expr[R] => Expr[Boolean],
      valueOf: Expr[R] => Expr[Any],
      construct: List[Expr[Any]] => Expr[A],
      onSuccess: Expr[A] => Expr[Out],
      onFailure: Expr[R] => Expr[Out]
  ): Expr[Out] = {
    def loop(remaining: List[Expr[R]], previous: Option[Expr[R]], boundReversed: List[Expr[R]]): Expr[Out] =
      remaining match {
        case field :: rest =>
          ValDefs.createDef[R](field, FreshName.FromPrefix("field")).use { fieldDef =>
            val result: Expr[R] = previous match {
              case None       => fieldDef
              case Some(prev) =>
                Expr.quote(if (Expr.splice(isSuccess(prev))) Expr.splice(fieldDef) else Expr.splice(prev))
            }
            ValDefs.createVal[R](result, FreshName.FromPrefix("fieldResult")).use { bound =>
              loop(rest, Some(bound), bound :: boundReversed)
            }
          }
        case Nil =>
          val bound = boundReversed.reverse
          val last = boundReversed.head
          val success = onSuccess(construct(bound.map(valueOf)))
          val failure = onFailure(last)
          Expr.quote(if (Expr.splice(isSuccess(last))) Expr.splice(success) else Expr.splice(failure))
      }
    if (fieldResults.isEmpty) onSuccess(construct(Nil))
    else loop(fieldResults, None, Nil)
  }

  /** Accumulating construction (see [[FlatFieldsConstruct]]).
    *
    * @param fieldResults
    *   the per-field decode expressions in constructor-argument order, each put into a local `def`
    * @param mergeErrors
    *   `(errors so far or null, bound field result) => errors or null`, a runtime helper call of the module: `null` and
    *   a success give `null`, `null` and a failure give its errors, errors and a failure give both in this order
    * @param valueOf
    *   the value of a successful (bound) field result
    * @param construct
    *   builds the value from the fields' values (in field order), casting each to its field's type
    * @param onSuccess
    *   wraps the constructed value
    * @param onFailure
    *   wraps the (non-null) merged errors
    * @param extraErrors
    *   errors merged after the fields' ones (e.g. unknown keys), `(errors or null) => errors or null`
    */
  protected def constructAccumulating[R: Type, E: Type, A: Type, Out: Type](fieldResults: List[Expr[R]])(
      mergeErrors: (Expr[E], Expr[R]) => Expr[E],
      valueOf: Expr[R] => Expr[Any],
      construct: List[Expr[Any]] => Expr[A],
      onSuccess: Expr[A] => Expr[Out],
      onFailure: Expr[E] => Expr[Out],
      extraErrors: Option[Expr[E] => Expr[E]] = None
  ): Expr[Out] =
    constructAccumulatingIndexed[R, E, A, Out](fieldResults)(
      (_, errors, result) => mergeErrors(errors, result),
      valueOf,
      construct,
      onSuccess,
      onFailure,
      extraErrors
    )

  /** [[constructAccumulating]] whose `mergeErrors` also gets the field's index (in `fieldResults`), e.g. to record the
    * failure under the field's name.
    */
  protected def constructAccumulatingIndexed[R: Type, E: Type, A: Type, Out: Type](fieldResults: List[Expr[R]])(
      mergeErrors: (Int, Expr[E], Expr[R]) => Expr[E],
      valueOf: Expr[R] => Expr[Any],
      construct: List[Expr[Any]] => Expr[A],
      onSuccess: Expr[A] => Expr[Out],
      onFailure: Expr[E] => Expr[Out],
      extraErrors: Option[Expr[E] => Expr[E]] = None
  ): Expr[Out] = {
    def finish(errors: Expr[E], bound: List[Expr[R]]): Expr[Out] = {
      val success = onSuccess(construct(bound.map(valueOf)))
      val failure = onFailure(errors)
      Expr.quote(if (Expr.splice(errors) == null) Expr.splice(success) else Expr.splice(failure))
    }
    def loop(remaining: List[Expr[R]], errors: Expr[E], boundReversed: List[Expr[R]]): Expr[Out] =
      remaining match {
        case field :: rest =>
          ValDefs.createDef[R](field, FreshName.FromPrefix("field")).use { fieldDef =>
            ValDefs.createVal[R](fieldDef, FreshName.FromPrefix("fieldResult")).use { bound =>
              val index = boundReversed.size
              ValDefs.createVal[E](mergeErrors(index, errors, bound), FreshName.FromPrefix("fieldErrors")).use { merged =>
                loop(rest, merged, bound :: boundReversed)
              }
            }
          }
        case Nil =>
          val bound = boundReversed.reverse
          extraErrors match {
            case Some(extra) =>
              ValDefs.createVal[E](extra(errors), FreshName.FromPrefix("allErrors")).use(finish(_, bound))
            case None => finish(errors, bound)
          }
      }
    loop(fieldResults, Expr.quote(null.asInstanceOf[E]), Nil)
  }
}
