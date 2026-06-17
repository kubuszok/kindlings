package hearth.kindlings.optics

/** Phantom capability that gates the `modify` path markers (`.each`/`.eachWhere`, `.at`/`.index`/`.atOrElse`,
  * `.eachLeft`/`.eachRight`, `.when`) on Scala 3.
  *
  * The `modify`/`modifyLens` path parameter is a **context function** `OpticsContext ?=> (S => A)`, which introduces a
  * `given OpticsContext` into scope for the path body ONLY. Each marker extension method requires `(using
  * OpticsContext)`, so they are applicable only inside a `modify(...)` path — outside one there is no `OpticsContext`
  * anywhere (the trait is `sealed` with no public instance and no `given`), so the markers do not even appear in IDE
  * completion on an ordinary collection/`Option`/`Either` value.
  *
  * The macro never runs the markers: it deconstructs the path AST. It peels this `OpticsContext` layer with
  * `Expr.betaReduce` (applying the context function to a throwaway value) and then parses the inner `S => A` lambda
  * exactly as on Scala 2. `OpticsContext extends PathStepEvidence`, so the synthesized `using OpticsContext` argument is
  * stripped by the same `isSubtypeOf[_, PathStepEvidence]` check that already drops the `IsElementOf`/… evidences.
  *
  * Scala 2 has no context functions, so this gating is Scala-3-only; the Scala 2 markers stay guarded by
  * `@compileTimeOnly` + the implicit-conversion projection (see `syntax.scala`).
  */
sealed trait OpticsContext extends PathStepEvidence
