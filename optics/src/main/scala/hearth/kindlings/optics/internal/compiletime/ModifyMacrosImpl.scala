package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommons
import hearth.fp.Id
import hearth.fp.instances.*

/** Shared, macro-platform-agnostic implementation of the `modify` optics macro.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `CaseClass`, `DestructuredExpr`, ...).
  *
  * Phase 1 supports field access only: a path `_.a.b.c` is parsed into a list of field names via Hearth's
  * [[hearth.typed.Exprs.DestructuredExpr.extractFieldPath]], and a nested copy-with-modification function is emitted by
  * reading every case field of each level ([[hearth.typed.Classes.CaseClass.caseFieldValuesAt]]) and rebuilding with the
  * focused field replaced ([[hearth.typed.Classes.CaseClass.construct]]). Collections/`.each`/`.at`/`.when`/Either and
  * lens composition are later phases.
  */
private[optics] trait ModifyMacrosImpl { this: MacroCommons =>

  /** `obj.modify(_.a.b.c)` → `PathModify[S, A]`. Parses the path lambda into field names, then emits
    * `PathModify(obj, (s, mod) => s.copy(a = s.a.copy(b = s.a.b.copy(c = mod(s.a.b.c)))))`.
    */
  def modify[S: Type, A: Type](obj: Expr[S], path: Expr[S => A]): Expr[PathModify[S, A]] = {
    val fieldNames = DestructuredExpr.extractFieldPath[S, A](path) match {
      case Right(fieldPath) => fieldPath.fieldNames
      case Left(error)      =>
        Environment.reportErrorAndAbort(
          s"`modify` expects a field-access path like `_.a.b.c`, but failed to parse the path: $error"
        )
    }

    // Build the copy-with-modification function `(s, mod) => <rebuilt s>`, where the leaf transformation applied inside
    // the nested copy is exactly `mod(<leaf>)`. The lambda parameters `s`/`mod` are quote-bound; their `Expr` handles
    // are recovered for the macro-side `buildModify` via `Expr.quote(s)` / `Expr.quote(mod)` (the standard Hearth
    // cross-quote idiom — see cats-derivation `FunctorMacrosImpl`).
    val doModify: Expr[(S, A => A) => S] =
      Expr.quote { (s: S, mod: A => A) =>
        Expr.splice {
          buildModify[S](Expr.quote(s), fieldNames) { leaf =>
            applyLeaf[S, A](Expr.quote(mod), leaf)
          }
        }
      }

    Expr.quote(PathModify[S, A](Expr.splice(obj), Expr.splice(doModify)))
  }

  /** Apply the user modification `mod: A => A` to the focused leaf. At the leaf the focused field's type IS the path's
    * leaf type `A`, so the incoming `leaf: Expr[S]` (whose `S` is structurally that field type) is reinterpreted as
    * `Expr[A]` and the result back to `Expr[S]`. The casts are macro-side `Expr` reinterpretations only — the generated
    * tree is a plain `mod(leaf)` call.
    */
  private def applyLeaf[S: Type, A: Type](mod: Expr[A => A], leaf: Expr[S]): Expr[S] =
    Expr.quote(Expr.splice(mod).apply(Expr.splice(leaf.asInstanceOf[Expr[A]]))).asInstanceOf[Expr[S]]

  /** Rebuild a value `sExpr: S` with the field reached by `fieldNames` transformed by `transformLeaf`. When
    * `fieldNames` is empty, `sExpr` itself is the focused leaf, so `transformLeaf` is applied directly. Otherwise the
    * head names the case field to descend into: read all of `S`'s case fields, recurse into the focused one, and
    * reconstruct `S` with that single field replaced.
    */
  private def buildModify[S: Type](sExpr: Expr[S], fieldNames: List[String])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    fieldNames match {
      case Nil          => transformLeaf(sExpr)
      case field :: rest =>
        CaseClass.parse[S] match {
          case ClassViewResult.Incompatible(reason) =>
            Environment.reportErrorAndAbort(
              s"`modify` can only descend into case classes, but [${Type.prettyPrint[S]}] is not one: $reason"
            )
          case ClassViewResult.Compatible(caseClass) =>
            val fieldValues = caseClass.caseFieldValuesAt(sExpr)
            val focused = fieldValues.getOrElse(
              field,
              Environment.reportErrorAndAbort(
                s"`modify`: no accessible field `$field` on [${Type.prettyPrint[S]}]"
              )
            )
            // Recurse into the focused field at its own (path-dependent) type, kept out of any quote via a helper.
            val rebuiltFocused = recurseInto(focused, rest, transformLeaf)
            reconstruct[S](caseClass, fieldValues.updated(field, rebuiltFocused))
        }
    }

  /** Recurse into a focused field expression carrying its existential type, so the path-dependent `Underlying` never
    * leaks into an `Expr.quote` (the di/mock Scala 2 pitfall). The `transformLeaf` is over `S`, but at the leaf the
    * field type and `S`/`A` coincide structurally — `transformLeaf` is only ever invoked when `rest` is empty and the
    * field type IS the focused `A`, so the cast is sound (and elided on the happy path where `rest` is non-empty).
    */
  private def recurseInto[S: Type](
      focused: Expr_??,
      rest: List[String],
      transformLeaf: Expr[S] => Expr[S]
  ): Expr_?? = {
    import focused.Underlying as F
    buildModify[F](focused.value, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[F]]).as_??
  }

  /** Reconstruct a case-class value from its (possibly modified) field expressions, by name. Mirrors di's constructor
    * application but for the case class's primary constructor; each argument is supplied verbatim from `fieldValues`.
    */
  private def reconstruct[S: Type](caseClass: CaseClass[S], fieldValues: Map[String, Expr_??]): Expr[S] =
    caseClass.construct[Id] { parameter =>
      fieldValues.getOrElse(
        parameter.name,
        Environment.reportErrorAndAbort(
          s"`modify`: missing value for field `${parameter.name}` of [${Type.prettyPrint[S]}]"
        )
      )
    } match {
      case Some(expr) => expr
      case None       =>
        Environment.reportErrorAndAbort(
          s"`modify`: the primary constructor of [${Type.prettyPrint[S]}] is not accessible at the call site"
        )
    }
}
