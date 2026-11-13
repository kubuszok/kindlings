package hearth.kindlings.circederivation.internal.compiletime

import hearth.MacroCommons

/** Flattens nested sealed hierarchies the way circe's `ConfiguredEncoder`/`ConfiguredDecoder` do: a child of a sum that
  * is itself a derived sum contributes its own children (recursively), so `Child` of
  * `sealed trait GrandParent; sealed trait Parent extends GrandParent; case class Child(..) extends Parent` is encoded
  * as `{"Child":{..}}` (or with the discriminator `"Child"`) instead of `{"Parent":{"Child":{..}}}`.
  *
  * An intermediate sum with a user-provided instance is kept as a single entry named after the intermediate type, and
  * that instance is used for it (upstream keeps such an instance under its constructor name).
  *
  * Hearth's `Enum.directChildren` returns the direct children on Scala 3 but pre-flattened leaves on Scala 2, so the
  * user-provided intermediates are recovered from the leaves' base classes there.
  */
trait SealedHierarchyFlattening { this: MacroCommons =>

  /** Nested sums are flattened for sealed traits/classes and Scala 3 enums, not for Enumerations, Java enums or union
    * types.
    */
  protected def flattensNestedSums[A: Type]: Boolean =
    !Type[A].isEnumeration && !Type[A].isJavaEnum && !Type.isUnionType[A]

  /** A child that is itself a sum type (a sealed trait or abstract class with known children, not a singleton). */
  protected def isNestedSum[C: Type]: Boolean =
    Type[C].isSealed && !Type[C].isObject && !Type[C].isVal && !Type[C].isJavaEnum && !Type[C].isEnumeration &&
      SingletonValue.unapply(Type[C]).isEmpty && Enum.parse[C].toOption.isDefined

  protected def isNestedSumType(tpe: ??): Boolean = {
    import tpe.Underlying as T
    isNestedSum[T]
  }

  private def isSubtype(a: ??, b: ??): Boolean = {
    import a.Underlying as X
    import b.Underlying as Y
    Type[X] <:< Type[Y]
  }

  private def isSame(a: ??, b: ??): Boolean = {
    import a.Underlying as X
    import b.Underlying as Y
    Type[X] =:= Type[Y]
  }

  private def childrenOf[B](enumm: Enum[B]): List[(String, ??)] =
    enumm.directChildren.toList.map { case (name, child) =>
      import child.Underlying as C
      (name, Type[C].as_??)
    }

  /** The entries a sum `A` dispatches on: leaf subtypes, with nested derived sums expanded, and intermediate sums that
    * have a user-provided instance (as reported by `hasUserInstance`) kept as one entry. Declaration order is kept.
    */
  protected def flattenedChildren[A: Type](enumm: Enum[A])(hasUserInstance: ?? => Boolean): List[(String, ??)] = {
    val direct = childrenOf(enumm)
    if (!flattensNestedSums[A]) direct
    else {
      def expand(children: List[(String, ??)]): List[(String, ??)] = children.flatMap { case entry @ (_, child) =>
        import child.Underlying as C
        if (isNestedSum[C] && !hasUserInstance(child))
          Enum.parse[C].toOption match {
            case Some(nested) => expand(childrenOf(nested))
            case None         => List(entry)
          }
        else List(entry)
      }
      def dedupe(entries: List[(String, ??)]): List[(String, ??)] =
        entries
          .foldLeft(List.empty[(String, ??)]) { case (acc, entry @ (_, tpe)) =>
            if (acc.exists { case (_, seen) => isSame(seen, tpe) }) acc else entry :: acc
          }
          .reverse

      val expanded = dedupe(expand(direct))

      // Scala 2: `directChildren` already returned the leaves, so look for user-provided intermediates among the leaves'
      // base classes (strictly between the leaf and `A`). On Scala 3 such intermediates were not expanded above.
      val self = Type[A].as_??
      val intermediates: List[??] = dedupe(expanded.flatMap { case (_, leaf) =>
        import leaf.Underlying as L
        Type.baseClasses[L].collect {
          case base
              if !isSame(base, leaf) && !isSame(base, self) && isSubtype(base, self) &&
                isNestedSumType(base) && hasUserInstance(base) =>
            ("", base)
        }
      }).map(_._2)
      val outermost = intermediates.filterNot(p => intermediates.exists(q => !isSame(q, p) && isSubtype(p, q)))

      if (outermost.isEmpty) expanded
      else
        dedupe(expanded.map { case entry @ (_, leaf) =>
          outermost.find(p => isSubtype(leaf, p)) match {
            case Some(p) =>
              import p.Underlying as P
              (Type[P].shortName, p)
            case None => entry
          }
        })
    }
  }

  /** A sum type whose derived instances are marked as `KindlingsSumEncoder`/`KindlingsSumDecoder` (so that they are
    * flattened when used as the instance of an intermediate sealed trait). Options and collections are excluded.
    */
  protected def isMarkedSum[A: Type]: Boolean =
    flattensNestedSums[A] && isNestedSum[A] && !(Type[A] <:< Type.of[Option[Any]]) &&
      !(Type[A] <:< Type.of[Iterable[Any]])

  /** The names of the direct subtypes of `A` (circe's `constructorNames`, untransformed), in declaration order. On
    * Scala 2 `directChildren` returns the leaves, so a leaf is replaced by its outermost base strictly below `A`.
    */
  protected def directChildNames[A: Type](enumm: Enum[A]): List[String] = {
    val direct = childrenOf(enumm)
    if (!flattensNestedSums[A]) direct.map(_._1)
    else {
      val self = Type[A].as_??
      direct.map { case (name, leaf) =>
        import leaf.Underlying as L
        val bases = Type.baseClasses[L].filter { base =>
          !isSame(base, leaf) && !isSame(base, self) && isSubtype(base, self) && isSubtype(leaf, base)
        }
        val outermost = bases.filterNot(p => bases.exists(q => !isSame(q, p) && isSubtype(p, q)))
        outermost.headOption.fold(name) { base =>
          import base.Underlying as B
          Type[B].shortName
        }
      }.distinct
    }
  }

  /** The entry (from [[flattenedChildren]]) that a matched subtype `C` is dispatched as. */
  protected def entryFor[C: Type](entries: List[(String, ??)]): Option[(String, ??)] = {
    val c = Type[C].as_??
    entries.find { case (_, entry) => isSame(c, entry) }.orElse(entries.find { case (_, entry) => isSubtype(c, entry) })
  }

  /** Upstream circe does not derive codecs for case classes with several parameter lists (its `Mirror`/`Generic` only
    * cover the first one), and encoding all of them while decoding only some would not round-trip: reject them.
    */
  protected def rejectMultipleParameterLists[A: Type](caseClass: CaseClass[A], macroName: String): Option[String] =
    if (SingletonValue.unapply(Type[A]).isDefined) None
    else if (caseClass.primaryConstructor.totalParameters.drop(1).exists(_.nonEmpty))
      Some(
        s"$macroName: ${Type[A].prettyPrint} has multiple parameter lists, which circe derivation does not support. " +
          "Move all fields into the first parameter list or provide an instance manually."
      )
    else None

  /** An `AnyVal` case class: unwrapped to its field by default, coded as a product (like upstream circe) when
    * `unwrapValueClasses` is disabled. Other value types (opaque types, refined/newtype integrations) are always
    * unwrapped.
    */
  protected def anyValCaseClass[A: Type]: Option[CaseClass[A]] =
    if (Type[A] <:< Type.of[AnyVal] && !Type[A].isPrimitive && !Type[A].isNamedTuple)
      CaseClass.parse[A].toOption.filter(_ => SingletonValue.unapply(Type[A]).isEmpty)
    else None

  /** Evaluates the configuration at compile time when possible (`Some` enables the compile-time checks and the
    * optimized codegen). `semiEval` follows immutable local `val`s on Scala 3 (including the proxies Scala 3 creates
    * for the non-inline arguments of inline methods, like `derived[A](using Configuration.default.withX)`) and refuses
    * `var`s and members of the module being compiled. Overridden on Scala 3 (`SemiEvalConfigScala3`), see
    * `docs/research/hearth-gap-semieval-named-args.md`.
    */
  protected def semiEvalConfig(
      configExpr: Expr[hearth.kindlings.circederivation.Configuration]
  ): Option[hearth.kindlings.circederivation.Configuration] =
    configExpr.semiEval.toOption

  /** The configuration evaluated at compile time (when `semiEval` can), set by every entry point. */
  protected var evaluatedConfigFlags: Option[hearth.kindlings.circederivation.Configuration] = None

  /** The single field of a case class, for the explicit unwrapping API (generic-extras' `deriveUnwrapped*`). */
  protected def singleFieldCaseClass[A: Type](macroName: String): (CaseClass[A], Parameter) =
    CaseClass.parse[A].toOption.filterNot(_ => Type[A].isTuple || SingletonValue.unapply(Type[A]).isDefined) match {
      case Some(cc) =>
        cc.primaryConstructor.totalParameters.flatten.toList match {
          case (_, param) :: Nil => (cc, param)
          case other             =>
            Environment.reportErrorAndAbort(
              s"$macroName: ${Type[A].prettyPrint} must be a case class with exactly one field, it has ${other.size}."
            )
        }
      case None =>
        Environment.reportErrorAndAbort(
          s"$macroName: ${Type[A].prettyPrint} is not a case class with exactly one field."
        )
    }
}
