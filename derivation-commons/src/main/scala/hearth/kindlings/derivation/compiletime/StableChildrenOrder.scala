package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.typed.ChildrenOrder

/** The order in which derivation modules see the subtypes of a sealed hierarchy, made independent of the build.
  *
  * Hearth reports the children in declaration order whenever the compiler knows it: always on Scala 3 (the order
  * `Mirror.SumOf` uses), but on Scala 2 only for a sealed type compiled in the same run as the macro expansion - Scala
  * 2 class files store the subtypes sorted by name, so for a sealed type from another module, or one that an
  * incremental build did not recompile, the children come sorted by name. Anything observable that depends on the
  * subtype order (schema symbols, union branches, the order subtypes are tried in, generator order, names listed in
  * errors, ...) would then differ between a clean and an incremental build.
  *
  * So on Scala 2 every module uses Hearth's `Type.directChildrenInOrder(ChildrenOrder.ByName)` (simple name in natural
  * order, then the full name; the leaves of nested sealed hierarchies come depth-first in that order), and on Scala 3
  * the declaration order. Java enums are put in their constants' declaration order (the order of the static fields in
  * the class file): on Scala 3 Hearth lists them in the order in which the compiler completed them, the constants
  * referred to by the compiled code first (docs/research/hearth-bug-java-enum-unreferenced-constants.md).
  * `scala.Enumeration` values keep the order Hearth reports.
  *
  * Usage, from a macro bundle trait `MyRuleImpl { this: MacroCommons => ... }`:
  * `StableChildrenOrder.directChildren(MyRuleImpl.this)(enumm)` (`MyRuleImpl.this`, not `this`, inside rule objects).
  */
object StableChildrenOrder {

  /** The direct children of `enumm` in the build-independent order (see [[StableChildrenOrder]]). */
  def directChildren[A](mc: MacroCommons)(enumm: mc.Enum[A]): List[(String, mc.??<:[A])] =
    inStableOrder(mc)(enumm.directChildren.toList)(enumm.tpe)

  /** The recursively flattened leaf children of `enumm` in the build-independent order (see [[StableChildrenOrder]]),
    * or `None` if Hearth cannot flatten the hierarchy.
    */
  def exhaustiveChildren[A](mc: MacroCommons)(enumm: mc.Enum[A]): Option[List[(String, mc.??<:[A])]] =
    enumm.exhaustiveChildren.map(children => inStableOrder(mc)(children.toList)(enumm.tpe))

  /** Puts children of `A` - direct ones or leaves of nested sealed hierarchies, as listed by Hearth - in the
    * build-independent order (see [[StableChildrenOrder]]).
    */
  def inStableOrder[A](mc: MacroCommons)(children: List[(String, mc.??<:[A])])(implicit
      A: mc.Type[A]
  ): List[(String, mc.??<:[A])] = {
    import mc.*
    // Java enums: the declaration order of the constants (on Scala 3 Hearth reports the order in which the compiler
    // completed them, which puts the constants referred to by the compiled code first)
    val javaEnumConstants = if (A.isJavaEnum) JavaEnumConstants(mc)(A.asUntyped) else None
    if (javaEnumConstants.isDefined) inOrderOf(children, javaEnumConstants.getOrElse(Nil))(_._1 == _)
    else if (Environment.isScala3 || A.isJavaEnum || A.isEnumeration) children
    else {
      // Every subtype of A (each direct child, followed by the subtypes of a sealed one), depth-first, by name.
      def byName(tpe: ??, depth: Int): List[??] = {
        import tpe.Underlying as T
        if (depth > 32 || !Type[T].isSealed) Nil
        else
          Type[T].directChildrenInOrder(ChildrenOrder.ByName).getOrElse(Nil).flatMap { case (_, child) =>
            val childTpe = child.Underlying.as_??
            childTpe :: byName(childTpe, depth + 1)
          }
      }
      inOrderOf(children, byName(A.as_??, 0)) { case ((_, child), reference) =>
        import reference.Underlying as R
        child.Underlying =:= Type[R]
      }
    }
  }

  /** `items` sorted by the position of their first match in `reference`; items without a match go last, in their
    * original order. Stable for items matching the same position.
    */
  def inOrderOf[T, R](items: List[T], reference: List[R])(matches: (T, R) => Boolean): List[T] = {
    val indexed = reference.toVector
    items.sortBy { item =>
      val index = indexed.indexWhere(matches(item, _))
      if (index >= 0) index else Int.MaxValue
    }
  }
}
