package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs

/** The subtypes of a sum as a JSON library that flattens nested sealed hierarchies writes them (see
  * `JsonSchemaConfig.flattensNestedSubtypes`): the leaves, except that an intermediate sealed hierarchy with its own
  * instance in the JSON library (`JsonSchemaConfig.nestedSubtypeInstance`) is kept as one subtype - or, when that
  * instance turns out (at runtime) to be one the library derived for that sum, replaced by its own subtypes again.
  *
  * Hearth's `Enum.directChildren` returns the direct children on Scala 3 but the leaves on Scala 2, so the
  * intermediates are recovered from the leaves' base classes (on both).
  */
trait SchemaSubtypeFlattening { this: MacroCommons & StdExtensions & JsonSchemaConfigs =>

  sealed trait SubtypeEntry
  object SubtypeEntry {

    /** A subtype written under its own (short) `name`. */
    final case class Leaf(name: String, tpe: ??) extends SubtypeEntry

    /** An intermediate sum with its own instance: one subtype named `name`, or (when `flattened` is `true` at runtime)
      * its `entries`.
      */
    final case class Intermediate(name: String, tpe: ??, flattened: Expr[Boolean], entries: List[SubtypeEntry])
        extends SubtypeEntry
  }

  /** Whether the JSON library flattens the nested sealed hierarchies of `A` (not Enumerations, Java enums or unions).
    */
  protected def flattensNestedSubtypesOf[A: Type](jsonCfg: JsonSchemaConfig): Boolean =
    jsonCfg.flattensNestedSubtypes && !Type[A].isEnumeration && !Type[A].isJavaEnum && !Type.isUnionType[A]

  private def isSame(a: ??, b: ??): Boolean = {
    import a.Underlying as X
    import b.Underlying as Y
    Type[X] =:= Type[Y]
  }

  private def isSubtype(a: ??, b: ??): Boolean = {
    import a.Underlying as X
    import b.Underlying as Y
    Type[X] <:< Type[Y]
  }

  private def isNestedSum(tpe: ??): Boolean = {
    import tpe.Underlying as C
    Type[C].isSealed && !Type[C].isObject && !Type[C].isVal && !Type[C].isJavaEnum && !Type[C].isEnumeration &&
    SingletonValue.unapply(Type[C]).isEmpty && Enum.parse[C].toOption.isDefined
  }

  private def dedupe(entries: List[(String, ??)]): List[(String, ??)] =
    entries
      .foldLeft(List.empty[(String, ??)]) { case (acc, entry @ (_, tpe)) =>
        if (acc.exists { case (_, seen) => isSame(seen, tpe) }) acc else entry :: acc
      }
      .reverse

  /** All the leaves of `A` (short names), in declaration order, without duplicates (a leaf reachable through several
    * intermediates is listed once).
    */
  protected def subtypeLeaves[A: Type](e: Enum[A]): List[(String, ??)] = {
    def expand(children: List[(String, ??)]): List[(String, ??)] = children.flatMap { case entry @ (_, child) =>
      import child.Underlying as C
      if (isNestedSum(child))
        Enum.parse[C].toOption match {
          case Some(nested) => expand(nested.directChildren.toList.map { case (n, c) => (n, c.Underlying.as_??) })
          case None         => List(entry)
        }
      else List(entry)
    }
    dedupe(expand(e.directChildren.toList.map { case (n, c) => (n, c.Underlying.as_??) }))
  }

  /** The subtypes of `A` as written by the JSON library (see [[SchemaSubtypeFlattening]]). */
  protected def flattenedSubtypes[A: Type](e: Enum[A], jsonCfg: JsonSchemaConfig): List[SubtypeEntry] = {
    val instances = scala.collection.mutable.Map.empty[String, Option[Expr[Boolean]]]
    def instanceOf(tpe: ??): Option[Expr[Boolean]] = {
      import tpe.Underlying as T
      instances.getOrElseUpdate(Type.plainPrint[T], jsonCfg.nestedSubtypeInstance(tpe))
    }
    // The intermediates with an instance strictly between `leaf` and `root`.
    def intermediatesOf(leaf: ??, root: ??): List[??] = {
      import leaf.Underlying as L
      Type.baseClasses[L].filter { base =>
        !isSame(base, leaf) && !isSame(base, root) && isSubtype(base, root) && isNestedSum(base) &&
        instanceOf(base).isDefined
      }
    }
    def group(leaves: List[(String, ??)], root: ??): List[SubtypeEntry] = {
      val outermostOf: List[((String, ??), Option[??])] = leaves.map { case entry @ (_, leaf) =>
        val candidates = intermediatesOf(leaf, root)
        entry -> candidates.find(p => !candidates.exists(q => !isSame(q, p) && isSubtype(p, q)))
      }
      outermostOf
        .foldLeft((List.empty[SubtypeEntry], List.empty[??])) { case ((acc, emitted), ((name, leaf), outermost)) =>
          outermost match {
            case None                                               => (SubtypeEntry.Leaf(name, leaf) :: acc, emitted)
            case Some(p) if emitted.exists(seen => isSame(seen, p)) => (acc, emitted)
            case Some(p)                                            =>
              import p.Underlying as P
              val inner = outermostOf.collect { case (entry, Some(q)) if isSame(q, p) => entry }
              val entry = SubtypeEntry.Intermediate(Type[P].shortName, p, instanceOf(p).get, group(inner, p))
              (entry :: acc, p :: emitted)
          }
        }
        ._1
        .reverse
    }
    group(subtypeLeaves(e), Type[A].as_??)
  }
}
