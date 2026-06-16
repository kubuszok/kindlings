package hearth.kindlings.di

/** A runtime registry of instances keyed by their (static) type, produced by [[DI.wiredInModule]].
  *
  * Mirrors the lookup/registry half of macwire's `Wired`: every public, parameterless member of the captured module is
  * registered under its declared type, and [[lookup]] returns all registered instances assignable to a queried class
  * (subtype-aware). Entries are factories (`() => Any`) re-invoked on each lookup, so a `def` member is re-read each
  * time, exactly like reading it off the module.
  *
  * The reflective dynamic-instantiation half of macwire's `Wired` (`wireClassInstance`/`wireClassInstanceByName`, which
  * construct a class by reflectively resolving its constructor parameters from the registry) is intentionally NOT
  * ported: it relies on JVM-only `java.lang.reflect` and a context classloader, which would not link on Scala.js /
  * Scala Native. Subtype matching here uses only [[java.lang.Class.isAssignableFrom]], which is portable.
  */
final class Wired private (private val entries: List[(java.lang.Class[Any], () => Any)]) {

  /** All registered instances whose registered type is assignable to `cls` (subtype-aware), in registration order. */
  def lookup[T](cls: java.lang.Class[T]): List[T] =
    entries.collect { case (registered, factory) if cls.isAssignableFrom(registered) => factory().asInstanceOf[T] }

  /** The single registered instance assignable to `cls`; throws if there are zero or several. */
  def lookupSingleOrThrow[T](cls: java.lang.Class[T]): T = lookup(cls) match {
    case single :: Nil => single
    case Nil           => throw new IllegalArgumentException(s"No instance of [${cls.getName}] is registered")
    case many          =>
      throw new IllegalArgumentException(s"Multiple instances of [${cls.getName}] are registered: ${many.size}")
  }

  /** A new [[Wired]] with the given instances added, each keyed by its runtime class. */
  def withInstances(instances: AnyRef*): Wired =
    new Wired(entries ++ instances.toList.map(i => i.getClass.asInstanceOf[java.lang.Class[Any]] -> (() => (i: Any))))

  /** A new [[Wired]] with an instance factory added (re-invoked on each lookup), keyed by the `ClassTag`'s class. */
  def withInstanceFactory[T <: AnyRef](factory: () => T)(implicit ct: scala.reflect.ClassTag[T]): Wired =
    new Wired(entries :+ (ct.runtimeClass.asInstanceOf[java.lang.Class[Any]] -> (() => (factory(): Any))))
}

object Wired {

  /** Build a [[Wired]] from type-keyed instance factories, preserving order. Used by the `wiredInModule` macro. */
  def apply(entries: List[(java.lang.Class[Any], () => Any)]): Wired = new Wired(entries)
}
