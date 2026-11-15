package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

import scala.util.Try

/** Points users at the Kindlings integration module which makes a refined/newtype type a value type, when a derivation
  * fails because such a type is not one (e.g. a map key the JSON library cannot write): Iron's `IronType` (`A :| C`)
  * and `RefinedType`/`RefinedSubtype` companions' `T`, refined's `Refined[A, P]`, neotype's `Newtype`/`Subtype` and
  * scala-newtype's `@newtype`/`@newsubtype` are only value types with `kindlings-iron-integration`,
  * `kindlings-refined-integration`, `kindlings-neotype-integration` or `kindlings-newtype-integration` on the
  * classpath.
  *
  * Types are recognised by the full names of the type, of its base classes and of the object standing for it (see
  * `Type.pseudoCompanionObject`), so no library has to be on the macro's classpath.
  */
object ValueTypeIntegrationHints {

  /** A refined/newtype library and the Kindlings module integrating it. */
  final case class Integration(library: String, module: String)

  val Iron: Integration = Integration("Iron", "kindlings-iron-integration")
  val Refined: Integration = Integration("refined", "kindlings-refined-integration")
  val Neotype: Integration = Integration("neotype", "kindlings-neotype-integration")
  val Newtype: Integration = Integration("scala-newtype", "kindlings-newtype-integration")

  /** Synthetic name standing for a scala-newtype `@newtype` companion (recognised by its members, not by a parent). */
  val ScalaNewtypeCompanionMarker: String = "io.estatico.newtype.macros.newtype"

  private val integrationByName: List[(String, Integration)] = List(
    "io.github.iltotore.iron.IronType" -> Iron,
    "io.github.iltotore.iron.:|" -> Iron,
    "io.github.iltotore.iron.RefinedType" -> Iron,
    "io.github.iltotore.iron.RefinedSubtype" -> Iron,
    "eu.timepit.refined.api.Refined" -> Refined,
    "neotype.Newtype" -> Neotype,
    "neotype.Subtype" -> Neotype,
    ScalaNewtypeCompanionMarker -> Newtype
  )

  /** The integration for a type whose own, base classes' and companion's full names (without type arguments) are
    * `names`.
    */
  def integrationFor(names: Iterable[String]): Option[Integration] = {
    val stripped = names.map(_.takeWhile(_ != '[').stripSuffix(".type")).toSet
    integrationByName.collectFirst { case (name, integration) if stripped(name) => integration }
  }

  /** The sentence pointing at `integration`'s module, about the type `typeName`. */
  def sentenceFor(typeName: String, integration: Integration): String =
    s"$typeName looks like a type of ${integration.library}: add ${integration.module} to the dependencies to make it " +
      "a value type wrapping its underlying type"

  /** The hint appended to an error about `typeName`, e.g. `" (... looks like a type of Iron: ...)"`. */
  def hintFor(typeName: String, integration: Integration): String = s" (${sentenceFor(typeName, integration)})"

  /** The hint for `names` (see [[integrationFor]]), or `""`. */
  def hintForNames(typeName: String, names: Iterable[String]): String =
    integrationFor(names).fold("")(hintFor(typeName, _))

  /** The hint for the type `tpe` (`" (... looks like a type of Iron: ...)"`), or `""` if it does not look like a type
    * of a refined/newtype library.
    */
  def hint[A](ctx: MacroCommons)(tpe: ctx.Type[A]): String =
    sentence(ctx)(tpe).fold("")(s => s" ($s)")

  /** The hint for the type `tpe` as a sentence on its own (e.g. for a list of reasons), if it looks like a type of a
    * refined/newtype library.
    */
  def sentence[A](ctx: MacroCommons)(tpe: ctx.Type[A]): Option[String] = {
    import ctx.*
    def namesOf[T](implicit T: Type[T]): List[String] =
      Type.fqcn[T] :: Try(Type.baseClasses[T].map(b => Type.fqcn(using b.Underlying))).getOrElse(Nil)
    val own = namesOf(using tpe)
    val companion = Try(Type.pseudoCompanionObject(using tpe)).toOption.flatten.toList.flatMap { c =>
      import c.Underlying as Companion
      val newtypeMarker =
        if (Try(Class[Companion].method("unsafeUnwrap").nonEmpty).getOrElse(false)) List(ScalaNewtypeCompanionMarker)
        else Nil
      namesOf[Companion] ++ newtypeMarker
    }
    integrationFor(own ++ companion).map(sentenceFor(Type.prettyPrint(using tpe), _))
  }
}
