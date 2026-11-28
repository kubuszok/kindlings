package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*

/** The implicit searches of the "use implicit when available" rules: a user-provided instance always wins over the
  * built-in rules, so how it is searched decides what the derivation does.
  *
  *   - [[summonUserInstance]]: like `summonExprIgnoring`, but a search that failed because the macro of a candidate
  *     crashed (threw an exception) is told apart from "no instance" (Scala 3 only: the Scala 2 compiler does not say
  *     why a candidate failed), so that the rule can fail with the reason instead of silently deriving the type
  *     structurally.
  *   - [[completeUserInstance]]: a user's generic instance (`implicit def enc[A](implicit a: Encoder[A]):
  *     Encoder[Page[A]]`) that is not found only because the instances it needs do not exist yet (`Encoder[Item]` for a
  *     case class `Item` that the derivation would derive) is completed with instances derived by the module, instead
  *     of deriving `Page[Item]` structurally.
  */
trait UserInstances extends UserInstancesPlatform { this: MacroCommons & DerivationPolicy =>

  /** The outcome of [[summonUserInstance]]. */
  sealed trait UserInstanceSearch[Inst] extends Product with Serializable {

    /** The instance, or why there is none (a crash is reported as "no instance": use [[crashReason]] to fail on it). */
    final def toEither: Either[String, Expr[Inst]] = this match {
      case UserInstanceSearch.Found(instance)  => Right(instance)
      case UserInstanceSearch.NotFound(reason) => Left(reason)
      case UserInstanceSearch.Crashed(reason)  => Left(reason)
    }

    final def crashReason: Option[String] = this match {
      case UserInstanceSearch.Crashed(reason) => Some(reason)
      case _                                  => None
    }
  }
  object UserInstanceSearch {
    final case class Found[Inst](instance: Expr[Inst]) extends UserInstanceSearch[Inst]
    final case class NotFound[Inst](reason: String) extends UserInstanceSearch[Inst]

    /** The search failed because the macro expansion of a candidate threw an exception. */
    final case class Crashed[Inst](reason: String) extends UserInstanceSearch[Inst]
  }

  /** Summons `Inst`, ignoring `ignored` (the module's own auto-derivation implicits). */
  protected def summonUserInstance[Inst: Type](ignored: Seq[UntypedMethod]): UserInstanceSearch[Inst] =
    summonIgnoringExplained[Inst](ignored) match {
      case Right(instance)       => UserInstanceSearch.Found(instance)
      case Left((reason, false)) => UserInstanceSearch.NotFound(reason)
      case Left((reason, true))  => UserInstanceSearch.Crashed(reason)
    }

  /** Fails the derivation of `Inst`: a candidate implicit crashed while it was searched. */
  protected def failOnCrashedUserInstance[Inst: Type, Out](reason: String): MIO[Out] =
    Log.error(s"The implicit search for ${Type[Inst].prettyPrint} crashed: $reason") >>
      MIO.fail(new UserInstances.CrashedImplicitSearch(Type[Inst].prettyPrint, reason))

  /** Derives the instance of a dependency of a user's generic instance (see [[completeUserInstance]]). */
  trait DependencyInstance[TC[_]] {
    def apply[B: Type]: MIO[Expr[TC[B]]]
  }

  /** A user's generic instance for `A` (a case class, case object, sealed hierarchy or enum with type arguments) whose
    * search failed only because the instances of some of `A`'s type arguments do not exist: those instances are derived
    * with `dependency` (by the module, e.g. as lazy vals of the instance being derived, so recursion through the user's
    * instance works) and the user's instance is built with them. `None` when there is no such instance - the type is
    * then derived as before.
    *
    * Only the type arguments of `A` are offered (not types nested deeper, nor other type classes), and only those that
    * have no instance yet, and only those the found instance actually uses are derived.
    */
  protected def completeUserInstance[TC[_], A: Type](
      typeClass: Type.Ctor1[TC],
      ignored: Seq[UntypedMethod]
  )(dependency: DependencyInstance[TC]): MIO[Option[Expr[TC[A]]]] =
    MIO.pure(()).flatMap { _ =>
      val typeArguments = Type.typeArguments[A]
      if (typeArguments.isEmpty || !isStructuralDerivationTarget[A])
        MIO.pure(None)
      else
        completeUserInstanceOf[TC, A](typeClass, ignored, typeArguments)(dependency)
    }

  private def completeUserInstanceOf[TC[_], A: Type](
      typeClass: Type.Ctor1[TC],
      ignored: Seq[UntypedMethod],
      typeArguments: List[??]
  )(dependency: DependencyInstance[TC]): MIO[Option[Expr[TC[A]]]] = {
    val missing = typeArguments
      .foldLeft(List.empty[??]) { (distinct, argument) =>
        if (distinct.exists(_.Underlying =:= argument.Underlying)) distinct else distinct :+ argument
      }
      .filter { argument =>
        isProperType(argument)
      }
      .filter { argument =>
        import argument.Underlying as B
        implicit val DependencyT: Type[TC[B]] = typeClass[B]
        summonIgnoringExplained[TC[B]](ignored).isLeft
      }
    if (missing.isEmpty)
      Log.info(s"No user instance to complete for ${Type[A].prettyPrint}: all its type arguments have instances") >>
        MIO.pure(None)
    else {
      implicit val InstanceT: Type[TC[A]] = typeClass[A]
      summonIgnoringWithGivens[TC, A](typeClass, missing, ignored) match {
        case None =>
          Log.info(
            s"No user instance for ${Type[A].prettyPrint} could be completed with instances for " +
              missing.map(_.Underlying.prettyPrint).mkString(", ")
          ) >> MIO.pure(None)
        case Some((used, instance)) =>
          val usedTypes = used.map(missing(_))
          Log.info(
            s"Completing the implicit ${Type[TC[A]].prettyPrint} with derived instances for " +
              usedTypes.map(_.Underlying.prettyPrint).mkString(", ")
          ) >>
            usedTypes
              .foldLeft(MIO.pure(Vector.empty[Expr[Any]])) { (previous, argument) =>
                import argument.Underlying as B
                previous.flatMap(done => dependency[B].map(expr => done :+ expr.asInstanceOf[Expr[Any]]))
              }
              .map(dependencies => Some(instance(dependencies.toList)))
      }
    }
  }
}

object UserInstances {

  /** Thrown (via `MIO.fail`) when the implicit search of a user-provided instance failed because a candidate's macro
    * expansion crashed: deriving the type structurally instead would silently change the result.
    */
  final class CrashedImplicitSearch(typeName: String, reason: String)
      extends RuntimeException(
        s"The implicit search for $typeName failed because the macro expansion of a candidate crashed " +
          s"(the type is not derived instead, as that could silently change the result): $reason"
      )

  private val crashMarker = "Exception occurred while executing macro expansion"

  /** Whether the explanation of a failed implicit search (Scala 3) says that a candidate's macro crashed. */
  def isCrashExplanation(explanation: String): Boolean = explanation.contains(crashMarker)

  /** The explanation of a crashed search without the stack trace: the exception and its message. */
  def crashReason(explanation: String): String = {
    val lines = explanation.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    val fromMarker = lines.dropWhile(line => !line.contains(crashMarker))
    val (message, _) = fromMarker.drop(1).span(line => !line.startsWith("at "))
    (fromMarker.take(1) ++ message.take(10)).mkString(" ")
  }
}
