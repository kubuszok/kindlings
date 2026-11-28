package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Scala 3 part of [[UserInstances]]: implicit searches that Hearth's `summonExprIgnoring` does not offer.
  *
  *   - The reason of a failed search, to tell a crashed macro expansion of a candidate (an exception thrown by its
  *     implementation) from a candidate that does not apply.
  *   - A search as if some instances existed (see [[summonIgnoringWithGivens]]).
  */
trait UserInstancesPlatform { this: MacroCommons =>

  /** The instance, or why there is none and whether a candidate's macro expansion crashed. */
  protected def summonIgnoringExplained[Inst: Type](
      ignored: Seq[UntypedMethod]
  ): Either[(String, Boolean), Expr[Inst]] = this match {
    case scala3: hearth.MacroCommonsScala3 =>
      import scala3.quotes.reflect.*
      given scala.quoted.Type[Inst] = Type[Inst].asInstanceOf[scala.quoted.Type[Inst]]
      val symbols = ignored.map(_.asInstanceOf[scala3.UntypedMethod].symbol)
      Implicits.searchIgnoring(TypeRepr.of[Inst])(symbols*) match {
        case success: ImplicitSearchSuccess => Right(success.tree.asExprOf[Inst].asInstanceOf[Expr[Inst]])
        case failure: ImplicitSearchFailure =>
          val explanation = failure.explanation
          if UserInstances.isCrashExplanation(explanation) then Left((UserInstances.crashReason(explanation), true))
          else
            failure match {
              case _: AmbiguousImplicits => Left((s"Ambiguous implicit value of type ${Type[Inst].prettyPrint}", false))
              case _: DivergingImplicit  => Left((s"Diverging implicit value of type ${Type[Inst].prettyPrint}", false))
              case _                     => Left((s"No implicit value of type ${Type[Inst].prettyPrint} found", false))
            }
      }
    case _ => Type[Inst].summonExprIgnoring(ignored*).toEither.left.map(reason => (reason, false))
  }

  /** Whether `tpe` is a proper type (not a type constructor), so that it can have a type class instance. */
  protected def isProperType(tpe: ??): Boolean = this match {
    case scala3: hearth.MacroCommonsScala3 =>
      import scala3.quotes.reflect.*
      import tpe.Underlying as T
      UntypedType.fromTyped[T].asInstanceOf[TypeRepr].dealias match {
        case _: TypeLambda  => false
        case _: AppliedType => true
        case other          => other.typeSymbol.typeMembers.forall(!_.isTypeParam)
      }
    case _ => true
  }

  /** Searches `TC[A]` as if `TC[B]` existed for the type arguments `missing` of `A` (`A = F[..., B, ...]`). On success:
    * the indices (in `missing`) of the instances the found instance uses (at least one) and the builder of the instance
    * from the expressions of those instances (in the same order).
    *
    * The probe searches `TC[F[..., B & KindlingsDependencyN[TC], ...]]`: a user's `implicit def enc[X](implicit x:
    * TC[X]): TC[F[X]]` infers `X = B & KindlingsDependencyN[TC]` and finds the placeholder `TC[X]` of the marker's
    * companion. The generated code is `{ given TC[B] = dependency; summonInline[TC[A]] }`: the instance is resolved
    * again where it is used, with the real instance given.
    */
  protected def summonIgnoringWithGivens[TC[_], A: Type](
      typeClass: Type.Ctor1[TC],
      missing: List[??],
      ignored: Seq[UntypedMethod]
  ): Option[(List[Int], List[Expr[Any]] => Expr[TC[A]])] = this match {
    case scala3: hearth.MacroCommonsScala3
        if missing.nonEmpty && missing.size <= KindlingsDependency.markerClassNames.size =>
      import scala3.quotes.reflect.*
      implicit val InstT: Type[TC[A]] = typeClass[A]
      val symbols = ignored.map(_.asInstanceOf[scala3.UntypedMethod].symbol)
      val missingReprs: List[TypeRepr] = missing.map { argument =>
        import argument.Underlying as B
        UntypedType.fromTyped[B].asInstanceOf[TypeRepr]
      }
      val markers: List[Symbol] = KindlingsDependency.markerClassNames.toList.map(Symbol.requiredClass)
      (
        UntypedType.fromTyped[TC[A]].asInstanceOf[TypeRepr],
        UntypedType.fromTyped[A].asInstanceOf[TypeRepr].dealias
      ) match {
        case (AppliedType(typeClassCtor, List(_)), AppliedType(subjectCtor, arguments)) =>
          val marked = arguments.map { argument =>
            missingReprs.indexWhere(_ =:= argument) match {
              case -1    => argument
              case index => AndType(argument, markers(index).typeRef.appliedTo(typeClassCtor))
            }
          }
          val probe = typeClassCtor.appliedTo(subjectCtor.appliedTo(marked))
          Implicits.searchIgnoring(probe)(symbols*) match {
            case success: ImplicitSearchSuccess =>
              val providers = markers.map(_.companionModule.moduleClass)
              val usedProviders = new TreeAccumulator[Set[Symbol]] {
                private def provider(tree: Tree): Option[Symbol] =
                  scala.util
                    .Try(tree.symbol)
                    .toOption
                    .filterNot(_.isNoSymbol)
                    .map(_.maybeOwner)
                    .filter(providers.contains)
                def foldTree(found: Set[Symbol], tree: Tree)(owner: Symbol): Set[Symbol] = tree match {
                  case Inlined(call, bindings, expansion) =>
                    val fromCall = call.flatMap(provider).toSet
                    foldTrees(foldTree(found ++ fromCall, expansion)(owner), bindings)(owner)
                  case _ => foldOverTree(found ++ provider(tree), tree)(owner)
                }
              }.foldTree(Set.empty, success.tree)(Symbol.spliceOwner)
              val used = missing.indices.toList.filter(index => usedProviders.contains(providers(index)))
              if used.isEmpty then None
              else Some((used, givensAndSummon[TC, A](used.map(missingReprs(_)), typeClassCtor)))
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }

  /** `{ given TC[B1] = dependency1; ...; scala.compiletime.summonInline[TC[A]] }` */
  private def givensAndSummon[TC[_], A](
      dependencyTypes: List[Any],
      typeClassCtor: Any
  )(implicit InstT: Type[TC[A]]): List[Expr[Any]] => Expr[TC[A]] = (dependencies: List[Expr[Any]]) => {
    given q: scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
    import q.reflect.*
    val owner = Symbol.spliceOwner
    val ctor = typeClassCtor.asInstanceOf[TypeRepr]
    val givens = dependencyTypes.zip(dependencies).zipWithIndex.map { case ((dependencyType, dependency), index) =>
      val symbol = Symbol.newVal(
        owner,
        s"kindlingsDependency$index",
        ctor.appliedTo(dependencyType.asInstanceOf[TypeRepr]),
        Flags.Given,
        Symbol.noSymbol
      )
      ValDef(symbol, Some(dependency.asInstanceOf[scala.quoted.Expr[Any]].asTerm.changeOwner(symbol)))
    }
    val summoned = InstT.asInstanceOf[scala.quoted.Type[?]] match {
      case '[t] => '{ scala.compiletime.summonInline[t] }.asTerm
    }
    Block(givens, summoned).asExpr.asInstanceOf[Expr[TC[A]]]
  }
}
