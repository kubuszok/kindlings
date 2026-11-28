package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Scala 2 part of [[UserInstances]]: implicit searches that Hearth's `summonExprIgnoring` does not offer.
  *
  *   - The reason of a failed search: the Scala 2 compiler does not tell why a candidate failed (a silent search
  *     discards the errors, including a crashed macro expansion of a candidate), so only "not found" is reported.
  *   - A search with some instances given: `{ implicit val d1: D1 = ...; implicitly[Inst] }` is type-checked at the
  *     expansion site.
  */
trait UserInstancesPlatform { this: MacroCommons =>

  /** The instance, or why there is none and whether a candidate's macro expansion crashed (never known on Scala 2). */
  protected def summonIgnoringExplained[Inst: Type](
      ignored: Seq[UntypedMethod]
  ): Either[(String, Boolean), Expr[Inst]] =
    Type[Inst].summonExprIgnoring(ignored*).toEither.left.map(reason => (reason, false))

  /** Whether `tpe` is a proper type (not a type constructor), so that it can have a type class instance. */
  protected def isProperType(tpe: ??): Boolean = {
    import tpe.Underlying as T
    !UntypedType.fromTyped[T].asInstanceOf[scala.reflect.api.Universe#Type].takesTypeArgs
  }

  /** Searches `TC[A]` as if `TC[B]` existed for the type arguments `missing` of `A` (as local implicit values). On
    * success: the indices (in `missing`) of the instances the found instance uses (at least one) and the builder of the
    * instance from the expressions of those instances (in the same order).
    */
  protected def summonIgnoringWithGivens[TC[_], A: Type](
      typeClass: Type.Ctor1[TC],
      missing: List[??],
      ignored: Seq[UntypedMethod]
  ): Option[(List[Int], List[Expr[Any]] => Expr[TC[A]])] = this match {
    case scala2: hearth.MacroCommonsScala2 if missing.nonEmpty =>
      type Inst = TC[A]
      implicit val InstT: Type[Inst] = typeClass[A]
      val dependencies: List[??] = missing.map { argument =>
        import argument.Underlying as B
        typeClass[B].as_??
      }
      val c: scala2.c.type = scala2.c
      import c.universe.{
        Apply,
        ApplyTag,
        Block,
        BlockTag,
        Ident,
        IdentTag,
        Quasiquote,
        TermName,
        Tree,
        TypeApply,
        TypeApplyTag,
        Symbol as CSymbol
      }
      val ignoredSymbols: Set[CSymbol] = ignored.map(_.asInstanceOf[scala2.UntypedMethod].symbol: CSymbol).toSet
      val dependencyTypes: Vector[c.Type] = dependencies.toVector.map { dependency =>
        import dependency.Underlying as D
        UntypedType.fromTyped[D].asInstanceOf[c.Type]
      }
      val instType: c.Type = UntypedType.fromTyped[Inst].asInstanceOf[c.Type]

      // `{ implicit val d1: D1 = value1; ...; implicitly[Inst] }`
      def block(indices: List[Int], values: List[Tree]): (List[TermName], Tree) = {
        val names = indices.map(_ => TermName(c.freshName("kindlingsDependency")))
        val vals = indices.zip(names).zip(values).map { case ((index, name), value) =>
          q"implicit val $name: ${dependencyTypes(index)} = $value"
        }
        (names, q"{ ..$vals; _root_.scala.Predef.implicitly[$instType] }")
      }

      def headSymbol(tree: Tree): CSymbol = tree match {
        case Apply(function, _)     => headSymbol(function)
        case TypeApply(function, _) => headSymbol(function)
        case _                      => tree.symbol
      }

      // the indices of the dependencies the instance found with them given uses
      def probe(indices: List[Int]): Option[List[Int]] = {
        val placeholders = indices.map(index => q"null.asInstanceOf[${dependencyTypes(index)}]")
        c.typecheck(block(indices, placeholders)._2, silent = true) match {
          case Block(stats, Apply(_, List(instance))) if !ignoredSymbols.contains(headSymbol(instance)) =>
            val valSymbols = stats.map(_.symbol)
            val referenced = instance.collect { case ident: Ident => ident.symbol }.toSet
            Some(indices.zip(valSymbols).collect { case (index, symbol) if referenced.contains(symbol) => index })
              .filter(_.nonEmpty)
          case _ => None
        }
      }

      // The instance is resolved again where the expression ends up: the same search, at the same expansion site.
      def builder(indices: List[Int]): List[Expr[Any]] => Expr[Inst] = (arguments: List[Expr[Any]]) =>
        c.Expr[Inst](block(indices, arguments.map(_.asInstanceOf[c.Expr[Any]].tree))._2)(
          Type[Inst].asInstanceOf[c.WeakTypeTag[Inst]]
        ).asInstanceOf[Expr[Inst]]

      val all = dependencies.indices.toList
      probe(all).flatMap { used =>
        if (used.size == all.size) Some((used, builder(used)))
        else probe(used).filter(_ == used).map(_ => (used, builder(used)))
      }
    case _ => None
  }
}
