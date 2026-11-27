package hearth.kindlings.optics
package internal.compiletime

import hearth.MacroCommons
import hearth.std.StdExtensions
import hearth.fp.Id
import hearth.fp.data.NonEmptyVector
import hearth.fp.instances.*
import hearth.fp.syntax.*
// The Hearth std SPI mixes in an `IsEither` extractor that shadows the optics `IsEither` evidence trait inside this
// trait's scope; alias the optics one so `deriveIsEither` can name both (the SPI `IsEither` as the `case` extractor,
// `OpticsEither` for the evidence type/`witness`).
import _root_.hearth.kindlings.optics.IsEither as OpticsEither

/** Shared, macro-platform-agnostic implementation of the `modify` optics macro.
  *
  * Mixed into the per-platform macro bundles (`MacroCommonsScala2`/`MacroCommonsScala3`), so every method here can use
  * the full cross-platform Hearth API (`Type`, `Expr`, `CaseClass`, `DestructuredExpr`, ...).
  *
  * Field access (`_.a.b.c`) emits a nested copy-with-modification. Traversal steps go through Hearth's std SPI: `.each`
  * / `.eachWhere(cond)` matches the container against `IsCollection` (rebuild via `factory`/`build`), `IsMap` (map
  * values) or `IsOption`; `.eachLeft`/`.eachRight` matches `IsEither`. This makes `.each` work over EVERY container a
  * provider supports — the built-ins plus anything registered on the classpath (cats `NonEmpty*` via
  * `kindlings-cats-integration`, java collections, ...). The path lambda is parsed with
  * [[hearth.typed.Exprs.DestructuredExpr.parse]] into an ordered list of [[PathStep]]s.
  *
  * The indexed steps `.at`/`.index`/`.atOrElse` are just iterate-and-rebuild at one position/key, so they go through
  * the SAME SPI: `IsCollection` (positional, with an index counter), `IsMap` (by key), `IsOption` (the single value) —
  * no positional-update SPI or runtime functor needed. The `.when[Subtype]` prism is a non-exhaustive 2-case
  * `value match { case s: Subtype => f(s); case other => other }` built via [[MatchCase.typeMatch]]. `modifyAll` and
  * lens composition (`modifyLens`/`andThenModify`) build on the same steps.
  */
private[optics] trait ModifyMacrosImpl extends hearth.kindlings.macros.compiletime.GenerationLogging {
  this: MacroCommons & StdExtensions =>

  protected val generationLoggingNamespace: String = "optics"
  protected def generationMarkerImported: Boolean = {
    implicit val tpe: Type[LogGeneration] = Type.of[LogGeneration]
    Expr.summonImplicit[LogGeneration].isDefined
  }

  // optics consumes Hearth's collection/map/option/either SPI (`IsCollection`/`IsMap`/`IsOption`/`IsEither`) to traverse
  // `.each`/`.eachLeft`/... over ANY supported container — built-ins plus anything a provider jar on the classpath
  // registers (e.g. cats `NonEmptyList` via `kindlings-cats-integration`). The providers are discovered by
  // `Environment.loadStandardExtensions()`, which must run before the first `Is*` match. `modify` is a direct-style
  // macro (not MIO), so we use a local once-guard rather than `derivation-commons`' MIO-based `ensureStandardExtensionsLoaded`.
  private var standardExtensionsLoaded: Boolean = false
  private def ensureStdExtensionsLoaded(): Unit =
    if (!standardExtensionsLoaded) {
      val _ = Environment.loadStandardExtensions()
      standardExtensionsLoaded = true
    }

  /** A single step of a parsed `modify` path. */
  sealed private trait PathStep
  private object PathStep {

    /** A plain field access `_.field`. */
    final case class Field(name: String) extends PathStep

    /** A `.each` / `.eachWhere(cond)` over a container. `predicate`, when present, is the `cond: elem => Boolean` of
      * `.eachWhere`. `container`/`elem`/`isMap` are parse-time hints; the build step re-derives everything by matching
      * `Type[S]` against `IsMap`/`IsCollection`/`IsOption`.
      */
    final case class Each(container: ??, elem: ??, predicate: Option[Expr_??], isMap: Boolean) extends PathStep

    /** What kind of indexed access this is. */
    sealed trait AtKind
    object AtKind {
      case object At extends AtKind // throws if absent
      case object Index extends AtKind // no-op if absent
      case object AtOrElse extends AtKind // inserts `default` if absent
    }

    /** What sort of container an [[At]] step traverses (selects the runtime helper + functor to summon). */
    sealed trait AtShape
    object AtShape {
      case object Seq extends AtShape // QuicklensIndexedFunctor, Int-keyed
      case object Map extends AtShape // QuicklensMapAtFunctor, key-keyed
      case object Single extends AtShape // QuicklensSingleAtFunctor, Option-like (no index)
    }

    /** A `.at(i)` / `.index(i)` / `.atOrElse(i, default)` over an indexed container, or the no-index `.at` / `.index` /
      * `.atOrElse(default)` over an `Option`-like container.
      *
      * `idx` is the index/key argument (absent for the `Single` shape). `default` is present only for `AtOrElse`.
      * `elem` is the focused element type derived from the container.
      */
    final case class At(
        container: ??,
        elem: ??,
        kind: AtKind,
        shape: AtShape,
        idx: Option[Expr_??],
        default: Option[Expr_??]
    ) extends PathStep

    /** A `.eachLeft` / `.eachRight` over an `Either[L, R]`. `isLeft` selects the branch; `branch` is the focused branch
      * type (`L` for `.eachLeft`, `R` for `.eachRight`).
      */
    final case class EachEither(container: ??, branch: ??, isLeft: Boolean) extends PathStep

    /** A `.when[Subtype]` prism: narrows the focus to `subtype`, leaving other subtypes unchanged. */
    final case class When(subtype: ??) extends PathStep
  }

  /** `obj.modify(_.a.b.c)` / `obj.modify(_.xs.each.field)` → `PathModify[S, A]`. Parses the path lambda into a list of
    * [[PathStep]]s, then emits `PathModify(obj, (s, mod) => <rebuilt s>)`.
    *
    * The focused leaf type `A` is the path lambda's result type, which the call site infers precisely: the `.each`
    * markers pin the element type via [[hearth.kindlings.optics.IsElementOf]] (invariantly, so no covariant widening),
    * so even leaf `.each`/`.eachWhere` paths produce a precise `A`. The macro additionally re-derives the element type
    * from the parsed container, so the codegen never depends on `A`'s inference being perfect.
    */
  /** A compact human-readable rendering of a parsed path, for the generation log (e.g. `.a.xs.each.b`). */
  private def describeSteps(steps: List[PathStep]): String =
    if (steps.isEmpty) "(identity)" else steps.map(describeStep).mkString
  private def describeStep(step: PathStep): String = step match {
    case PathStep.Field(name)                  => s".$name"
    case PathStep.Each(_, _, predicate, isMap) =>
      val base = if (isMap) ".eachValue" else ".each"
      if (predicate.isDefined) s"$base(where)" else base
    case PathStep.At(_, _, kind, shape, _, _) =>
      val k = kind match {
        case PathStep.AtKind.At       => "at"
        case PathStep.AtKind.Index    => "index"
        case PathStep.AtKind.AtOrElse => "atOrElse"
      }
      s".$k<$shape>"
    case PathStep.EachEither(_, _, isLeft) => if (isLeft) ".eachLeft" else ".eachRight"
    case PathStep.When(subtype)            =>
      import subtype.Underlying as T
      s".when[${Type.prettyPrint[T]}]"
  }

  def modify[S: Type, A: Type, P: Type](obj: Expr[S], path: Expr[P]): Expr[PathModify[S, A]] = {
    ensureStdExtensionsLoaded()
    val trace = new Trace
    val steps: List[PathStep] = parsePath[P](path)
    trace.step(s"modify[${Type.prettyPrint[S]} => ${Type.prettyPrint[A]}]")
    trace.step(s"parsed path: ${describeSteps(steps)}")
    // Build the copy-with-modification function `(s, mod) => <rebuilt s>`, where the leaf transformation applied at the
    // focus is exactly `mod(<leaf>)`. The lambda parameters `s`/`mod` are quote-bound; their `Expr` handles are
    // recovered for the macro-side `buildModify` via `Expr.quote(s)` / `Expr.quote(mod)` (the standard Hearth cross-quote
    // idiom — see cats-derivation `FunctorMacrosImpl`).
    implicit val resultType: Type[PathModify[S, A]] = Type.of[PathModify[S, A]]
    val result = withHoistedPredicates[PathModify[S, A]](List(steps)) { hoisted =>
      val steps = hoisted.head
      val doModify: Expr[(S, A => A) => S] =
        Expr.quote { (s: S, mod: A => A) =>
          Expr.splice {
            buildModify[S](Expr.quote(s), steps) { leaf =>
              applyLeaf[S, A](Expr.quote(mod), leaf)
            }
          }
        }

      Expr.quote(PathModify[S, A](Expr.splice(obj), Expr.splice(doModify)))
    }
    emitGenerationLog("modify", trace, result.prettyPrint)
    result
  }

  /** Materialize the `.each` evidence `IsElementOf.Aux[C, Elem]` by checking the std SPI: `Elem` is the value type of
    * an `IsMap`, the item of an `IsCollection`, or the inner of an `IsOption`. Used by a whitebox (Scala 2) /
    * transparent inline (Scala 3) given so `_.xs.each` type-checks ONLY when `xs`'s type actually has a provider, with
    * the precise element type taken from that provider — and a new provider jar on the classpath enables `.each`
    * automatically. The declared return type is widened to `Expr[IsElementOf[C]]`, but the emitted tree is the precise
    * `IsElementOf.witness[C, Elem]` (typed `Aux[C, Elem]`), which whitebox/transparent expose to the call site.
    */
  def deriveIsElementOf[C: Type]: Expr[IsElementOf[C]] = {
    ensureStdExtensionsLoaded()
    Type[C] match {
      case IsMap(m) =>
        import m.Underlying as Pair
        isElementOfWitnessForMap[C, Pair](m.value)
      case IsCollection(coll) =>
        import coll.Underlying as Elem
        isElementOfWitness[C, Elem]
      case IsOption(opt) =>
        import opt.Underlying as Elem
        isElementOfWitness[C, Elem]
      case _ =>
        abort(
          s"`.each` is not supported on [${Type.prettyPrint[C]}] — it must be a collection, map or Option, i.e. a type " +
            "with an IsCollection/IsMap/IsOption provider (the built-ins, or e.g. cats `NonEmpty*` once " +
            "`kindlings-cats-integration` is on the classpath)."
        )
    }
  }

  // `.each` over a Map traverses to the value type, which lives on the `IsMapOf` (a helper method so the value `Type`
  // is a regular type parameter rather than a path-dependent member leaking into the quote).
  private def isElementOfWitnessForMap[C: Type, Pair: Type](isMap: IsMapOf[C, Pair]): Expr[IsElementOf[C]] = {
    import isMap.Value
    isElementOfWitness[C, Value]
  }

  private def isElementOfWitness[C: Type, Elem: Type]: Expr[IsElementOf[C]] =
    Expr.quote(IsElementOf.witness[C, Elem]).asInstanceOf[Expr[IsElementOf[C]]]

  /** Materialize the `.eachLeft`/`.eachRight` evidence `IsEither.Aux[C, L, R]` from the `IsEither` SPI (the branch
    * types). Whitebox (Scala 2) / transparent inline (Scala 3), like [[deriveIsElementOf]].
    */
  def deriveIsEither[C: Type]: Expr[OpticsEither[C]] = {
    ensureStdExtensionsLoaded()
    Type[C] match {
      case IsEither(e) =>
        import e.{LeftValue, RightValue}
        // NB: reference the optics `IsEither` by its FULL path inside the quote — the local `OpticsEither` alias is only
        // an import rename, so emitting it into the generated tree would produce an unresolvable `OpticsEither` symbol.
        Expr
          .quote(_root_.hearth.kindlings.optics.IsEither.witness[C, LeftValue, RightValue])
          .asInstanceOf[Expr[OpticsEither[C]]]
      case _ =>
        abort(
          s"`modify`: `.eachLeft`/`.eachRight` is only supported on `Either`, but [${Type.prettyPrint[C]}] is not one."
        )
    }
  }

  /** Materialize the no-index `.at`/`.index`/`.atOrElse` (Option) evidence `IsSingleElementOf.Aux[C, Elem]` from the
    * `IsOption` SPI.
    */
  def deriveIsSingleElementOf[C: Type]: Expr[IsSingleElementOf[C]] = {
    ensureStdExtensionsLoaded()
    Type[C] match {
      case IsOption(o) =>
        import o.Underlying as Inner
        Expr.quote(IsSingleElementOf.witness[C, Inner]).asInstanceOf[Expr[IsSingleElementOf[C]]]
      case _ =>
        abort(
          s"`modify`: the no-index `.at`/`.index`/`.atOrElse` is only supported on `Option`, but [${Type
              .prettyPrint[C]}] is not one."
        )
    }
  }

  /** Materialize the indexed `.at`/`.index`/`.atOrElse` evidence `IsIndexedElementOf.Aux[C, Idx, Elem]` from the SPI: a
    * `Map` is keyed by its key type (`IsMap`), a `Seq` or an `Array` is positional (`Int`, element from
    * `IsCollection`). Restricted to those so positional `.at` is not offered on unordered collections (e.g. `Set`).
    */
  def deriveIsIndexedElementOf[C: Type]: Expr[IsIndexedElementOf[C]] = {
    ensureStdExtensionsLoaded()
    Type[C] match {
      case IsMap(m) =>
        import m.Underlying as Pair
        isIndexedWitnessForMap[C, Pair](m.value)
      case IsCollection(coll) if isSeqOrArray[C] =>
        import coll.Underlying as Item
        Expr.quote(IsIndexedElementOf.witness[C, Int, Item]).asInstanceOf[Expr[IsIndexedElementOf[C]]]
      case _ =>
        abort(
          s"`modify`: `.at`/`.index`/`.atOrElse` works on a `Seq` (by `Int`) or a `Map` (by key), but [${Type
              .prettyPrint[C]}] is neither."
        )
    }
  }

  // For a Map, the index is the key type and the element is the value type (both off the `IsMapOf`).
  private def isIndexedWitnessForMap[C: Type, Pair: Type](isMap: IsMapOf[C, Pair]): Expr[IsIndexedElementOf[C]] = {
    import isMap.{Key, Value}
    Expr.quote(IsIndexedElementOf.witness[C, Key, Value]).asInstanceOf[Expr[IsIndexedElementOf[C]]]
  }

  // As quicklens: any `Seq` (immutable or not) and `Array`; not the unordered collections (e.g. `Set`).
  private def isSeqOrArray[C: Type]: Boolean = {
    implicit val seqAny: Type[scala.collection.Seq[Any]] = Type.of[scala.collection.Seq[Any]]
    Type.isSubtypeOf[C, scala.collection.Seq[Any]] || Type.Ctor1.of[Array].unapply(Type[C]).isDefined
  }

  /** `obj.modifyAll(_.a, _.b.each, _.c)` → `PathModify[S, A]`. Parses each path into its own list of [[PathStep]]s,
    * then merges them into a single copy-with-modification function that applies `mod` to EVERY focused leaf.
    * Overlapping field prefixes are shared — `modifyAll(_.a.b, _.a.c)` copies `a` exactly once (quicklens semantics).
    * All paths must focus the same leaf type `A` (the call site pins this).
    */
  def modifyAll[S: Type, A: Type, P: Type](
      obj: Expr[S],
      path: Expr[P],
      morePaths: VarArgs[P]
  ): Expr[PathModify[S, A]] = {
    ensureStdExtensionsLoaded()
    val paths = path :: explicitPaths("modifyAll", morePaths)
    val trace = new Trace
    val pathSteps: List[List[PathStep]] = paths.map(parsePath[P])
    trace.step(s"modifyAll[${Type.prettyPrint[S]} => ${Type.prettyPrint[A]}] over ${pathSteps.size} path(s)")
    pathSteps.foreach(steps => trace.step(s"  parsed path: ${describeSteps(steps)}"))
    implicit val resultType: Type[PathModify[S, A]] = Type.of[PathModify[S, A]]
    val result = withHoistedPredicates[PathModify[S, A]](pathSteps) { pathSteps =>
      val doModify: Expr[(S, A => A) => S] =
        Expr.quote { (s: S, mod: A => A) =>
          Expr.splice {
            buildModifyMulti[S](Expr.quote(s), pathSteps) { leaf =>
              applyLeaf[S, A](Expr.quote(mod), leaf)
            }
          }
        }
      Expr.quote(PathModify[S, A](Expr.splice(obj), Expr.splice(doModify)))
    }
    emitGenerationLog("modifyAll", trace, result.prettyPrint)
    result
  }

  /** `modifyLens[T](_.a.b)` → `PathLazyModify[T, U]`: a reusable lens NOT bound to an object. Same machinery as
    * [[modify]], but the rebuilt value is a function of the lambda parameter `t` rather than a captured object.
    */
  def modifyLens[T: Type, Foc: Type, P: Type](path: Expr[P]): Expr[PathLazyModify[T, Foc]] = {
    ensureStdExtensionsLoaded()
    val trace = new Trace
    val steps: List[PathStep] = parsePath[P](path)
    trace.step(s"modifyLens[${Type.prettyPrint[T]} => ${Type.prettyPrint[Foc]}]")
    trace.step(s"parsed path: ${describeSteps(steps)}")
    implicit val resultType: Type[PathLazyModify[T, Foc]] = Type.of[PathLazyModify[T, Foc]]
    val result = withHoistedPredicates[PathLazyModify[T, Foc]](List(steps)) { hoisted =>
      val steps = hoisted.head
      val doModify: Expr[(T, Foc => Foc) => T] =
        Expr.quote { (t: T, mod: Foc => Foc) =>
          Expr.splice {
            buildModify[T](Expr.quote(t), steps) { leaf =>
              applyLeaf[T, Foc](Expr.quote(mod), leaf)
            }
          }
        }
      Expr.quote(PathLazyModify[T, Foc](Expr.splice(doModify)))
    }
    emitGenerationLog("modifyLens", trace, result.prettyPrint)
    result
  }

  /** `modifyAllLens[T](_.a, _.b)` → `PathLazyModify[T, Foc]`: the multi-path reusable lens form. Same shared-prefix
    * merge as [[modifyAll]], but unbound from any object.
    */
  def modifyAllLens[T: Type, Foc: Type, P: Type](path: Expr[P], morePaths: VarArgs[P]): Expr[PathLazyModify[T, Foc]] = {
    ensureStdExtensionsLoaded()
    val paths = path :: explicitPaths("modifyAllLens", morePaths)
    val trace = new Trace
    val pathSteps: List[List[PathStep]] = paths.map(parsePath[P])
    trace.step(s"modifyAllLens[${Type.prettyPrint[T]} => ${Type.prettyPrint[Foc]}] over ${pathSteps.size} path(s)")
    pathSteps.foreach(steps => trace.step(s"  parsed path: ${describeSteps(steps)}"))
    implicit val resultType: Type[PathLazyModify[T, Foc]] = Type.of[PathLazyModify[T, Foc]]
    val result = withHoistedPredicates[PathLazyModify[T, Foc]](pathSteps) { pathSteps =>
      val doModify: Expr[(T, Foc => Foc) => T] =
        Expr.quote { (t: T, mod: Foc => Foc) =>
          Expr.splice {
            buildModifyMulti[T](Expr.quote(t), pathSteps) { leaf =>
              applyLeaf[T, Foc](Expr.quote(mod), leaf)
            }
          }
        }
      Expr.quote(PathLazyModify[T, Foc](Expr.splice(doModify)))
    }
    emitGenerationLog("modifyAllLens", trace, result.prettyPrint)
    result
  }

  /** Bind every `.eachWhere` predicate to a `val` in front of the generated `PathModify`/`PathLazyModify` and make the
    * steps refer to it, so the predicate — a lambda written inside the path lambda — is evaluated once and stays at the
    * call site. Moved into the generated modification function instead, a predicate capturing a local `val` crashes the
    * Scala 2 compiler (its closure is owned by the call site, but would be nested in the generated function).
    */
  private def withHoistedPredicates[R: Type](paths: List[List[PathStep]])(
      use: List[List[PathStep]] => Expr[R]
  ): Expr[R] = {
    def hoistPath(todo: List[PathStep], done: List[PathStep])(continue: List[PathStep] => Expr[R]): Expr[R] =
      todo match {
        case Nil                                                      => continue(done.reverse)
        case (each @ PathStep.Each(_, _, Some(predicate), _)) :: rest =>
          import predicate.Underlying as P
          ValDefs.createVal[P](predicate.value, FreshName.FromPrefix("predicate")).use { ref =>
            hoistPath(rest, each.copy(predicate = Some(ref.as_??)) :: done)(continue)
          }
        case step :: rest => hoistPath(rest, step :: done)(continue)
      }
    def hoistPaths(todo: List[List[PathStep]], done: List[List[PathStep]]): Expr[R] = todo match {
      case Nil          => use(done.reverse)
      case path :: rest => hoistPath(path, Nil)(hoisted => hoistPaths(rest, hoisted :: done))
    }
    hoistPaths(paths, Nil)
  }

  /** The extra paths of `modifyAll`/`modifyAllLens`, which must be written out at the call site: a splatted,
    * non-literal sequence (`paths*` on Scala 3, `paths: _*` on Scala 2) only has its elements at runtime, so it is
    * rejected rather than silently dropped.
    */
  private def explicitPaths[P](method: String, paths: VarArgs[P]): List[Expr[P]] =
    VarArgs
      .toIterableOption(paths)
      .getOrElse(
        abort(
          s"`$method` expects an explicit list of paths, got a splatted sequence whose elements are only known at runtime"
        )
      )
      .toList

  /** The paths of one `modify`/`modifyAll`, merged into a tree as quicklens does (its `PathTree`): each node lists its
    * steps in the order they first occur, and every step has a sequence of sub-trees, applied one after another.
    *
    * A path is merged into the LAST sub-tree of an equivalent step, unless that sub-tree is a leaf (the modification is
    * applied there): then it starts a new sub-tree, applied after it. So `modifyAll(_.a.x, _.a.y)` rebuilds `a` once
    * with both fields, `modifyAll(_.xs.each.a, _.xs.each.b)` traverses `xs` once, while
    * `modifyAll(_.a.each, _.a, _.a.each)` applies the three modifications in that order.
    */
  sealed private trait PathTree
  private object PathTree {
    case object Empty extends PathTree
    final case class Node(children: Vector[(PathStep, Vector[PathTree])]) extends PathTree

    def of(steps: List[PathStep]): PathTree = steps match {
      case Nil          => Empty
      case head :: tail => Node(Vector(head -> Vector(of(tail))))
    }

    def of(paths: Iterable[List[PathStep]]): PathTree = paths.foldLeft(Empty: PathTree)(add)

    def add(tree: PathTree, steps: List[PathStep]): PathTree = (tree, steps) match {
      case (Empty, _)                     => of(steps)
      case (node: Node, Nil)              => node
      case (Node(children), step :: tail) =>
        if (!children.exists(child => equivalent(child._1, step))) Node(children :+ (step -> Vector(of(tail))))
        else
          Node(children.map {
            case (existing, trees) if equivalent(existing, step) =>
              existing -> (trees.last match {
                case Empty             => trees :+ of(tail)
                case _ if tail.isEmpty => trees :+ Empty
                case last              => trees.init :+ add(last, tail)
              })
            case other => other
          })
    }

    /** Steps that can share one rebuild/traversal. The steps carrying expressions (`.eachWhere(p)`, `.at(i)`, ...) are
      * not compared, so each of them is applied on its own (in order).
      */
    private def equivalent(a: PathStep, b: PathStep): Boolean = (a, b) match {
      case (PathStep.Field(n1), PathStep.Field(n2))                       => n1 == n2
      case (PathStep.Each(_, _, None, _), PathStep.Each(_, _, None, _))   => true
      case (PathStep.EachEither(_, _, l1), PathStep.EachEither(_, _, l2)) => l1 == l2
      case (PathStep.When(t1), PathStep.When(t2))                         =>
        import t1.Underlying as T1
        import t2.Underlying as T2
        Type[T1] =:= Type[T2]
      case _ => false
    }
  }

  /** Rebuild `sExpr: S` applying `transformLeaf` at the focus of EVERY path in `paths` (see [[PathTree]]). */
  private def buildModifyMulti[S: Type](sExpr: Expr[S], paths: List[List[PathStep]])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = applyTree[S](sExpr, PathTree.of(paths))(transformLeaf)

  /** Rebuild a value `sExpr: S` with the focus reached by `steps` transformed by `transformLeaf`. */
  private def buildModify[S: Type](sExpr: Expr[S], steps: List[PathStep])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = applyTree[S](sExpr, PathTree.of(steps))(transformLeaf)

  /** Rebuild `sExpr: S` according to `tree`: at a leaf `sExpr` is the focus, so `transformLeaf` is applied; otherwise
    * the node's steps are applied in order — consecutive fields with ONE rebuild of `S`, every traversal step (`.each`,
    * `.at`, `.eachLeft`, `.when`, ...) once per sub-tree.
    */
  private def applyTree[S: Type](sExpr: Expr[S], tree: PathTree)(transformLeaf: Expr[S] => Expr[S]): Expr[S] =
    tree match {
      case PathTree.Empty          => transformLeaf(sExpr)
      case PathTree.Node(children) => applyChildren[S](sExpr, children.toList, first = true)(transformLeaf)
    }

  private def applyChildren[S: Type](
      sExpr: Expr[S],
      children: List[(PathStep, Vector[PathTree])],
      first: Boolean
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = children match {
    case Nil                         => sExpr
    case (_: PathStep.Field, _) :: _ =>
      val (fieldChildren, others) = children.span(_._1.isInstanceOf[PathStep.Field])
      val fields = fieldChildren.collect { case (PathStep.Field(name), trees) => name -> trees }
      // the fields are read from the value several times: an intermediate result is bound to a `val` first
      val rebuilt = bindUnless[S](first, sExpr)(buildFields[S](_, fields)(transformLeaf))
      applyChildren[S](rebuilt, others, first = false)(transformLeaf)
    case (step, trees) :: others =>
      val rebuilt = trees.foldLeft(sExpr) { (acc, tree) =>
        step match {
          case each: PathStep.Each    => buildEachStep[S](acc, each, tree)(transformLeaf)
          case at: PathStep.At        => buildAtStep[S](acc, at, tree)(transformLeaf)
          case e: PathStep.EachEither => buildEitherStep[S](acc, e, tree)(transformLeaf)
          case w: PathStep.When       => buildWhenStep[S](acc, w, tree)(transformLeaf)
          case PathStep.Field(name)   => abort(s"`modify`: unexpected field step `$name`")
        }
      }
      applyChildren[S](rebuilt, others, first = false)(transformLeaf)
  }

  /** `use(e)`, with `e` bound to a local `val` unless it is known to be a plain reference (`asIs`). */
  private def bindUnless[S: Type](asIs: Boolean, e: Expr[S])(use: Expr[S] => Expr[S]): Expr[S] =
    if (asIs) use(e)
    else ValDefs.createVal[S](e, FreshName.FromPrefix("intermediate")).use(use)

  /** Apply the sub-trees of one field, one after another, to its value. */
  private def applyFieldTrees[S: Type](focused: Expr_??, trees: Vector[PathTree])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr_?? = {
    import focused.Underlying as F
    applyFieldTreesTyped[S, F](focused.value, trees)(transformLeaf).as_??
  }
  private def applyFieldTreesTyped[S: Type, F: Type](focused: Expr[F], trees: Vector[PathTree])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[F] =
    trees.tail.foldLeft(descend[S, F](focused, trees.head)(transformLeaf)) { (acc, tree) =>
      bindUnless[F](asIs = false, acc)(descend[S, F](_, tree)(transformLeaf))
    }

  /** Apply `rest` to a part `e` of `S` (a field, an element of a traversed container); the leaf transform (over `S`) is
    * re-typed at the part's type, which is sound as it only fires where the remaining steps bottom out at the focus.
    */
  private def descend[S: Type, E: Type](e: Expr[E], rest: PathTree)(transformLeaf: Expr[S] => Expr[S]): Expr[E] =
    applyTree[E](e, rest)(leaf => transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[E]])

  /** Descend the `fields` (each with its sub-trees) into `sExpr: S` and rebuild it ONCE:
    *
    *   - a case class is rebuilt with its constructor (what the generated `copy` does);
    *   - a sealed hierarchy / `enum` is matched over its subtypes, each of which must have the fields (quicklens-style
    *     common-field modification, no `.when[Sub]` per case needed);
    *   - anything else goes through its `copy` method, see [[buildFieldsViaCopy]].
    */
  private def buildFields[S: Type](sExpr: Expr[S], fields: List[(String, Vector[PathTree])])(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    CaseClass.parse[S] match {
      case ClassViewResult.Compatible(caseClass) =>
        caseFieldValuesOf[S](caseClass, sExpr).filter(_ => rebuildsWithConstructor(caseClass)) match {
          case Some(fieldValues) =>
            val updated = fields.foldLeft(fieldValues) { case (acc, (field, trees)) =>
              val focused = acc.getOrElse(
                field,
                abort(s"`modify`: no accessible field `$field` on [${Type.prettyPrint[S]}]")
              )
              acc.updated(field, applyFieldTrees[S](focused, trees)(transformLeaf))
            }
            reconstruct[S](caseClass, updated)
          case None =>
            buildFieldsViaCopy[S](sExpr, fields, "its fields cannot be rebuilt positionally")(transformLeaf)
        }
      case ClassViewResult.Incompatible(ccReason) =>
        Enum.parse[S] match {
          case ClassViewResult.Compatible(enm) =>
            enm
              .matchOn[Id, S](sExpr) { childExpr =>
                import childExpr.Underlying as Child
                val rebuilt: Expr[Child] =
                  buildFields[Child](childExpr.value, fields)(leaf =>
                    transformLeaf(leaf.asInstanceOf[Expr[S]]).asInstanceOf[Expr[Child]]
                  )
                whenUpcast[Child, S](rebuilt)
              }
              .getOrElse(abort(s"`modify`: [${Type.prettyPrint[S]}] is a sealed hierarchy with no cases"))
          case ClassViewResult.Incompatible(_) => buildFieldsViaCopy[S](sExpr, fields, ccReason)(transformLeaf)
        }
    }

  /** Parse the path lambda `_.a.each.b` into an ordered list of [[PathStep]]s (root → leaf).
    *
    * Walks the [[hearth.typed.Exprs.DestructuredExpr.MethodCall]] chain: a call with only an instance argument is a
    * field access; a call to `each`/`eachWhere` (the marker extensions) is a collection traversal whose receiver is the
    * container expression (its element type is derived from the container — see [[eachStep]]). Bottoms out at the
    * lambda parameter reference.
    *
    * `P` is the path's static type: a plain `S => A` on Scala 2, the context function `OpticsContext ?=> (S => A)` on
    * Scala 3. [[hearth.typed.Exprs.DestructuredExpr.skipContextualWrappers]] peels the contextual layer (the `?=>`
    * lambda, or the block binding a synthetic `given`) down to the real `S => A` lambda — a no-op on Scala 2. The
    * dropped `OpticsContext` may still be referenced from the marker-evidence argument positions in the body, but those
    * are `PathStepEvidence`-typed and [[nonEvidenceArgs]] discards them.
    */
  private def parsePath[P: Type](path: Expr[P]): List[PathStep] = {
    val parsed = DestructuredExpr.skipContextualWrappers(DestructuredExpr.parse[P](path))
    parsed match {
      case lam: DestructuredExpr.Lambda =>
        lam.params match {
          case List(param) => walk(lam.body, param)
          case params      =>
            abort(s"`modify` expects a single-parameter path lambda, got ${params.size} parameters")
        }
      case other =>
        abort(s"`modify` expects a path lambda like `_.a.b.c`, but got: ${other.plainPrint}")
    }
  }

  private def walk(
      expr: DestructuredExpr,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = expr match {
    case ref: DestructuredExpr.Lambda.ParamRef if ref.param eq rootParam =>
      Nil
    // A `.each` / `.eachWhere(cond)` step: a call named `each`/`eachWhere` whose receiver is the container (recovered by
    // `receiverOf`, which strips the Scala 2 `EachOps` wrapper). The predicate (for `.eachWhere`) is recovered by
    // `predicateArg` (the `Function1`-typed value argument, skipping any synthesized `IsElementOf` evidence).
    case mc: DestructuredExpr.MethodCall if mc.method.name == "each" || mc.method.name == "eachWhere" =>
      val instance = receiverOf(mc).getOrElse(
        abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
      )
      val predicate = if (mc.method.name == "eachWhere") predicateArg(mc).map(predExprOf) else None
      eachStep(instance, predicate, rootParam)
    case mc: DestructuredExpr.MethodCall
        if mc.method.name == "at" || mc.method.name == "index" || mc.method.name == "atOrElse" =>
      atStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall if mc.method.name == "eachLeft" || mc.method.name == "eachRight" =>
      eitherStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall if mc.method.name == "when" =>
      whenStep(mc, rootParam)
    case mc: DestructuredExpr.MethodCall =>
      mc.applied match {
        case List(ai: DestructuredExpr.MethodCall.AppliedInstance) =>
          walk(ai.value, rootParam) :+ PathStep.Field(mc.method.name)
        case _ =>
          abort(
            s"`modify` expects a field-access path like `_.a.b.c` (optionally with `.each`), but got an unsupported " +
              s"step: ${mc.plainPrint}"
          )
      }
    case other =>
      abort(
        s"`modify` expects a field-access path like `_.a.b.c` (optionally with `.each`), but got: ${other.plainPrint}"
      )
  }

  /** Build the [[PathStep.Each]] for a recovered container `instance`, deriving the element type from the container's
    * constructor (`F[A]`'s sole argument, or `M[K, V]`'s value argument) rather than from the `.each` marker's result
    * type — the container always carries the precise element type.
    */
  private def eachStep(
      instance: DestructuredExpr,
      predicate: Option[Expr_??],
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val container = instance.tpe
    val isMap = isMapContainer(container)
    val elem = elementTypeOf(container, isMap)
    walk(instance, rootParam) :+ PathStep.Each(container, elem, predicate, isMap)
  }

  /** Parse a `.at(i)` / `.index(i)` / `.atOrElse(i, default)` (Seq/Map) or `.at` / `.index` / `.atOrElse(default)`
    * (Option) step. The container is recovered like `.each`; the shape (Seq/Map/Option) is decided from the container
    * type. Index/default arguments are recovered from the call's value arguments, skipping the synthesized evidence.
    */
  private def atStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
    )
    val container = instance.tpe
    val shape = atShapeOf(container)
    val elem = atElementTypeOf(container, shape)
    val kind = mc.method.name match {
      case "at"       => PathStep.AtKind.At
      case "index"    => PathStep.AtKind.Index
      case "atOrElse" => PathStep.AtKind.AtOrElse
      case other      => abort(s"`modify`: unexpected indexed step `.$other`")
    }
    // Recover the non-evidence value arguments (the synthesized `IsIndexedElementOf`/`IsSingleElementOf` evidence is a
    // leading `using` value arg on Scala 3; on Scala 2 the evidence is on the wrapper class, so it never appears here).
    val realArgs = nonEvidenceArgs(mc)
    val (idx, default) = shape match {
      case PathStep.AtShape.Single =>
        // No index. `atOrElse` carries `(default)`; `at`/`index` carry nothing.
        val default = if (kind == PathStep.AtKind.AtOrElse) realArgs.headOption.map(exprOf) else None
        (None, default)
      case _ =>
        // `at(i)`/`index(i)` carry `(i)`; `atOrElse(i, default)` carries `(i, default)`.
        val idx = realArgs.headOption.map(exprOf)
        val default = if (kind == PathStep.AtKind.AtOrElse) realArgs.lift(1).map(exprOf) else None
        (idx, default)
    }
    walk(instance, rootParam) :+ PathStep.At(container, elem, kind, shape, idx, default)
  }

  /** Parse a `.eachLeft` / `.eachRight` step over an `Either[L, R]`. */
  private def eitherStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the container of `.${mc.method.name}` in ${mc.plainPrint}")
    )
    val container = instance.tpe
    val isLeft = mc.method.name == "eachLeft"
    val branch = eitherBranchTypeOf(container, isLeft)
    walk(instance, rootParam) :+ PathStep.EachEither(container, branch, isLeft)
  }

  /** Parse a `.when[Subtype]` prism step. The narrowed subtype is recovered from the call's type arguments. */
  private def whenStep(
      mc: DestructuredExpr.MethodCall,
      rootParam: DestructuredExpr.Lambda.Param
  ): List[PathStep] = {
    val instance = receiverOf(mc).getOrElse(
      abort(s"`modify`: could not recover the receiver of `.when` in ${mc.plainPrint}")
    )
    // On Scala 3 the extension `when[T <: C]` desugars to two type-argument clauses: the extension's `[C]` (e.g.
    // `[Animal]`) followed by the method's own `[T]` (e.g. `[Dog]`). The narrowed subtype is the LAST type-argument
    // clause (on Scala 2 there is a single clause, so `last` is also correct).
    val subtype = mc.applied
      .collect { case at: DestructuredExpr.MethodCall.AppliedTypes if at.typeArgs.nonEmpty => at.typeArgs.last }
      .lastOption
      .getOrElse(abort(s"`modify`: `.when` requires an explicit subtype argument, e.g. `.when[Sub]`"))
    walk(instance, rootParam) :+ PathStep.When(subtype)
  }

  /** All value arguments of a call that are NOT the synthesized marker evidence (`IsElementOf`/`IsIndexedElementOf`/
    * `IsSingleElementOf`/`IsEither`), in order. On Scala 3 the `using` evidence appears as a leading value argument; on
    * Scala 2 it lives on the wrapper class and never reaches here.
    */
  private def nonEvidenceArgs(mc: DestructuredExpr.MethodCall): List[DestructuredExpr] = {
    val allValueArgs = mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }.flatten
    allValueArgs.filterNot(isEvidenceTyped)
  }

  private lazy val PathStepEvidenceType: Type[PathStepEvidence] = Type.of[PathStepEvidence]
  private lazy val NothingType: Type[Nothing] = Type.of[Nothing]

  private def isEvidenceTyped(d: DestructuredExpr): Boolean = {
    import d.tpe.Underlying as T
    implicit val ev: Type[PathStepEvidence] = PathStepEvidenceType
    implicit val nothing: Type[Nothing] = NothingType
    // `???` (a default of `.atOrElse`) is typed `Nothing`, a subtype of everything: it is not the evidence.
    Type.isSubtypeOf[T, PathStepEvidence] && !Type.isSubtypeOf[T, Nothing]
  }

  private def exprOf(d: DestructuredExpr): Expr_?? = d.toUntypedExpr.as_??

  /** Decide the [[PathStep.AtShape]] of a container targeted by `.at`/`.index`/`.atOrElse` via the std SPI: an `IsMap`
    * is a `Map` (by key), an `IsOption` is `Single`, an `IsCollection` is a positional `Seq`. (`IsMap` is checked first
    * since `Map <: Iterable`.)
    */
  private def atShapeOf(container: ??): PathStep.AtShape = {
    import container.Underlying as C
    Type[C] match {
      case IsMap(_)        => PathStep.AtShape.Map
      case IsOption(_)     => PathStep.AtShape.Single
      case IsCollection(_) => PathStep.AtShape.Seq
      case _               =>
        abort(
          s"`modify`: `.at`/`.index`/`.atOrElse` is not supported on [${Type.prettyPrint[C]}] — it must be a Seq, Map " +
            "or Option."
        )
    }
  }

  /** The focused element type of an `.at`/`.index`/`.atOrElse` over a container of the given [[PathStep.AtShape]]. */
  private def atElementTypeOf(container: ??, shape: PathStep.AtShape): ?? = shape match {
    case PathStep.AtShape.Map => elementTypeOf(container, isMap = true)
    case _                    => elementTypeOf(container, isMap = false)
  }

  /** The focused branch type of `.eachLeft` (`L`) / `.eachRight` (`R`) over an `Either[L, R]` container. */
  private def eitherBranchTypeOf(container: ??, isLeft: Boolean): ?? = {
    import container.Underlying as C
    Type
      .decompose2[C]
      .map { case (_, (left, right)) => if (isLeft) left else right }
      .getOrElse(
        abort(s"`modify`: `.eachLeft`/`.eachRight` expected an `Either[L, R]`, but [${Type.prettyPrint[C]}] is not one")
      )
  }

  /** Recover the container expression an `.each`/`.eachWhere` call is applied to. The `.each` marker is an extension
    * over `EachOps[C, A]` (Scala 2 implicit class) / an `extension (c: C)(using IsElementOf.Aux[C, A])` (Scala 3): on
    * Scala 2 the `each` call's instance is the `EachOps` wrapper whose value argument is the container; on Scala 3 the
    * extension method's instance IS the container directly. We dig through the instance/value-arg chain to the
    * container.
    */
  private def receiverOf(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] = {
    // The instance of the `each` call is the implicit-class wrapper around the container; its first value/instance
    // argument is the container expression.
    val instanceArg: Option[DestructuredExpr] = mc.applied.collectFirst {
      case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value
    }
    instanceArg.flatMap(unwrapEachWrapper)
  }

  // The Scala 2 marker wrappers: implicit CONVERSIONS (`toEachOps`/`toAtOps`/`toSingleAtOps`/`toEitherOps`, applied as
  // `toXOps(container)(evidence).step`, whose first value argument is the container) and the `WhenOps` implicit class.
  private val EachWrapperNames =
    Set(
      "EachOps",
      "toEachOps",
      "AtOps",
      "toAtOps",
      "SingleAtOps",
      "toSingleAtOps",
      "EitherOps",
      "toEitherOps",
      "WhenOps",
      "<init>"
    )

  /** Strip the marker implicit-class wrapper (`EachOps`/`AtOps`/... constructor or its `new …` call), yielding the
    * wrapped container expression. If the instance is already the container (no wrapper node survived destructuring,
    * e.g. on Scala 3 where the markers are `extension` methods), it is returned as-is.
    */
  private def unwrapEachWrapper(expr: DestructuredExpr): Option[DestructuredExpr] = expr match {
    case mc: DestructuredExpr.MethodCall =>
      val name = mc.method.name
      if (mc.method.isConstructor || EachWrapperNames.contains(name)) {
        // The container is the wrapper's value argument (or, defensively, its instance argument).
        mc.applied
          .collectFirst { case av: DestructuredExpr.MethodCall.AppliedValues if av.args.nonEmpty => av.args.head }
          .orElse(mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value })
      } else Some(expr)
    case _ => Some(expr)
  }

  /** The `.eachWhere` predicate among the call's value arguments: the (single) `Function1`-typed argument. This skips
    * the synthesized `IsElementOf` evidence, which on Scala 3 appears as a leading `using` value argument of the
    * `eachWhere` extension method (on Scala 2 the evidence is on the wrapper class, so only the predicate is present).
    */
  private def predicateArg(mc: DestructuredExpr.MethodCall): Option[DestructuredExpr] = {
    val allValueArgs = mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }.flatten
    allValueArgs.find(isFunction1Typed).orElse(allValueArgs.headOption)
  }

  private def isFunction1Typed(d: DestructuredExpr): Boolean = {
    import d.tpe.Underlying as T
    Type.decompose2[T].exists { case (ctor, _) => ctor.sameTypeConstructorAs(Function1Ctor.asUntyped) }
  }

  private lazy val Function1Ctor: Type.Ctor2[Function1] = Type.Ctor2.of[Function1]

  private def predExprOf(d: DestructuredExpr): Expr_?? = d.toUntypedExpr.as_??

  /** Whether `container` is a binary type constructor `M[K, V]` (a map-like) rather than a unary `F[A]`. */
  private def isMapContainer(container: ??): Boolean = {
    import container.Underlying as C
    Type.decompose2[C].isDefined && Type.decompose1[C].isEmpty
  }

  /** The element type traversed by `.each`: the sole type argument of a unary `F[A]`, or the *value* (second) argument
    * of a map-like `M[K, V]`.
    */
  private def elementTypeOf(container: ??, isMap: Boolean): ?? = {
    import container.Underlying as C
    if (isMap)
      Type
        .decompose2[C]
        .map { case (_, (_, value)) => value }
        .getOrElse(abort(s"`modify`: `.each` over a map expected `M[K, V]`, but [${Type.prettyPrint[C]}] is not one"))
    else
      Type
        .decompose1[C]
        .map { case (_, elem) => elem }
        .getOrElse(abort(s"`modify`: `.each` expected a container `F[A]`, but [${Type.prettyPrint[C]}] is not applied"))
  }

  /** Apply the user modification `mod: A => A` to the focused leaf. At the leaf the focused field's type IS the path's
    * leaf type `A`, so the incoming `leaf: Expr[S]` (whose `S` is structurally that field type) is reinterpreted as
    * `Expr[A]` and the result back to `Expr[S]`. The casts are macro-side `Expr` reinterpretations only — the generated
    * tree is a plain `mod(leaf)` call.
    */
  private def applyLeaf[S: Type, A: Type](mod: Expr[A => A], leaf: Expr[S]): Expr[S] =
    Expr.quote(Expr.splice(mod).apply(Expr.splice(leaf.asInstanceOf[Expr[A]]))).asInstanceOf[Expr[S]]

  /** Field values of a case class, or `None` when Hearth cannot read them (a case class declaring a method that
    * overloads a field's name trips an assertion in `caseFieldValuesAt` — see
    * `docs/research/hearth-bug-case-field-overloaded-by-method.md`); the caller then goes through `copy`.
    */
  private def caseFieldValuesOf[S: Type](caseClass: CaseClass[S], sExpr: Expr[S]): Option[Map[String, Expr_??]] =
    try Some(caseClass.caseFieldValuesAt(sExpr))
    catch { case _: AssertionError => None }

  /** Whether a case class is rebuilt by calling its constructor (equivalent to the generated `copy`, without its
    * default-argument accessors): every parameter is in the first parameter list and `copy` is the generated one. A
    * case class with further parameter lists or a hand-written `copy` goes through `copy`.
    */
  private def rebuildsWithConstructor[S: Type](caseClass: CaseClass[S]): Boolean =
    caseClass.primaryConstructor.totalParameters.count(_.nonEmpty) <= 1 &&
      // a hand-written `copy` replaces the generated one: it is what quicklens calls, so we call it too
      Method.unsortedMethodsNamed[S]("copy").forall(_.isSynthetic)

  /** Descend `fields` into a type that is neither a (plain) case class nor a sealed hierarchy, as quicklens does: read
    * each field with its accessor and rebuild the value with ONE `s.copy(field1 = ..., field2 = ...)`. This covers
    * classes with a hand-written `copy`, intersection types and self types (`A & B`, `this` of a class with a self
    * type), and case classes with a second, implicit parameter list (its implicits are resolved at the call site).
    *
    * Among several `copy` overloads having these parameters, the compiler-generated one is preferred, then the one with
    * most parameters (all the other parameters need default values).
    */
  private def buildFieldsViaCopy[S: Type](sExpr: Expr[S], fields: List[(String, Vector[PathTree])], reason: String)(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    def notSupported(details: String): Nothing = abort(
      s"`modify` can only descend a field into a case class, a sealed hierarchy/enum (whose every case has that " +
        s"field) or a class with a `copy` method taking that field, but [${Type.prettyPrint[S]}] is none of them: " +
        s"$details ($reason)"
    )
    val names = fields.map(_._1)
    val rebuilt: Map[String, Expr_??] = fields.map { case (field, trees) =>
      val focused: Expr_?? = Method
        .unsortedMethodsNamed[S](field)
        .iterator
        .filter(_.isNullary)
        .flatMap(callOrError(_)(_ => sExpr.as_??, _ => Map.empty, _ => Map.empty).toOption)
        .take(1)
        .toList
        .headOption
        .getOrElse(notSupported(s"it has no accessible member `$field`"))
      field -> applyFieldTrees[S](focused, trees)(transformLeaf)
    }.toMap
    val copies = Method
      .unsortedMethodsNamed[S]("copy")
      .filter(m => m.isAvailable(AtCallSite) && m.totalParameters.headOption.exists(ps => names.forall(ps.contains)))
      .sortBy(m => (!m.isSynthetic, -m.arity))
    if (copies.isEmpty)
      notSupported(s"it has no `copy` method with ${names.map(n => s"`$n`").mkString("parameter(s) ", ", ", "")}")
    val attempts: List[Either[String, Expr_??]] = copies.map { copy =>
      callOrError(copy)(
        _ => sExpr.as_??,
        _ => Map.empty,
        av =>
          av.parameters.flatten.flatMap { case (name, parameter) =>
            rebuilt.get(name) match {
              case Some(value)                  => List(name -> value)
              case None if parameter.isImplicit => summonParameter(parameter).map(name -> _).toList
              case None                         => Nil
            }
          }.toMap
      )
    }
    attempts.collectFirst { case Right(result) => result } match {
      case Some(result) =>
        import result.{Underlying as R, value as copied}
        castTo[R, S](copied)
      case None =>
        notSupported(
          s"calling `copy(${names.map(n => s"$n = ...").mkString(", ")})` failed: " +
            attempts.collect { case Left(e) => e }.mkString("; ")
        )
    }
  }

  // Hearth reports some unsupported shapes (e.g. a default argument of a Scala 3 extension `copy`, which refers to
  // the extension's receiver) with an exception rather than a `Left`.
  private def callOrError(method: Method)(
      onInstance: Method.OnInstance => Expr_??,
      onTypes: Method.ApplyTypes => UntypedTypeArguments,
      onValues: Method.ApplyValues => Arguments
  ): Either[String, Expr_??] =
    try method.fold(onInstance, onTypes, onValues)
    catch { case scala.util.control.NonFatal(e) => Left(e.getMessage.linesIterator.mkString(" ").trim) }

  private def summonParameter(parameter: Parameter): Option[Expr_??] = {
    import parameter.tpe.Underlying as P
    Expr.summonImplicit[P].toOption.map(_.as_??)
  }

  /** `copy` of an intersection type (`A & B`) returns only the case class (`A`): cast back to the static type, as
    * quicklens does.
    */
  private def castTo[R: Type, S: Type](e: Expr[R]): Expr[S] =
    if (Type.isSubtypeOf[R, S]) e.upcast[S] else Expr.quote(Expr.splice(e).asInstanceOf[S])

  /** Emit the `.each` step over `sExpr: S` (where `S` IS the container type `F[A]`/`M[K,A]`). Summon the relevant
    * functor, build the element lambda `a => <rest of path on a>` via `LambdaBuilder`, and call the runtime helper.
    */
  private def buildEachStep[S: Type](sExpr: Expr[S], each: PathStep.Each, rest: PathTree)(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    // Dispatch the `.each` traversal on Hearth's std SPI: `IsMap` (checked first — `Map <: Iterable`), then
    // `IsCollection`, then `IsOption`. This covers every container a provider supports — the built-ins plus anything a
    // jar on the classpath registers (cats `NonEmptyList`/`NonEmptyMap` via `kindlings-cats-integration`, java
    // collections, `Optional`, ...). The element type is taken from the matcher, not from `each.elem`.
    Type[S] match {
      case IsMap(m) =>
        import m.Underlying as Pair
        buildEachMap[S, Pair](sExpr, m.value, each.predicate, rest)(transformLeaf)
      case IsCollection(c) =>
        import c.Underlying as Item
        buildEachCollection[S, Item](sExpr, c.value, each.predicate, rest)(transformLeaf)
      case IsOption(o) =>
        import o.Underlying as Inner
        buildEachOption[S, Inner](sExpr, o.value, each.predicate, rest)(transformLeaf)
      case _ =>
        abort(
          s"`modify`: `.each` is not supported on [${Type.prettyPrint[S]}] — it must be a collection, map or Option, " +
            "i.e. a type with an IsCollection/IsMap/IsOption provider (the built-ins, or e.g. cats `NonEmpty*` once " +
            "`kindlings-cats-integration` is on the classpath)."
        )
    }

  /** Transform one element/value `elemExpr` by applying the remaining path steps; for `.eachWhere` apply the transform
    * only when the predicate holds, otherwise keep the element. The leaf transform is `transformLeaf` (over the
    * container `S`) re-typed at the structurally-coincident element type `Elem` (sound: it only fires when `rest`
    * bottoms out at the focus, where `S`/`Elem` coincide).
    */
  private def transformElementExpr[S: Type, Elem: Type](
      elemExpr: Expr[Elem],
      rest: PathTree,
      predicate: Option[Expr_??]
  )(transformLeaf: Expr[S] => Expr[S]): Expr[Elem] = {
    val transformed: Expr[Elem] =
      descend[S, Elem](elemExpr, rest)(transformLeaf)
    predicate match {
      case None    => transformed
      case Some(p) =>
        val pred = p.value.asInstanceOf[Expr[Elem => Boolean]]
        Expr.quote {
          if (Expr.splice(pred).apply(Expr.splice(elemExpr))) Expr.splice(transformed) else Expr.splice(elemExpr)
        }
    }
  }

  /** `.each` over a collection via `IsCollection`: iterate the source, transform each element, and rebuild the same
    * collection type through the provider's `factory`/`build` (a `mutable.Builder` fed in a `while` loop, then handed
    * to `build.ctor`). Mirrors the decoder rules' builder pattern, but reading from the existing collection.
    */
  private def buildEachCollection[S: Type, Item: Type](
      sExpr: Expr[S],
      isColl: IsCollectionOf[S, Item],
      predicate: Option[Expr_??],
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = {
    import isColl.CtorResult
    val factoryExpr = isColl.factory
    val srcIterable = isColl.asIterable(sExpr)
    val builderExpr: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = Expr.quote {
      val src = Expr.splice(srcIterable)
      val builder = Expr.splice(factoryExpr).newBuilder
      val it = src.iterator
      while (it.hasNext) {
        val x: Item = it.next()
        val _ = builder += Expr.splice(transformElementExpr[S, Item](Expr.quote(x), rest, predicate)(transformLeaf))
      }
      builder
    }
    fromCtorResult[S, Item, CtorResult](isColl.build, builderExpr)
  }

  /** `.each` over the *values* of a map via `IsMap`: iterate the entries, transform each value, keep keys, and rebuild
    * via the provider's `factory`/`build` and `pair`/`key`/`value`.
    */
  private def buildEachMap[S: Type, Pair: Type](
      sExpr: Expr[S],
      isMap: IsMapOf[S, Pair],
      predicate: Option[Expr_??],
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = {
    import isMap.{Value, CtorResult}
    val factoryExpr = isMap.factory
    val srcIterable = isMap.asIterable(sExpr)
    val builderExpr: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = Expr.quote {
      val src = Expr.splice(srcIterable)
      val builder = Expr.splice(factoryExpr).newBuilder
      val it = src.iterator
      while (it.hasNext) {
        val p: Pair = it.next()
        val _ = builder += Expr.splice {
          val pExpr: Expr[Pair] = Expr.quote(p)
          val newValue: Expr[Value] = transformElementExpr[S, Value](isMap.value(pExpr), rest, predicate)(transformLeaf)
          isMap.pair(isMap.key(pExpr), newValue)
        }
      }
      builder
    }
    fromCtorResult[S, Pair, CtorResult](isMap.build, builderExpr)
  }

  /** `.each` over an `Option` via `IsOption`: map the contained value (None untouched). */
  private def buildEachOption[S: Type, Inner: Type](
      sExpr: Expr[S],
      isOpt: IsOptionOf[S, Inner],
      predicate: Option[Expr_??],
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] =
    isOpt.fold[S](sExpr)(
      isOpt.empty,
      (v: Expr[Inner]) => isOpt.of(transformElementExpr[S, Inner](v, rest, predicate)(transformLeaf))
    )

  /** Turn a populated `Builder[Item, CtorResult]` into the rebuilt collection `Expr[S]` via `build.ctor`, handling all
    * five `CtorLikeOf` shapes. `.each` is size-preserving over a valid source, so the smart-constructor
    * (`Either`-typed) shapes never legitimately fail; a `Left` there indicates a broken provider and throws a clear
    * error.
    */
  private def fromCtorResult[S: Type, Item: Type, CtorResult: Type](
      build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], S],
      builderExpr: Expr[scala.collection.mutable.Builder[Item, CtorResult]]
  ): Expr[S] = {
    val built = build.ctor(builderExpr)
    build match {
      // `Result[S] = S` — the ctor already yields the collection.
      case _: CtorLikeOf.PlainValue[?, ?] => built.asInstanceOf[Expr[S]]
      // Smart-constructor collections yield `Either[_, S]`; `.each` preserves size over a valid source, so a `Left` is
      // unreachable in practice. Unwrap to `S`, surfacing a clear error in the impossible case.
      case _: CtorLikeOf.EitherStringOrValue[?, ?] =>
        unwrapSmartCtor[S](built.asInstanceOf[Expr[Either[Any, S]]])
      case _: CtorLikeOf.EitherIterableStringOrValue[?, ?] =>
        unwrapSmartCtor[S](built.asInstanceOf[Expr[Either[Any, S]]])
      case _: CtorLikeOf.EitherThrowableOrValue[?, ?] =>
        unwrapSmartCtor[S](built.asInstanceOf[Expr[Either[Any, S]]])
      case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] =>
        unwrapSmartCtor[S](built.asInstanceOf[Expr[Either[Any, S]]])
    }
  }

  private def unwrapSmartCtor[S: Type](built: Expr[Either[Any, S]]): Expr[S] =
    Expr.quote {
      Expr.splice(built) match {
        case Right(value) => value
        case Left(_)      =>
          throw new RuntimeException("optics `.each`: the collection's smart constructor rejected the rebuilt value")
      }
    }

  /** Emit a `.at(i)` / `.index(i)` / `.atOrElse(i, default)` (Seq/Map) or `.at` / `.index` / `.atOrElse(default)`
    * (Option) step over `sExpr: S` (where `S` IS the container). Summon the relevant indexed functor, build the element
    * lambda for `rest`, and call the matching runtime helper, threading the index/default through as erased `Any`s.
    */
  /** Emit a `.at(i)` / `.index(i)` / `.atOrElse(i, default)` step. Indexed access is just iterate-and-rebuild applied
    * at one position/key, so it goes through the same SPI as `.each`: `IsCollection` (positional, with an index
    * counter), `IsMap` (by key), `IsOption` (the single value). No positional-update SPI or bespoke functor is needed.
    */
  private def buildAtStep[S: Type](sExpr: Expr[S], at: PathStep.At, rest: PathTree)(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    Type[S] match {
      case IsMap(m) =>
        import m.Underlying as Pair
        buildAtMap[S, Pair](sExpr, m.value, at, rest)(transformLeaf)
      case IsOption(o) =>
        import o.Underlying as Inner
        buildAtOption[S, Inner](sExpr, o.value, at, rest)(transformLeaf)
      case IsCollection(c) =>
        import c.Underlying as Item
        buildAtSeq[S, Item](sExpr, c.value, at, rest)(transformLeaf)
      case _ =>
        abort(
          s"`modify`: `.at`/`.index`/`.atOrElse` is not supported on [${Type.prettyPrint[S]}] — it must be a Seq " +
            "(positional), a Map (by key) or an Option."
        )
    }

  /** `.at(i)` / `.index(i)` / `.atOrElse(i, default)` over a `Seq` via `IsCollection`: iterate with an index counter,
    * transform the element at `i`, and rebuild. `.at` throws if `i` is out of range; `.index` is a no-op if absent;
    * `.atOrElse` appends `f(default)` when `i` is absent (as quicklens: at the end, whatever `i` is).
    */
  private def buildAtSeq[S: Type, Item: Type](
      sExpr: Expr[S],
      isColl: IsCollectionOf[S, Item],
      at: PathStep.At,
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = {
    import isColl.CtorResult
    import PathStep.AtKind
    val factoryExpr = isColl.factory
    val srcIterable = isColl.asIterable(sExpr)
    val idxExpr = at.idx
      .getOrElse(abort("`modify`: `.at`/`.index`/`.atOrElse` over a Seq needs an index"))
      .value
      .asInstanceOf[Expr[Int]]
    def tx(itemExpr: Expr[Item]): Expr[Item] = transformElementExpr[S, Item](itemExpr, rest, None)(transformLeaf)
    // What reading a missing index throws: an `Array` (`array(i)`) throws the more specific exception.
    val isArray = Type.Ctor1.of[Array].unapply(Type[S]).isDefined
    def outOfBounds(target: Expr[Int], size: Expr[Int]): Expr[IndexOutOfBoundsException] =
      if (isArray)
        Expr.quote {
          new ArrayIndexOutOfBoundsException(
            "Index " + Expr.splice(target) + " out of bounds for length " + Expr.splice(size)
          ): IndexOutOfBoundsException
        }
      else
        Expr.quote {
          new IndexOutOfBoundsException(
            Expr.splice(target).toString + " is out of bounds (min 0, max " + (Expr.splice(size) - 1) + ")"
          )
        }
    val builderExpr: Expr[scala.collection.mutable.Builder[Item, CtorResult]] = at.kind match {
      case AtKind.Index =>
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val target = Expr.splice(idxExpr)
          val it = Expr.splice(srcIterable).iterator
          var i = 0
          while (it.hasNext) {
            val x: Item = it.next()
            val _ = builder += (if (i == target) Expr.splice(tx(Expr.quote(x))) else x)
            i += 1
          }
          builder
        }
      case AtKind.At =>
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val target = Expr.splice(idxExpr)
          val it = Expr.splice(srcIterable).iterator
          var i = 0
          var found = false
          while (it.hasNext) {
            val x: Item = it.next()
            if (i == target) { found = true; val _ = builder += Expr.splice(tx(Expr.quote(x))) }
            else { val _ = builder += x }
            i += 1
          }
          // As quicklens (`Seq#apply`): a missing index is an `IndexOutOfBoundsException`.
          if (!found) throw Expr.splice(outOfBounds(Expr.quote(target), Expr.quote(i)))
          builder
        }
      case AtKind.AtOrElse =>
        val defaultExpr =
          defaultAs[Item](at.default.getOrElse(abort("`modify`: `.atOrElse` needs a default")))
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val target = Expr.splice(idxExpr)
          val it = Expr.splice(srcIterable).iterator
          var i = 0
          var found = false
          while (it.hasNext) {
            val x: Item = it.next()
            if (i == target) { found = true; val _ = builder += Expr.splice(tx(Expr.quote(x))) }
            else { val _ = builder += x }
            i += 1
          }
          // As quicklens: when the index is absent, `f(default)` is appended (whatever the index).
          if (!found) { val _ = builder += Expr.splice(tx(defaultExpr)) }
          builder
        }
    }
    fromCtorResult[S, Item, CtorResult](isColl.build, builderExpr)
  }

  /** `.at(key)` / `.index(key)` / `.atOrElse(key, default)` over a `Map` via `IsMap`: iterate entries, transform the
    * value whose key matches, and rebuild. `.at` throws if absent; `.index` is a no-op; `.atOrElse` inserts `(key,
    * f(default))`.
    */
  private def buildAtMap[S: Type, Pair: Type](
      sExpr: Expr[S],
      isMap: IsMapOf[S, Pair],
      at: PathStep.At,
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = {
    import isMap.{Key, Value, CtorResult}
    import PathStep.AtKind
    val factoryExpr = isMap.factory
    val srcIterable = isMap.asIterable(sExpr)
    val keyExpr = at.idx
      .getOrElse(abort("`modify`: `.at`/`.index`/`.atOrElse` over a Map needs a key"))
      .value
      .asInstanceOf[Expr[Key]]
    def entry(pExpr: Expr[Pair]): Expr[Pair] =
      isMap.pair(isMap.key(pExpr), transformElementExpr[S, Value](isMap.value(pExpr), rest, None)(transformLeaf))
    val builderExpr: Expr[scala.collection.mutable.Builder[Pair, CtorResult]] = at.kind match {
      case AtKind.Index =>
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val key = Expr.splice(keyExpr)
          val it = Expr.splice(srcIterable).iterator
          while (it.hasNext) {
            val p: Pair = it.next()
            val _ = builder += (if (Expr.splice(isMap.key(Expr.quote(p))) == key) Expr.splice(entry(Expr.quote(p)))
                                else p)
          }
          builder
        }
      case AtKind.At =>
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val key = Expr.splice(keyExpr)
          val it = Expr.splice(srcIterable).iterator
          var found = false
          while (it.hasNext) {
            val p: Pair = it.next()
            if (Expr.splice(isMap.key(Expr.quote(p))) == key) {
              found = true; val _ = builder += Expr.splice(entry(Expr.quote(p)))
            } else { val _ = builder += p }
          }
          if (!found) throw new NoSuchElementException("key not found: " + key)
          builder
        }
      case AtKind.AtOrElse =>
        val defaultExpr =
          defaultAs[Value](at.default.getOrElse(abort("`modify`: `.atOrElse` needs a default")))
        val newEntry = isMap.pair(keyExpr, transformElementExpr[S, Value](defaultExpr, rest, None)(transformLeaf))
        Expr.quote {
          val builder = Expr.splice(factoryExpr).newBuilder
          val key = Expr.splice(keyExpr)
          val it = Expr.splice(srcIterable).iterator
          var found = false
          while (it.hasNext) {
            val p: Pair = it.next()
            if (Expr.splice(isMap.key(Expr.quote(p))) == key) {
              found = true; val _ = builder += Expr.splice(entry(Expr.quote(p)))
            } else { val _ = builder += p }
          }
          if (!found) { val _ = builder += Expr.splice(newEntry) }
          builder
        }
    }
    fromCtorResult[S, Pair, CtorResult](isMap.build, builderExpr)
  }

  /** `.at` / `.index` / `.atOrElse(default)` over an `Option` via `IsOption`: transform the contained value. `.at`
    * throws on `None`; `.index` is a no-op on `None`; `.atOrElse` transforms the value or the default.
    */
  private def buildAtOption[S: Type, Inner: Type](
      sExpr: Expr[S],
      isOpt: IsOptionOf[S, Inner],
      at: PathStep.At,
      rest: PathTree
  )(transformLeaf: Expr[S] => Expr[S]): Expr[S] = {
    import PathStep.AtKind
    def tx(v: Expr[Inner]): Expr[Inner] = transformElementExpr[S, Inner](v, rest, None)(transformLeaf)
    at.kind match {
      case AtKind.Index =>
        isOpt.fold[S](sExpr)(isOpt.empty, (v: Expr[Inner]) => isOpt.of(tx(v)))
      case AtKind.At =>
        isOpt.fold[S](sExpr)(throwAsS[S]("None.at"), (v: Expr[Inner]) => isOpt.of(tx(v)))
      case AtKind.AtOrElse =>
        val defaultExpr =
          defaultAs[Inner](at.default.getOrElse(abort("`modify`: `.atOrElse` needs a default")))
        isOpt.of(tx(isOpt.getOrElse(sExpr)(defaultExpr)))
    }
  }

  /** The default of `.atOrElse`, typed as the element: an expression of a narrower type (`???`, typed `Nothing`) is
    * ascribed, so the rest of the path sees the element type.
    */
  private def defaultAs[T: Type](default: Expr_??): Expr[T] = {
    import default.Underlying as D
    if (Type[D] =:= Type[T]) default.value.asInstanceOf[Expr[T]] else ascribe[D, T](default.value)
  }
  private def ascribe[D: Type, T: Type](e: Expr[D]): Expr[T] = {
    val widened = e.asInstanceOf[Expr[T]]
    Expr.quote { val default: T = Expr.splice(widened); default }
  }

  /** A `throw new NoSuchElementException(msg)` expression ascribed to `S` (for the `None`-branch of Option `.at`). */
  private def throwAsS[S: Type](msg: String): Expr[S] = {
    val m = Expr(msg)
    Expr.quote(throw new NoSuchElementException(Expr.splice(m))).asInstanceOf[Expr[S]]
  }

  /** Emit a `.eachLeft` / `.eachRight` step over `sExpr: S` (an `Either[L, R]`) via Hearth's `IsEither` SPI: fold the
    * either, transform the targeted branch, and rebuild with `left`/`right`. The other branch is left untouched.
    */
  private def buildEitherStep[S: Type](sExpr: Expr[S], step: PathStep.EachEither, rest: PathTree)(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] =
    Type[S] match {
      case IsEither(e) =>
        import e.{LeftValue, RightValue}
        if (step.isLeft)
          e.value.fold[S](sExpr)(
            (l: Expr[LeftValue]) => e.value.left(transformElementExpr[S, LeftValue](l, rest, None)(transformLeaf)),
            (r: Expr[RightValue]) => e.value.right(r)
          )
        else
          e.value.fold[S](sExpr)(
            (l: Expr[LeftValue]) => e.value.left(l),
            (r: Expr[RightValue]) => e.value.right(transformElementExpr[S, RightValue](r, rest, None)(transformLeaf))
          )
      case _ =>
        abort(
          s"`modify`: `.eachLeft`/`.eachRight` is only supported on `Either`, but [${Type.prettyPrint[S]}] is not one."
        )
    }

  /** Emit a `.when[Subtype]` prism step over `sExpr: S`: a non-exhaustive 2-case match
    * `s match { case t: Subtype => <rest on t>; case other => other }`. Built with `MatchCase.typeMatch[Subtype]`
    * (recurse on the narrowed value) and `MatchCase.typeMatch[S]` (the parent type — a catch-all returning the value
    * unchanged). On Scala 3 `matchOn` annotates the scrutinee `@unchecked`, so the non-exhaustive match compiles
    * cleanly on both platforms.
    */
  private def buildWhenStep[S: Type](sExpr: Expr[S], step: PathStep.When, rest: PathTree)(
      transformLeaf: Expr[S] => Expr[S]
  ): Expr[S] = {
    import step.subtype.Underlying as Sub
    if (!Type.isSubtypeOf[Sub, S])
      abort(s"`modify`: `.when[${Type.prettyPrint[Sub]}]` requires a subtype of [${Type.prettyPrint[S]}]")
    val matchedCase = MatchCase.typeMatch[Sub]().map { (sub: Expr[Sub]) =>
      // Recurse into the narrowed subtype; the leaf transform re-types `transformLeaf` (over `S`) at the
      // structurally-coincident `Sub`, then the rebuilt `Sub` is widened back to `S` (sound: `Sub <: S`).
      val rebuilt: Expr[Sub] =
        descend[S, Sub](sub, rest)(transformLeaf)
      whenUpcast[Sub, S](rebuilt)
    }
    val fallthrough = MatchCase.typeMatch[S]().map((other: Expr[S]) => other)
    MatchCase.matchOn[S, S](sExpr)(NonEmptyVector(matchedCase, fallthrough))
  }

  /** Widen `Expr[Sub]` to `Expr[S]` via an in-tree `.asInstanceOf[S]` — sound because `Sub <: S` (checked in
    * `buildWhenStep`), and kept in-tree so the generated match arms agree on the result type `S`. (`Sub <: S` is not
    * visible to the macro at the static type level since both are abstract, so a plain ascription does not type-check;
    * the cast is a runtime no-op given the verified subtype relationship.)
    */
  private def whenUpcast[Sub: Type, S: Type](e: Expr[Sub]): Expr[S] =
    Expr.quote(Expr.splice(e).asInstanceOf[S])

  /** Reconstruct a case-class value from its (possibly modified) field expressions, by name. */
  private def reconstruct[S: Type](caseClass: CaseClass[S], fieldValues: Map[String, Expr_??]): Expr[S] =
    caseClass.construct[Id] { parameter =>
      fieldValues.getOrElse(
        parameter.name,
        abort(s"`modify`: missing value for field `${parameter.name}` of [${Type.prettyPrint[S]}]")
      )
    } match {
      case Some(expr) => expr
      case None       =>
        abort(s"`modify`: the primary constructor of [${Type.prettyPrint[S]}] is not accessible at the call site")
    }

  private def abort(msg: String): Nothing = Environment.reportErrorAndAbort(msg)
}
