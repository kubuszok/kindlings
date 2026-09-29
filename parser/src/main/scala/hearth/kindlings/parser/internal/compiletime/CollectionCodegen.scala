package hearth.kindlings.parser
package internal.compiletime

import hearth.MacroCommons
import hearth.std.StdExtensions

/** The code collecting the values of a repetition (`rep`, `rep1`, `sepBy`, `sepBy1`) into its collection, generated
  * through Hearth's `IsCollection` standard extension: the Scala and Java collections, arrays and whatever providers
  * are on the classpath (e.g. cats `NonEmptyList` with `kindlings-cats-integration`).
  *
  * The generated code feeds the collection's own mutable `Builder` (no intermediate `List`): the builder is created
  * when the repetition starts, every element is appended as it is reduced, and `result()` (or the collection's smart
  * constructor) runs once the repetition is used. All functions are closed (they refer only to their parameters), so
  * the bridges can inline them into the generated class like actions.
  */
private[parser] trait CollectionCodegen { this: MacroCommons & StdExtensions =>

  /** Untyped (compiler) trees of the functions implementing a collection.
    *
    * @param factory
    *   `Factory[Item, _]` expression, evaluated once per parser
    * @param newBuilder
    *   `(factory: Any) => Any`: a new builder
    * @param add
    *   `(builder: Any, element: Any) => Any`: appends the element, returns the builder
    * @param result
    *   `(builder: Any) => Any`: the collection
    * @param rejectable
    *   whether the collection's smart constructor can reject the values (`result` then throws `RejectedValue`)
    */
  final case class CollectionCode(factory: Any, newBuilder: Any, add: Any, result: Any, rejectable: Boolean)

  private var standardExtensionsLoaded: Boolean = false
  private def ensureStdExtensionsLoaded(): Unit =
    if (!standardExtensionsLoaded) {
      val _ = Environment.loadStandardExtensions()
      standardExtensionsLoaded = true
    }

  /** The code collecting `element`s into `collection`, or why it is not possible. */
  def collectionCode(collection: UntypedType, element: UntypedType): Either[String, CollectionCode] = {
    ensureStdExtensionsLoaded()
    val coll = UntypedType.as_??(collection)
    val elem = UntypedType.as_??(element)
    import coll.Underlying as C
    import elem.Underlying as A
    Type[C] match {
      case IsMap(_) =>
        Left(
          s"`.as[${Type.prettyPrint[C]}]`: repetitions cannot be collected into maps (collect pairs into a sequence and convert it in the action)"
        )
      case IsCollection(c) =>
        import c.Underlying as Item
        if (Type[A] <:< Type[Item]) Right(code[C, Item](c.value))
        else
          Left(
            s"`.as[${Type.prettyPrint[C]}]`: the repeated values are ${Type.prettyPrint[A]}, which is not a subtype of the collection's element type ${Type.prettyPrint[Item]}"
          )
      case _ =>
        Left(
          s"`.as[${Type.prettyPrint[C]}]`: ${Type.prettyPrint[C]} is not a supported collection (it needs an IsCollection provider: " +
            "Scala and Java collections and arrays are supported out of the box, other ones by a provider on the classpath, " +
            "e.g. cats `NonEmptyList` / `Chain` with kindlings-cats-integration)"
        )
    }
  }

  private def code[C: Type, Item: Type](c: IsCollectionOf[C, Item]): CollectionCode = {
    import c.CtorResult
    val factory: Expr[scala.collection.Factory[Item, CtorResult]] = c.factory
    val factoryAny: Expr[Any] = Expr.quote((Expr.splice(factory): Any))
    val newBuilder: Expr[Any => Any] = Expr.quote { (f: Any) =>
      (f.asInstanceOf[scala.collection.Factory[Item, CtorResult]].newBuilder: Any)
    }
    val add: Expr[(Any, Any) => Any] = Expr.quote { (b: Any, e: Any) =>
      val _ = b.asInstanceOf[scala.collection.mutable.Builder[Item, CtorResult]].addOne(e.asInstanceOf[Item])
      b
    }
    val result: Expr[Any => Any] = Expr.quote { (b: Any) =>
      val builder = b.asInstanceOf[scala.collection.mutable.Builder[Item, CtorResult]]
      (Expr.splice(fromCtorResult[C, Item, CtorResult](c.build, Expr.quote(builder))): Any)
    }
    val rejectable = c.build match {
      case _: CtorLikeOf.PlainValue[?, ?] => false
      case _                              => true
    }
    CollectionCode(factoryAny.asUntyped, newBuilder.asUntyped, add.asUntyped, result.asUntyped, rejectable)
  }

  /** The collection built by `build` (handling the five `CtorLikeOf` shapes: smart constructors yield `Either`s). */
  private def fromCtorResult[C: Type, Item: Type, CtorResult: Type](
      build: CtorLikeOf[scala.collection.mutable.Builder[Item, CtorResult], C],
      builder: Expr[scala.collection.mutable.Builder[Item, CtorResult]]
  ): Expr[C] = {
    val built = build.ctor(builder)
    build match {
      case _: CtorLikeOf.PlainValue[?, ?]                     => built.asInstanceOf[Expr[C]]
      case _: CtorLikeOf.EitherStringOrValue[?, ?]            => unwrap[C](built.asInstanceOf[Expr[Either[Any, C]]])
      case _: CtorLikeOf.EitherIterableStringOrValue[?, ?]    => unwrap[C](built.asInstanceOf[Expr[Either[Any, C]]])
      case _: CtorLikeOf.EitherThrowableOrValue[?, ?]         => unwrap[C](built.asInstanceOf[Expr[Either[Any, C]]])
      case _: CtorLikeOf.EitherIterableThrowableOrValue[?, ?] => unwrap[C](built.asInstanceOf[Expr[Either[Any, C]]])
    }
  }

  /** A rejection becomes a `RejectedValue`, which the machine reports as a `ParseError` in the engine's `F`. */
  private def unwrap[C: Type](built: Expr[Either[Any, C]]): Expr[C] = {
    val name = Expr(Type.plainPrint[C].replace("_root_.", ""))
    Expr.quote {
      Expr.splice(built) match {
        case Right(value) => value
        case Left(error)  =>
          throw hearth.kindlings.parser.internal.runtime.RejectedValue(Expr.splice(name), error)
      }
    }
  }
}
