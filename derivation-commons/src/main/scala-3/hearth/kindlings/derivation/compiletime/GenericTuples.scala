package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons

/** Scala 3 generic tuples (`EmptyTuple`, `H *: T`, tuples with more than 22 elements, match types reducing to them),
  * which Hearth's `CaseClass` does not decompose (see `docs/research/hearth-gap-generic-tuples.md`). `TupleN` types
  * (`(A, B)`, `Tuple2[A, B]`) are not generic tuples here: `CaseClass` handles them.
  *
  * At runtime a generic tuple is a `TupleN` (up to 22 elements), a `TupleXXL` or `EmptyTuple`: its elements are read
  * with `Product.productElement` and it is built with `Tuple.fromArray`.
  */
trait GenericTuples { this: MacroCommons =>

  /** The element types of a tuple that is not a `TupleN` case class, `None` for any other type (named tuples too). */
  protected def genericTupleElementTypes[A: Type]: Option[List[??]] =
    if !Type[A].isTuple || Type[A].isNamedTuple then None
    else
      this match {
        case scala3: hearth.MacroCommonsScala3 =>
          given scala.quoted.Quotes = scala3.quotes
          import scala3.quotes.reflect.*
          val consSym = Symbol.requiredClass("scala.*:")
          // A recursive type alias (`type R = (Int, Option[R])`) is not unfolded by `dealias`.
          def unfold(t: TypeRepr): TypeRepr = t.dealias.simplified.dealias match {
            case tr: TypeRef if tr.typeSymbol.isAliasType => tr.translucentSuperType
            case other                                    => other
          }
          def loop(t: TypeRepr): Option[List[TypeRepr]] = unfold(t) match {
            case AppliedType(tycon, List(h, tl)) if tycon.typeSymbol == consSym  => loop(tl).map(h :: _)
            case AppliedType(tycon, args) if defn.isTupleClass(tycon.typeSymbol) => Some(args)
            case other if other =:= TypeRepr.of[EmptyTuple]                      => Some(Nil)
            case _                                                               => None
          }
          val tpe = UntypedType.fromTyped[A].asInstanceOf[TypeRepr]
          unfold(tpe) match {
            // a `TupleN` is handled by `CaseClass`
            case AppliedType(tycon, _) if defn.isTupleClass(tycon.typeSymbol) => None
            case _                                                            =>
              loop(tpe).map(_.map(t => UntypedType.as_??(t.asInstanceOf[UntypedType])))
          }
        case _ => None
      }

  /** `scala.Tuple.fromArray(Array[Object](elements*)).asInstanceOf[A]`: the elements are expected to be simple
    * expressions (values bound before), the array literal is the only code generated per element.
    */
  protected def constructGenericTuple[A: Type](elements: List[Expr[Any]]): Option[Expr[A]] =
    this match {
      case _: hearth.MacroCommonsScala3 =>
        // the Quotes of the splice being built (not the macro entry's): the result may be used in a nested splice
        given scala.quoted.Quotes = CrossQuotes.ctx[scala.quoted.Quotes]
        given scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
        val boxed = elements.map(e => '{ ${ e.asInstanceOf[scala.quoted.Expr[Any]] }.asInstanceOf[Object] })
        val tuple = '{ scala.Tuple.fromArray(scala.Array[Object](${ scala.quoted.Varargs(boxed) }*)).asInstanceOf[A] }
        Some(tuple.asInstanceOf[Expr[A]])
      case _ => None
    }
}
