package hearth.kindlings.parser
package internal.runtime

import scala.collection.mutable.ListBuffer

/** Tables plus the per-production run-time data collected from the evaluated grammar block. Production `p` of the
  * tables is flattened production `p - 1` (production 0 is the augmented start production).
  */
final private[parser] class CompiledGrammar(val tables: Tables, flat: Flatten.Result[String => Any, Alt.Action]) {

  import CompiledGrammar.*

  private val prodCount = flat.prods.size + 1

  /** Per production and right-hand side position: `null` for plain non-terminal values, [[Listify]] for list builders
    * to convert, otherwise the terminal's conversion function.
    */
  val converters: Array[Array[Any]] = new Array[Array[Any]](prodCount)

  /** Per production: one of the `Act*` codes. */
  val actionKind: Array[Int] = new Array[Int](prodCount)

  /** Per production: the user action function, if any. */
  val actionFn: Array[Array[Any] => Any] = new Array[Array[Any] => Any](prodCount)

  /** Per production: the constant value (`ActConst`) or the element index (`ActListAppend`). */
  val actionArg: Array[Any] = new Array[Any](prodCount)

  flat.prods.zipWithIndex.foreach { case (prod, index) =>
    val p = index + 1
    converters(p) = prod.rhs
      .map {
        case Flatten.RNt(_, true)   => Listify
        case Flatten.RNt(_, false)  => null
        case Flatten.RTerm(convert) => convert
      }
      .toArray[Any]
    prod.action match {
      case Flatten.AUser(action) =>
        actionKind(p) = if (action.effectful) ActEffect else ActPure
        actionFn(p) = action.fn
      case Flatten.APass              => actionKind(p) = ActPass
      case Flatten.AConst(value)      => actionKind(p) = ActConst; actionArg(p) = value
      case Flatten.AOptNone           => actionKind(p) = ActOptNone
      case Flatten.AOptSome           => actionKind(p) = ActOptSome
      case Flatten.AListEmpty         => actionKind(p) = ActListEmpty
      case Flatten.AListOne           => actionKind(p) = ActListOne
      case Flatten.AListAppend(index) => actionKind(p) = ActListAppend; actionArg(p) = index
    }
  }
}
private[parser] object CompiledGrammar {

  final val ActPure = 0
  final val ActEffect = 1
  final val ActPass = 2
  final val ActConst = 3
  final val ActOptNone = 4
  final val ActOptSome = 5
  final val ActListEmpty = 6
  final val ActListOne = 7
  final val ActListAppend = 8

  /** Marker converter: the value is a `ListBuffer` built by a `rep`/`sepBy` helper, convert it to a `List`. */
  object Listify

  def listify(value: Any): List[Any] = value.asInstanceOf[ListBuffer[Any]].toList
}
