package hearth.kindlings.diffderivation

trait Diff[A] {
  def prettyName: String
  def plainName: String
  def simpleName: String
  def shortName: String
  def diff(left: A, right: A): DiffResult
  def snapshot(value: A): DiffResult

  def ignoreField(fieldName: String): Diff[A] =
    Diff.Modified(this, Map(fieldName -> Diff.Modified.Ignore))

  def modifyField(fieldName: String): Diff.FieldModifier[A] =
    new Diff.FieldModifier[A](this, fieldName)
}
object Diff extends DiffCompanionCompat {

  def apply[A](implicit ev: Diff[A]): Diff[A] = ev

  def approximate[T](epsilon: T)(implicit num: Numeric[T]): Diff[T] = new Diff[T] {
    def plainName: String = num.zero.getClass.getSimpleName
    def prettyName: String = internal.runtime.DiffRuntime.prettyTypeName(plainName)
    def simpleName: String = plainName
    def shortName: String = plainName
    def diff(left: T, right: T): DiffResult = {
      val delta = num.abs(num.minus(left, right))
      if (num.lteq(delta, epsilon))
        DiffResult.Identical(plainName, plainName, simpleName, shortName, left.toString)
      else
        DiffResult.ValueChanged(plainName, plainName, simpleName, shortName, left.toString, right.toString)
    }
    def snapshot(value: T): DiffResult =
      DiffResult.Identical(plainName, plainName, simpleName, shortName, value.toString)
  }

  class FieldModifier[A](underlying: Diff[A], fieldName: String) {
    def ignore: Diff[A] = Modified(underlying, Map(fieldName -> Modified.Ignore))
    def using(fieldDiff: Diff[?]): Diff[A] =
      Modified(underlying, Map(fieldName -> Modified.Replace(fieldDiff.asInstanceOf[Diff[Any]])))
  }

  final private[diffderivation] case class Modified[A](
      underlying: Diff[A],
      overrides: Map[String, Modified.Override]
  ) extends Diff[A] {
    def prettyName: String = underlying.prettyName
    def plainName: String = underlying.plainName
    def simpleName: String = underlying.simpleName
    def shortName: String = underlying.shortName

    def diff(left: A, right: A): DiffResult =
      applyOverrides(underlying.diff(left, right), left, right, isSnapshot = false)

    def snapshot(value: A): DiffResult =
      applyOverrides(underlying.snapshot(value), value, value, isSnapshot = true)

    override def ignoreField(fieldName: String): Diff[A] =
      Modified(underlying, overrides + (fieldName -> Modified.Ignore))

    override def modifyField(fieldName: String): FieldModifier[A] =
      new FieldModifier[A](underlying, fieldName) {
        override def ignore: Diff[A] =
          Modified(Modified.this.underlying, Modified.this.overrides + (fieldName -> Modified.Ignore))
        override def using(fieldDiff: Diff[?]): Diff[A] =
          Modified(
            Modified.this.underlying,
            Modified.this.overrides + (fieldName -> Modified.Replace(fieldDiff.asInstanceOf[Diff[Any]]))
          )
      }

    // `left`/`right` are the values `result` was computed from: the whole value for a `Record`, the subtype's value
    // for a `Variant` (whose body is the `Record` of that subtype), so a replaced field's Diff can be run on the
    // field values read by name from the case class (`Product.productElementName`).
    private def applyOverrides(result: DiffResult, left: Any, right: Any, isSnapshot: Boolean): DiffResult =
      result match {
        case r: DiffResult.Record =>
          val modifiedFields = r.fields.flatMap { case (name, fieldResult) =>
            overrides.get(name) match {
              case Some(Modified.Ignore)             => None
              case Some(Modified.Replace(fieldDiff)) =>
                val replaced = (Modified.fieldValue(left, name), Modified.fieldValue(right, name)) match {
                  case (Some(l), Some(r)) =>
                    if (isSnapshot) fieldDiff.snapshot(l) else fieldDiff.diff(l, r)
                  case _ => fieldResult
                }
                Some((name, replaced))
              case None => Some((name, fieldResult))
            }
          }
          DiffResult.Record(r.prettyName, r.plainName, r.simpleName, r.shortName, modifiedFields)
        case v: DiffResult.Variant =>
          DiffResult.Variant(
            v.prettyName,
            v.plainName,
            v.simpleName,
            v.shortName,
            v.variantName,
            applyOverrides(v.body, left, right, isSnapshot)
          )
        case other => other
      }
  }
  private[diffderivation] object Modified {
    sealed trait Override
    case object Ignore extends Override
    final case class Replace(diff: Diff[Any]) extends Override

    /** Reads a case class field by name, if `value` is a `Product` with such an element. */
    def fieldValue(value: Any, name: String): Option[Any] = value match {
      case p: Product =>
        var i = 0
        val n = p.productArity
        var found: Option[Any] = None
        while (found.isEmpty && i < n) {
          if (p.productElementName(i) == name) found = Some(p.productElement(i))
          i += 1
        }
        found
      case _ => None
    }
  }

  def seqDiff[A](elemDiff: Diff[A], matcher: ObjectMatcher[A]): Diff[Iterable[A]] = new Diff[Iterable[A]] {
    // each name style from the element's name in the same style (`prettyName` is the only one with ANSI colors)
    def prettyName: String = s"Iterable[${elemDiff.prettyName}]"
    def plainName: String = s"Iterable[${elemDiff.plainName}]"
    def simpleName: String = s"Iterable[${elemDiff.simpleName}]"
    def shortName: String = s"Iterable[${elemDiff.shortName}]"
    def diff(left: Iterable[A], right: Iterable[A]): DiffResult =
      internal.runtime.DiffRuntime.diffSeqByKey(
        prettyName,
        plainName,
        simpleName,
        shortName,
        left,
        right,
        elemDiff,
        matcher
      )
    def snapshot(value: Iterable[A]): DiffResult =
      internal.runtime.DiffRuntime.diffSeq(
        prettyName,
        plainName,
        simpleName,
        shortName,
        value,
        Iterable.empty,
        elemDiff
      )
  }

  def assertNoDiff[A](left: A, right: A)(implicit diff: Diff[A]): Unit = {
    val result = diff.diff(left, right)
    if (!result.isIdentical) {
      val rendered = DiffRenderer.render(result, RenderConfig.plain)
      throw new AssertionError(s"Values differ:\n$rendered")
    }
  }

  def assertDiff[A](left: A, right: A)(implicit diff: Diff[A]): DiffResult = {
    val result = diff.diff(left, right)
    if (result.isIdentical) {
      throw new AssertionError("Expected values to differ, but they are identical")
    }
    result
  }

  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
