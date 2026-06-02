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
    def prettyName: String = num.zero.getClass.getSimpleName
    def plainName: String = prettyName
    def simpleName: String = prettyName
    def shortName: String = prettyName
    def diff(left: T, right: T): DiffResult = {
      val delta = num.abs(num.minus(left, right))
      if (num.lteq(delta, epsilon))
        DiffResult.Identical(prettyName, plainName, simpleName, shortName, left.toString)
      else
        DiffResult.ValueChanged(prettyName, plainName, simpleName, shortName, left.toString, right.toString)
    }
    def snapshot(value: T): DiffResult =
      DiffResult.Identical(prettyName, plainName, simpleName, shortName, value.toString)
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
      applyOverrides(underlying.diff(left, right))

    def snapshot(value: A): DiffResult =
      applyOverrides(underlying.snapshot(value))

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

    private def applyOverrides(result: DiffResult): DiffResult = result match {
      case r: DiffResult.Record =>
        val modifiedFields = r.fields.flatMap { case (name, fieldResult) =>
          overrides.get(name) match {
            case Some(Modified.Ignore)     => None
            case Some(Modified.Replace(_)) => Some((name, fieldResult))
            case None                      => Some((name, fieldResult))
          }
        }
        DiffResult.Record(r.prettyName, r.plainName, r.simpleName, r.shortName, modifiedFields)
      case v: DiffResult.Variant =>
        DiffResult.Variant(v.prettyName, v.plainName, v.simpleName, v.shortName, v.variantName, applyOverrides(v.body))
      case other => other
    }
  }
  private[diffderivation] object Modified {
    sealed trait Override
    case object Ignore extends Override
    final case class Replace(diff: Diff[Any]) extends Override
  }

  def seqDiff[A](elemDiff: Diff[A], matcher: ObjectMatcher[A]): Diff[Iterable[A]] = new Diff[Iterable[A]] {
    def prettyName: String = s"Iterable[${elemDiff.prettyName}]"
    def plainName: String = prettyName
    def simpleName: String = prettyName
    def shortName: String = prettyName
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
