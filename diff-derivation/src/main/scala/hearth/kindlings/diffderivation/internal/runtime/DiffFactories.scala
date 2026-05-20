package hearth.kindlings.diffderivation.internal.runtime

import hearth.kindlings.diffderivation.{Diff, DiffResult}

object DiffFactories {

  def instance[A](
      _prettyName: => String,
      _plainName: => String,
      _simpleName: => String,
      _shortName: => String,
      diffFn: (A, A) => DiffResult,
      snapshotFn: A => DiffResult
  ): Diff[A] = new Diff[A] {
    lazy val prettyName: String = _prettyName
    lazy val plainName: String = _plainName
    lazy val simpleName: String = _simpleName
    lazy val shortName: String = _shortName
    def diff(left: A, right: A): DiffResult = diffFn(left, right)
    def snapshot(value: A): DiffResult = snapshotFn(value)
  }
}
