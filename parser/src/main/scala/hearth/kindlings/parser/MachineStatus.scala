package hearth.kindlings.parser

/** The statuses returned by `machine.run(budget)` of a machine created with [[Parser.pushMachine]]:
  *
  * {{{
  * machine.run() match {
  *   case MachineStatus.NeedInput => machine.feed(nextChunk) // or machine.endOfInput()
  *   case MachineStatus.Effect    => machine.resume(runEffect(machine.pendingEffect))
  *   case MachineStatus.Yield     => () // the step budget was spent: run again
  *   case MachineStatus.Done      => machine.result
  *   case MachineStatus.Error     => throw machine.error
  * }
  * }}}
  *
  * The values are the same as the (internal) `Machine.Done`, `Machine.NeedInput`, ... constants.
  */
object MachineStatus {

  /** Parsing finished: the value is in `machine.result`. */
  final val Done = 0

  /** Parsing failed: the [[ParseError]] is in `machine.error`. */
  final val Error = 1

  /** An effectful action produced `machine.pendingEffect`: run it and continue with `machine.resume(result)`. */
  final val Effect = 2

  /** The input buffer is exhausted: `machine.feed(chunk)` or `machine.endOfInput()`, then run again. */
  final val NeedInput = 3

  /** The step budget passed to `run` was spent: run again. */
  final val Yield = 4
}
