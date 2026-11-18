package hearth.kindlings.parser.publicapi

import hearth.MacroSuite
import hearth.kindlings.parser.*

/** The push API (`Parser.pushMachine`) can be driven with the public API only: no `internal.runtime` import. */
final class PushMachineStatusSpec extends MacroSuite {

  group("Parser.pushMachine driven through MachineStatus") {

    test("status constants match the ones the machine returns") {
      import hearth.kindlings.parser.internal.runtime.Machine
      MachineStatus.Done ==> Machine.Done
      MachineStatus.Error ==> Machine.Error
      MachineStatus.Effect ==> Machine.Effect
      MachineStatus.NeedInput ==> Machine.NeedInput
      MachineStatus.Yield ==> Machine.Yield
    }

    test("chunks are fed until the machine is done") {
      val machine = PushMachineStatusSpec.sum.pushMachine()
      val chunks = Iterator("1 2 ", "3", "4 5", " 6")
      var result: Option[Int] = None
      while (result.isEmpty)
        machine.run() match {
          case MachineStatus.NeedInput =>
            if (chunks.hasNext) machine.feed(chunks.next()) else machine.endOfInput()
          case MachineStatus.Yield => ()
          case MachineStatus.Done  => result = Some(machine.result.asInstanceOf[Int])
          case MachineStatus.Error => throw machine.error
          case other               => fail(s"unexpected status $other")
        }
      result ==> Some(48)
    }

    test("errors are reported with MachineStatus.Error") {
      val machine = PushMachineStatusSpec.sum.pushMachine()
      machine.feed("1 x")
      machine.endOfInput()
      var status = machine.run()
      while (status == MachineStatus.Yield) status = machine.run()
      status ==> MachineStatus.Error
    }
  }
}
object PushMachineStatusSpec {

  val sum: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val sum = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    sum ::= all(rep(num)).pure(_.sum)
    sum
  }
}
