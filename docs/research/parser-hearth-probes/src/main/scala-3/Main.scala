package parserhearthprobe

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.MinLength

object Main {
  type AtLeastTwo = List[Int] :| MinLength[2]

  def main(args: Array[String]): Unit = {
    val matches = Probe.aliasMatches[AtLeastTwo]
    println(s"Iron alias: original=${matches._1}, dealiased=${matches._2}")
    val instance = Probe.selfReference
    assert(instance.identity eq instance)
    assert(instance.viaSelf == 42)
    println("AnonymousInstance: fluent this.type and calls via self passed")
  }
}
