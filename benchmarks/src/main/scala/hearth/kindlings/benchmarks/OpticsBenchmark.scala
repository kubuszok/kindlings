package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** Runtime cost of a generated `modify` lens vs. a hand-written `.copy` chain vs. SoftwareMill quicklens (the library
  * `kindlings-optics` reimplements). All three should produce essentially identical bytecode, so this benchmark exists
  * to confirm kindlings-optics carries no runtime overhead over a hand-written copy or over quicklens.
  */
object OpticsModel {
  final case class OStreet(name: String, number: Int)
  final case class OAddress(street: OStreet, city: String)
  final case class OCompany(name: String, address: OAddress)
  final case class OEmployee(name: String, salary: Int, company: OCompany)
  final case class OTeam(members: List[OEmployee])

  val employee: OEmployee =
    OEmployee("Alice", 100000, OCompany("Acme", OAddress(OStreet("Main", 1), "Springfield")))

  val team: OTeam = OTeam(List.tabulate(16)(i => employee.copy(name = s"E$i", salary = 90000 + i)))

  final case class OScores(values: Vector[Int], byName: Map[String, Int])
  val scores: OScores = OScores(Vector.tabulate(16)(identity), (0 until 16).map(i => s"k$i" -> i).toMap)

  val salaries: Array[OStreet] = Array.tabulate(16)(i => OStreet(s"s$i", i))

  /** Not a case class: rebuilt through its hand-written `copy`. */
  final class OPoint(val x: Int, val y: Int) {
    def copy(x: Int = x, y: Int = y): OPoint = new OPoint(x, y)
  }
  val point: OPoint = new OPoint(1, 2)
}

/** kindlings-optics — `obj.modify(_.a.b.c)`. */
object KindlingsOpticsInstances {
  import hearth.kindlings.optics.*
  import OpticsModel.*

  def deepName(e: OEmployee): OEmployee = e.modify(_.company.address.street.name).using(_.toUpperCase)
  def eachSalary(t: OTeam): OTeam = t.modify(_.members.each.salary).using(_ + 1)
  // Paths sharing a prefix and an `.each`: one traversal, one rebuild per element.
  def allNames(t: OTeam): OTeam =
    t.modifyAll(_.members.each.name, _.members.each.company.name, _.members.each.company.address.city)
      .using(_.toUpperCase)
  def vectorAt(s: OScores): OScores = s.modify(_.values.at(7)).using(_ + 1)
  def mapAt(s: OScores): OScores = s.modify(_.byName.at("k7")).using(_ + 1)
  def arrayIndex(a: Array[OStreet]): Array[OStreet] = modify(a)(_.index(7).number).using(_ + 1)
  def explicitCopy(p: OPoint): OPoint = p.modifyAll(_.x, _.y).using(_ + 1)
  def eachWhereSalary(t: OTeam): OTeam = t.modify(_.members.eachWhere(_.salary > 90007).salary).using(_ + 1)
}

/** SoftwareMill quicklens baseline — same `.modify(_.a.b.c)` surface. */
object QuicklensOpticsInstances {
  import com.softwaremill.quicklens.*
  import OpticsModel.*

  def deepName(e: OEmployee): OEmployee = e.modify(_.company.address.street.name).using(_.toUpperCase)
  def eachSalary(t: OTeam): OTeam = t.modify(_.members.each.salary).using(_ + 1)
  // Paths sharing a prefix and an `.each`: one traversal, one rebuild per element.
  def allNames(t: OTeam): OTeam =
    t.modifyAll(_.members.each.name, _.members.each.company.name, _.members.each.company.address.city)
      .using(_.toUpperCase)
  def vectorAt(s: OScores): OScores = s.modify(_.values.at(7)).using(_ + 1)
  def mapAt(s: OScores): OScores = s.modify(_.byName.at("k7")).using(_ + 1)
  def arrayIndex(a: Array[OStreet]): Array[OStreet] = modify(a)(_.index(7).number).using(_ + 1)
  def explicitCopy(p: OPoint): OPoint = p.modifyAll(_.x, _.y).using(_ + 1)
  def eachWhereSalary(t: OTeam): OTeam = t.modify(_.members.eachWhere(_.salary > 90007).salary).using(_ + 1)
}

/** Hand-written `.copy` baseline. */
object HandWrittenOpticsInstances {
  import OpticsModel.*

  def deepName(e: OEmployee): OEmployee =
    e.copy(company =
      e.company.copy(address =
        e.company.address.copy(street = e.company.address.street.copy(name = e.company.address.street.name.toUpperCase))
      )
    )

  def eachSalary(t: OTeam): OTeam = t.copy(members = t.members.map(m => m.copy(salary = m.salary + 1)))
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class OpticsBenchmark {

  @Benchmark def kindlingsDeepName(): OpticsModel.OEmployee = KindlingsOpticsInstances.deepName(OpticsModel.employee)
  @Benchmark def quicklensDeepName(): OpticsModel.OEmployee = QuicklensOpticsInstances.deepName(OpticsModel.employee)
  @Benchmark def handWrittenDeepName(): OpticsModel.OEmployee =
    HandWrittenOpticsInstances.deepName(OpticsModel.employee)

  @Benchmark def kindlingsEachSalary(): OpticsModel.OTeam = KindlingsOpticsInstances.eachSalary(OpticsModel.team)
  @Benchmark def quicklensEachSalary(): OpticsModel.OTeam = QuicklensOpticsInstances.eachSalary(OpticsModel.team)
  @Benchmark def handWrittenEachSalary(): OpticsModel.OTeam = HandWrittenOpticsInstances.eachSalary(OpticsModel.team)

  @Benchmark def kindlingsAllNames(): OpticsModel.OTeam = KindlingsOpticsInstances.allNames(OpticsModel.team)
  @Benchmark def quicklensAllNames(): OpticsModel.OTeam = QuicklensOpticsInstances.allNames(OpticsModel.team)

  @Benchmark def kindlingsVectorAt(): OpticsModel.OScores = KindlingsOpticsInstances.vectorAt(OpticsModel.scores)
  @Benchmark def quicklensVectorAt(): OpticsModel.OScores = QuicklensOpticsInstances.vectorAt(OpticsModel.scores)

  @Benchmark def kindlingsMapAt(): OpticsModel.OScores = KindlingsOpticsInstances.mapAt(OpticsModel.scores)
  @Benchmark def quicklensMapAt(): OpticsModel.OScores = QuicklensOpticsInstances.mapAt(OpticsModel.scores)

  @Benchmark def kindlingsArrayIndex(): Array[OpticsModel.OStreet] =
    KindlingsOpticsInstances.arrayIndex(OpticsModel.salaries)
  @Benchmark def quicklensArrayIndex(): Array[OpticsModel.OStreet] =
    QuicklensOpticsInstances.arrayIndex(OpticsModel.salaries)

  @Benchmark def kindlingsExplicitCopy(): OpticsModel.OPoint = KindlingsOpticsInstances.explicitCopy(OpticsModel.point)
  @Benchmark def quicklensExplicitCopy(): OpticsModel.OPoint = QuicklensOpticsInstances.explicitCopy(OpticsModel.point)

  @Benchmark def kindlingsEachWhereSalary(): OpticsModel.OTeam =
    KindlingsOpticsInstances.eachWhereSalary(OpticsModel.team)
  @Benchmark def quicklensEachWhereSalary(): OpticsModel.OTeam =
    QuicklensOpticsInstances.eachWhereSalary(OpticsModel.team)
}
