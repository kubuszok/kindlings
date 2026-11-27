package hearth.kindlings.oracle.pureconfig

import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigRenderOptions, ConfigValue}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle
import pureconfig.{ConfigCursor, ConfigReader, ConfigWriter}
import pureconfig.error.{ConfigReaderFailures, ConvertFailure}

import scala.jdk.CollectionConverters.*

/** Helpers of the pureconfig oracle suites: the same input is read (or the same value written) by Kindlings' instance
  * and by upstream's, and the results are compared, failures included (reason, path and origin).
  */
trait PureConfigOracleSupport { this: MacroSuite =>

  // --- helpers -------------------------------------------------------------------------------------------------------

  /** A cursor at the root of a HOCON document (an object). */
  protected def root(hocon: String): ConfigCursor = ConfigCursor(ConfigFactory.parseString(hocon).root(), Nil)

  /** A cursor at `v` of `v = <hocon>`, to read scalars and lists. */
  protected def value(hocon: String): ConfigCursor =
    root(s"v = $hocon").asObjectCursor.flatMap(_.atKey("v")).fold(f => sys.error(f.prettyPrint()), identity)

  protected def failures(f: ConfigReaderFailures): List[String] = f.toList.map {
    case ConvertFailure(reason, origin, path) =>
      // (the origin too: upstream's suites assert it, and it tells at which cursor a failure was reported)
      val at = origin.fold("")(o => s" (${o.description})")
      s"at '$path'$at: ${reason.getClass.getSimpleName}: ${reason.description}"
    case other => other.description
  }

  protected def show(result: Either[ConfigReaderFailures, Any]): String =
    result.fold(f => failures(f).mkString("Left(", " | ", ")"), v => s"Right($v)")

  /** Reads every input with both readers; returns `input: kindlings=... upstream=...` for each one they disagree on. */
  protected def compareReads[A](kindlings: ConfigReader[A], upstream: ConfigReader[A])(
      inputs: ConfigCursor*
  )(expectedDifferences: String*): Unit = {
    val differences = inputs.toList.flatMap { cur =>
      val k = show(kindlings.from(cur))
      val u = show(upstream.from(cur))
      if (k == u) Nil
      else
        List(s"${cur.valueOpt.fold("<undefined>")(_.render(ConfigRenderOptions.concise()))}: kindlings=$k upstream=$u")
    }
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs pureconfig reads:\n${differences.mkString("\n")}"
    )
  }

  /** Every input with every result each library produced, for assertions on the results themselves. */
  protected def read[A](reader: ConfigReader[A], cur: ConfigCursor): String = show(reader.from(cur))

  protected def toScala(v: ConfigValue): Any = v match {
    case o: ConfigObject => scala.collection.immutable.TreeMap(o.asScala.toSeq.map { case (k, x) => k -> toScala(x) }*)
    case l: ConfigList   => l.asScala.toList.map(toScala)
    case other           => other.unwrapped()
  }

  protected def write[A](writer: ConfigWriter[A], a: A): Either[String, Any] =
    try Right(toScala(writer.to(a)))
    catch { case e: Exception => Left(s"${e.getClass.getSimpleName}: ${e.getMessage}") }

  /** Writes every value with both writers; returns the paths at which the written config values differ. */
  protected def compareWrites[A](kindlings: ConfigWriter[A], upstream: ConfigWriter[A])(values: A*)(
      expectedDifferences: String*
  ): Unit = {
    val differences = values.toList.flatMap { a =>
      Oracle.diff(write(kindlings, a), write(upstream, a)).map(d => s"$a: $d")
    }
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs pureconfig writes:\n${differences.mkString("\n")}"
    )
  }
}
