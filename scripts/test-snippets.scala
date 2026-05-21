//> using scala 3.3.7
//> using dep com.kubuszok::scala-cli-md-spec:0.2.1
//> using dep org.virtuslab::scala-yaml:0.3.1
//> using jvm 17

import com.kubuszok.scalaclimdspec.*
import java.io.File
import java.nio.file.Files
import scala.collection.immutable.ListMap
import scala.util.{Try, Using}
import scala.util.chaining.*

// Kindlings-specific configuration

case class MkDocsConfig(extra: Map[String, String])
object MkDocsConfig {

  def parse(cfgFile: File): Either[String, MkDocsConfig] = {
    import org.virtuslab.yaml.*
    def decode(any: Any): Map[String, String] = any match {
      case map: Map[?, ?] =>
        map.flatMap {
          case (k, v: Map[?, ?]) => decode(v).map { case (k2, v2) => s"$k.$k2" -> v2 }
          case (k, v: List[?])   => decode(v).map { case (k2, v2) => s"$k.$k2" -> v2 }
          case (k, v)            => Map(k.toString -> v.toString)
        }.toMap
      case list: List[?] =>
        list.zipWithIndex.flatMap {
          case (i: Map[?, ?], idx) => decode(i).map { case (k, v) => s"[$idx].$k" -> v }
          case (i: List[?], idx)   => decode(i).map { case (k, v) => s"[$idx].$k" -> v }
          case (i, idx)            => Map(s"[$idx]" -> i.toString)
        }.toMap
      case _ => throw new IllegalArgumentException(s"$any is not an expected YAML")
    }
    for {
      cfgStr <- Using(io.Source.fromFile(cfgFile))(_.getLines().toList.mkString("\n")).toEither.left
        .map(_.getMessage())
      cfgRaw <- cfgStr.as[Any].left.map(_.toString)
      extra <- Try(decode(cfgRaw.asInstanceOf[Map[Any, Any]].apply("extra"))).toEither.left.map(_.getMessage)
    } yield MkDocsConfig(extra)
  }
}

class KindlingsExtendedRunner(runner: Runner)(
    kindlingsVersion: String,
    mkDocsCfg: MkDocsConfig
) extends Runner {

  private val defaultScalaVersion = "2.13.18"
  private val snapshotRepo = "https://central.sonatype.com/repository/maven-snapshots"

  private val replacePatterns = (mkDocsCfg.extra + (raw"kindlings_version\(\)" -> kindlingsVersion)).map {
    case (k, v) =>
      (raw"\{\{\s*" + k + raw"\s*\}\}") -> v
  }

  val addDefaultScala: Snippet.Content => Snippet.Content = {
    case Snippet.Content.Single(content) =>
      Snippet.Content.Single(
        if content.contains("//> using scala") then content
        else s"//> using scala $defaultScalaVersion\n$content"
      )
    case Snippet.Content.Multiple(files) =>
      Snippet.Content.Multiple(
        if files.values.exists(_.content.contains("//> using scala")) then files
        else {
          val (fileName, Snippet.Content.Single(content0)) = files.head
          val content1: Snippet.Content.Single =
            Snippet.Content.Single(s"//> using scala $defaultScalaVersion\n$content0")
          ListMap(fileName -> content1) ++ files.tail
        }
      )
  }

  val interpolateTemplates: Snippet.Content => Snippet.Content = {
    case Snippet.Content.Single(content) =>
      Snippet.Content.Single(replacePatterns.foldLeft(content) { case (s, (k, v)) => s.replaceAll(k, v) })
    case Snippet.Content.Multiple(files) =>
      Snippet.Content.Multiple(
        ListMap.from(files.mapValues(interpolateTemplates(_).asInstanceOf[Snippet.Content.Single]))
      )
  }

  val addSnapshotRepo: Snippet.Content => Snippet.Content = {
    case Snippet.Content.Single(content) =>
      Snippet.Content.Single(
        if content.contains("//> using repository") then content
        else content.replaceFirst("(//> using scala [^\n]*\n)", s"$$1//> using repository $snapshotRepo\n")
      )
    case Snippet.Content.Multiple(files) =>
      Snippet.Content.Multiple(
        if files.values.exists(_.content.contains("//> using repository")) then files
        else {
          val (fileName, Snippet.Content.Single(content0)) = files.head
          val content1: Snippet.Content.Single = Snippet.Content.Single(
            content0.replaceFirst("(//> using scala [^\n]*\n)", s"$$1//> using repository $snapshotRepo\n")
          )
          ListMap(fileName -> content1) ++ files.tail
        }
      )
  }

  export runner.{docsDir, filter, tmpDir}

  private val ignored = Map(
    // Dep-only snippets (no actual code, just show how to add the dependency)
    "avro-derivation.md#Installation[2]" -> "Dep-only snippet",
    "cats-derivation.md#Installation[3]" -> "Dep-only snippet",
    "cats-integration.md#Installation[3]" -> "Dep-only snippet",
    "circe-derivation.md#Installation[3]" -> "Dep-only snippet",
    "fast-show-pretty.md#Installation[3]" -> "Dep-only snippet",
    "index.md#Quick start[2]" -> "Dep-only snippet",
    "iron-integration.md#Installation[3]" -> "Dep-only snippet",
    "jsoniter-derivation.md#Installation[3]" -> "Dep-only snippet",
    "jsoniter-json.md#Installation[3]" -> "Dep-only snippet",
    "pureconfig-derivation.md#Installation[2]" -> "Dep-only snippet",
    "refined-integration.md#Installation[3]" -> "Dep-only snippet",
    "scalacheck-derivation.md#Installation[3]" -> "Dep-only snippet",
    "sconfig-derivation.md#Installation[3]" -> "Dep-only snippet",
    "tapir-schema-derivation.md#Installation[4]" -> "Dep-only snippet",
    "ubjson-derivation.md#Installation[3]" -> "Dep-only snippet",
    "xml-derivation.md#Installation[4]" -> "Dep-only snippet",
    "yaml-derivation.md#Installation[3]" -> "Dep-only snippet",
    // Runtime issues in script mode (compilation succeeds)
    "scalacheck-derivation.md#Usage examples[4]" -> "Recursive Arbitrary may stack overflow in script mode",
    "tapir-schema-derivation.md#Usage examples[5]" -> "Initialization order issue with implicit val in script wrapper"
  )

  extension (snippet: Snippet)
    def adjusted: Snippet =
      // interpolate templates before Default adjustments
      runner.adjusted(snippet.copy(content = interpolateTemplates(snippet.content)))

    def howToRun: Runner.Strategy =
      ignored.get(snippet.stableName).fold(runner.howToRun(snippet))(Runner.Strategy.Ignore(_))

  extension (snippets: List[Snippet])
    def adjusted: List[Snippet] = runner.adjusted(snippets).map { snippet =>
      // add default Scala but only to those snippets that are already run (adding it before would make all of them run)
      howToRun(snippet) match
        case Runner.Strategy.Ignore(_) => snippet
        case _                         => snippet.copy(content = addSnapshotRepo(addDefaultScala(snippet.content)))
    }
}

/** Usage:
  *
  * From the project root (if called from other directory, adapt path after PWD accordingly):
  *
  * on CI:
  * {{{
  * # run all tests, use artifacts published locally from current tag
  * scala-cli run scripts/test-snippets.scala -- --extra "kindlings-version=$(sbt -batch -error 'print fastShowPretty/version')" "$PWD/docs/user-guide"
  * }}}
  *
  * during development:
  * {{{
  * # sbt publish-local-for-tests
  * # fix the version to what sbt generated, fix tmp directory to something to be able to preview generated files
  * scala-cli run scripts/test-snippets.scala -- --extra "kindlings-version=0.1.x-n-g1234567-SNAPSHOT" --test-only "circe-derivation.md*" "$PWD/docs/user-guide" "/tmp/docs-snippets"
  * }}}
  */
@main def testKindlingsSnippets(args: String*): Unit = testSnippets(args.toArray) { cfg =>
  KindlingsExtendedRunner(Runner.Default(cfg))(
    kindlingsVersion = cfg
      .extra("kindlings-version")
      .trim
      .pipe("\\[([0-9]+)m".r.replaceAllIn(_, "")) // remove possible console coloring from sbt
      .pipe(raw"(?U)\s".r.replaceAllIn(_, "")) // remove possible ESC characters
      .replaceAll("\\[0J", ""), // replace this one offending thing
    mkDocsCfg = MkDocsConfig
      .parse(File(s"${cfg.docsDir}/../mkdocs.yml").getAbsoluteFile())
      .fold(s => throw Exception(s), identity)
  )
}
