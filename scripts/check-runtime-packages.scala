//> using scala 3.3.8
//> using jvm 17

// Checks the FAQ's tables that sort the packages of the published modules into "used at runtime", "used at runtime,
// but probably only in tests" and "used only during macro expansion":
//
//  1. every package of every published module is in the tables, and the tables list no package that no longer exists,
//  2. (when compiled test classes are passed) the code that macros expanded into, inside test classes, refers only to
//     packages that the tables say are needed at runtime - otherwise users who remove the compile-time packages from
//     their fat JARs, as the FAQ suggests, would get a NoClassDefFoundError.
//
// Adapted from Hearth's `scripts/check-runtime-packages.scala` (kubuszok/hearth#414). Differences:
//  - the tables may use `*` for exactly one package segment (e.g. `hearth.kindlings.*.internal.compiletime`), so they
//    list a handful of patterns instead of every module's packages; when several entries match a package, the one
//    with more segments wins (then the one with fewer `*`),
//  - only references to `hearth.kindlings` are checked - Hearth's own packages are checked by Hearth,
//  - `--test-classes-glob` accepts a glob, because sbt 2 puts every module's classes in its own directory.
//
// Usage (from the repository root, after compiling the tests):
//
//   scala-cli run scripts/check-runtime-packages.scala -- \
//     --test-classes-glob 'target/out/jvm/scala-3.*/*/test-classes'
//
// `--test-classes` / `--test-classes-glob` are checked; `--own-classes` are additional classes that share packages with
// the library but are not part of it (fixtures). Test classes are always treated as own classes. All can be repeated.
// Without them, only check 1 runs.

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, FileSystems, Path, Paths}
import scala.jdk.CollectionConverters.*

// Configuration

val faqFile = Paths.get("docs/user-guide/faq.md")
val sectionHeading = "## Which parts of Kindlings end up in my application at runtime?"

sealed trait Group
object Group {
  case object Runtime extends Group
  case object TestOnly extends Group
  case object CompileTimeOnly extends Group
}
val groupHeadings: List[(String, Group)] = List(
  "### Used at runtime: keep" -> Group.Runtime,
  "### Used at runtime, but probably only in tests" -> Group.TestOnly,
  "### Used only during macro expansion" -> Group.CompileTimeOnly
)

val rootPackage = "hearth.kindlings"

// Test, benchmark and example modules are not published, so they never end up in users' fat JARs.
val notModules: Set[String] = Set("docs", "project", "scripts", "target")
def isPublished(moduleName: String): Boolean =
  !notModules(moduleName) && !moduleName.contains("tests") && !moduleName.contains("benchmarks") &&
    !moduleName.contains("examples")

// Whole artifacts whose own tests call them directly at runtime, so references to them from test classes do not come
// from expanded macros.
val artifactsIgnoredInTestClasses: Set[String] = Set.empty

// Test classes that call compile-time-only APIs directly, rather than through a macro: unit tests of the macro
// internals, and code that refers to an opt-in marker value on purpose (to keep `-Wunused` quiet).
val specsCallingCompileTimeApisDirectly: Set[String] = Set(
  "hearth.kindlings.derivation.compiletime.DerivationErrorRenderingSpec",
  "hearth.kindlings.derivation.compiletime.DerivationPolicySpec",
  "hearth.kindlings.derivation.compiletime.DerivationTimeoutSpec",
  "hearth.kindlings.derivation.compiletime.StableChildrenOrderSpec",
  "hearth.kindlings.derivation.compiletime.UserInstancesSpec",
  "hearth.kindlings.derivation.compiletime.ValueTypeIntegrationHintsSpec",
  "hearth.kindlings.di.WiringGraphSpec",
  "hearth.kindlings.tapirschemaderivation.internal.compiletime.PreferSchemaConfigHintSpec",
  "hearth.kindlings.policytest.viaimport.ImportedInstances"
)

// Published packages

val publishedModules: List[Path] = Files
  .list(Paths.get("."))
  .iterator()
  .asScala
  .filter(dir => Files.isDirectory(dir.resolve("src/main")))
  .filter(dir => isPublished(dir.getFileName.toString))
  .toList
  .sortBy(_.getFileName.toString)

// Sources live in `src/main/scala`, `src/main/scala-2`, `src/main/scalajvm`, etc., all following the
// directory-per-package convention, so the path relative to such a root is the package name.
def packagesOf(module: Path): Set[String] = {
  val main = module.resolve("src/main")
  Files
    .list(main)
    .iterator()
    .asScala
    .filter(root => root.getFileName.toString.startsWith("scala"))
    .flatMap { sourceRoot =>
      Files
        .walk(sourceRoot)
        .iterator()
        .asScala
        .filter(_.toString.endsWith(".scala"))
        .map(file => sourceRoot.relativize(file.getParent).iterator().asScala.mkString("."))
    }
    .filter(_.nonEmpty)
    .toSet
}

val packagesByModule: Map[String, Set[String]] =
  publishedModules.map(module => module.getFileName.toString -> packagesOf(module)).toMap
val allPackages = packagesByModule.values.flatten.toSet

// The FAQ tables

val faqSection: List[String] = {
  val lines = Files.readAllLines(faqFile).asScala.toList
  val fromHeading = lines.dropWhile(line => !line.startsWith(sectionHeading))
  if fromHeading.isEmpty then {
    System.err.println(s"Cannot find the section '$sectionHeading' in $faqFile")
    sys.exit(1)
  }
  fromHeading.head :: fromHeading.tail.takeWhile(line => !line.startsWith("## "))
}

val backticked = """`([^`]+)`""".r
val packageName = ("""\Q""" + rootPackage + """\E(\.([a-z_][a-z0-9_]*|\*))*""").r

// Only the first column lists packages: other columns may mention packages in passing.
def packagesInFirstColumn(line: String): List[String] =
  if line.startsWith("|") && !line.startsWith("|--") then {
    val firstCell = line.split('|').lift(1).getOrElse("")
    backticked.findAllMatchIn(firstCell).map(_.group(1)).filter(packageName.matches).toList
  } else Nil

val listedPackages: Map[String, Group] = {
  val (_, listed) = faqSection.foldLeft((Option.empty[Group], Map.empty[String, Group])) {
    case ((_, listed), line) if line.startsWith("### ") =>
      (groupHeadings.collectFirst { case (heading, group) if line.startsWith(heading) => group }, listed)
    case ((Some(group), listed), line) =>
      (Some(group), listed ++ packagesInFirstColumn(line).map(_ -> group))
    case ((None, listed), _) =>
      (None, listed)
  }
  listed
}

// A listed entry covers the package it names and its subpackages; `*` stands for exactly one segment.
def covers(listed: String, pkg: String): Boolean = {
  val pattern = listed.split('.')
  val segments = pkg.split('.')
  segments.length >= pattern.length && pattern.zip(segments).forall { case (p, s) => p == "*" || p == s }
}

def groupOf(pkg: String): Option[Group] =
  listedPackages.toList
    .collect { case (listed, group) if covers(listed, pkg) => listed -> group }
    .sortBy { case (listed, _) =>
      val pattern = listed.split('.')
      (-pattern.length, pattern.count(_ == "*"))
    }
    .headOption
    .map(_._2)

// Check 1: the tables cover the published packages

val unclassified = for {
  (module, packages) <- packagesByModule.toList.sortBy(_._1)
  pkg <- packages.toList.sorted
  if pkg == rootPackage || pkg.startsWith(rootPackage + ".")
  if groupOf(pkg).isEmpty
} yield s"$pkg (module $module)"

val stale = listedPackages.keys.toList.sorted.filterNot(listed => allPackages.exists(covers(listed, _)))

// Check 2: expanded macros refer only to runtime packages

def classFilesIn(dir: Path): List[Path] =
  Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".class")).toList

def internalName(dir: Path, classFile: Path): String =
  dir.relativize(classFile).iterator().asScala.mkString("/").stripSuffix(".class")

def topLevelClassName(internalName: String): String =
  internalName.takeWhile(_ != '$').replace('/', '.')

def packageOfInternalName(internalName: String): String =
  internalName.split('/').dropRight(1).mkString(".")

val rootInternalPrefix = rootPackage.replace('.', '/') + "/"
val descriptorReference = ("""L(\Q""" + rootInternalPrefix + """\E[^;<>]+)[;<]""").r

// The constant pool lists every class the code uses: in `Class` entries (instantiation, calls, casts) and in
// descriptors and generic signatures (field, parameter and result types).
def referencedClasses(classFile: Path): Set[String] = {
  val in = new DataInputStream(new ByteArrayInputStream(Files.readAllBytes(classFile)))
  in.skipBytes(8) // magic, minor and major version
  val count = in.readUnsignedShort()
  val utf8 = Array.ofDim[String](count)
  val classNameIndices = List.newBuilder[Int]
  var index = 1
  while index < count do {
    in.readUnsignedByte() match {
      case 1                                  => utf8(index) = in.readUTF()
      case 7                                  => classNameIndices += in.readUnsignedShort()
      case 8 | 16 | 19 | 20                   => in.skipBytes(2)
      case 15                                 => in.skipBytes(3)
      case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => in.skipBytes(4)
      case 5 | 6                              => in.skipBytes(8); index += 1 // longs and doubles take two slots
      case tag => throw new IllegalStateException(s"Unknown constant pool tag $tag in $classFile")
    }
    index += 1
  }
  val fromClassEntries = classNameIndices.result().map(utf8(_)).filter(_.startsWith(rootInternalPrefix))
  val fromDescriptors = utf8.iterator.filter(_ != null).flatMap(descriptorReference.findAllMatchIn(_).map(_.group(1)))
  (fromClassEntries ++ fromDescriptors).toSet
}

def valuesAfter(flag: String, args: List[String]): List[String] = args
  .sliding(2)
  .collect { case List(`flag`, value) => value }
  .toList

// `dir/*/x` style globs, relative to the working directory.
def expandGlob(glob: String): List[Path] = {
  val matcher = FileSystems.getDefault.getPathMatcher("glob:" + glob)
  val fixedPrefix = Paths.get(glob.split('/').takeWhile(segment => !segment.exists("*?[{".contains(_))).mkString("/"))
  if !Files.isDirectory(fixedPrefix) then Nil
  else
    Files
      .walk(fixedPrefix, glob.count(_ == '/') + 1)
      .iterator()
      .asScala
      .filter(path => Files.isDirectory(path) && matcher.matches(path))
      .toList
      .sortBy(_.toString)
}

def isOnlyInIgnoredArtifacts(pkg: String): Boolean = {
  val modules = packagesByModule.collect { case (module, packages) if packages(pkg) => module }
  modules.nonEmpty && modules.forall(artifactsIgnoredInTestClasses)
}

def referencesToCompileTimePackages(testClassDirs: List[Path], ownClassDirs: List[Path]): List[String] = {
  val ownClasses = (testClassDirs ++ ownClassDirs).flatMap(dir => classFilesIn(dir).map(internalName(dir, _))).toSet
  val references = for {
    dir <- testClassDirs
    classFile <- classFilesIn(dir)
    from = topLevelClassName(internalName(dir, classFile))
    if !specsCallingCompileTimeApisDirectly(from)
    referenced <- referencedClasses(classFile)
    if !ownClasses(referenced)
    pkg = packageOfInternalName(referenced)
    if !isOnlyInIgnoredArtifacts(pkg)
    group = groupOf(pkg)
    if !group.contains(Group.Runtime) && !group.contains(Group.TestOnly)
  } yield (referenced.replace('/', '.'), group, from)
  references
    .groupBy { case (referenced, group, _) => (referenced, group) }
    .toList
    .sortBy(_._1._1)
    .map { case ((referenced, group), usages) =>
      val groupName = group.fold("not in the tables")(_ => "used only during macro expansion")
      s"$referenced ($groupName), referred to from: ${usages.map(_._3).distinct.sorted.mkString(", ")}"
    }
}

@main def checkRuntimePackages(args: String*): Unit = {
  val globs = valuesAfter("--test-classes-glob", args.toList)
  val globbed = globs.flatMap(expandGlob)
  val testClassDirs = valuesAfter("--test-classes", args.toList).map(Paths.get(_)) ++ globbed
  val ownClassDirs = valuesAfter("--own-classes", args.toList).map(Paths.get(_))
  val missingDirs = (testClassDirs ++ ownClassDirs).filterNot(Files.isDirectory(_))
  if missingDirs.nonEmpty || (globs.nonEmpty && globbed.isEmpty) then {
    System.err.println(
      s"Not a directory: ${(missingDirs ++ globs.filter(expandGlob(_).isEmpty)).mkString(", ")} - compile the tests first"
    )
    sys.exit(1)
  }

  val problems = List(
    "Packages of published modules that the FAQ does not classify as runtime / test-only / compile-time-only" -> unclassified,
    "Packages listed in the FAQ that no published module contains" -> stale,
    "Classes that code expanded by macros refers to, but that the FAQ does not list as needed at runtime" ->
      referencesToCompileTimePackages(testClassDirs, ownClassDirs)
  ).filter(_._2.nonEmpty)

  if problems.isEmpty then {
    println(
      s"All ${allPackages.size} packages of ${publishedModules.size} published modules are classified in $faqFile"
    )
    if testClassDirs.nonEmpty then
      println(s"Classes in ${testClassDirs.size} test-classes directories refer only to packages needed at runtime")
  } else {
    problems.foreach { case (title, entries) =>
      System.err.println(s"$title:")
      entries.foreach(entry => System.err.println(s"  - $entry"))
    }
    System.err.println(s"Update the tables in the section '$sectionHeading' of $faqFile")
    sys.exit(1)
  }
}
