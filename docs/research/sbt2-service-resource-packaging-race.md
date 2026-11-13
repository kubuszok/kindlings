# sbt 2: `packageBin` races `copyResources` on generated `META-INF/services` files

**Fix location**: `project/ServiceResourceGenPlugin.scala` (the `copyResources` call moves from the `Compile / exportedProducts` override into a `Compile / products` override)
**sbt**: 2.0.12 (sources: `org.scala-sbt:main_3:2.0.12`, `util-cache_3:2.0.12`, `actions_3:2.0.12`, `io_3:1.13.6`)
**Date**: 2026-11-13

## Symptom

Intermittently, in a different module each time (CI docs-snippet job running `publish-local-for-tests`, local
`integrationTests2_13/testFull` after `cleanFull`, all platforms and Scala versions):

```
[error] file referenced by the build does not exist: …/kindlings-jsoniter-derivation/classes/META-INF/services/hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigExtension.<8 hex>.tmp
[error] (jsoniterDerivation / Compile / packageBin / packageConfiguration) file referenced by the build does not exist: …
```

The failing path is always `<module>/classes/META-INF/services/<trait FQCN>.<8 hex>.tmp`, and the module is
always one that sets `macroExtensionTraits` (circe/jsoniter derivation, iron/refined/neotype/newtype/cats
integration). A re-run usually passes.

## Root cause

Two sbt tasks of the same module, with no ordering between them, touch `classDirectory` in the same command:

1. **The writer: `copyResources`, forced by our plugin.** `ServiceResourceGenPlugin` overrode

   ```scala
   Compile / exportedProducts := Def.uncached {
     (Compile / copyResources).value
     (Compile / exportedProducts).value
   }
   ```

   `copyResourcesTask` (`sbt/Defaults.scala:2639`) calls `Sync.sync` (`sbt/Sync.scala:65`), which copies every
   out-of-date resource into `classDirectory` with `Sync.copy` → `IO.copyFile` (`sbt/io/IO.scala:1149`) →
   `IO.writeFile` (`IO.scala:483`) → `IO.writeStaged` (`IO.scala:535`). `writeStaged` writes into a sibling
   staging file named

   ```scala
   new File(parent, s"${name}.${UUID.randomUUID().toString().take(8)}.tmp")   // IO.scala:547
   ```

   and then atomically moves it onto the target. That is exactly the `<name>.<8 hex>.tmp` in the error. The
   copy is out of date (so it really writes) after every `clean`/`cleanFull`, and whenever the generated SPI
   file changes.

2. **The reader: `packageBin / mappings`.** In sbt 2 `products` no longer depends on `copyResources`:

   ```scala
   def makeProducts = Def.task {                 // Defaults.scala:4497
     compile.value; resources.value
     backendDir.toFile() :: resourceDirs.toList.filter(_.exists())
   }
   ```

   (in sbt 1 it ran `copyResources` and returned only `classDirectory`). `packageBinMappings`
   (`Defaults.scala:1749`) lists `Path.allSubpaths` of every product directory, keeps `isFile` entries, drops
   class-directory copies whose relative path also exists in a resource directory, and turns each into a
   `HashedVirtualFileRef`. A staging `.tmp` has a unique name, so it survives the de-duplication. Then the
   cached `packageConfiguration`/`packageBin` (`Defaults.scala:1958-1980`) hash their inputs
   (`ActionCache.mkInput`, `sbt/util/ActionCache.scala:304`, which hashes each mapping's content via
   `PathHashWriters`). By then `writeStaged` has renamed the `.tmp` away, the hash throws
   `NoSuchFileException`, and `mkInput` rethrows it as
   `"file referenced by the build does not exist: $path"` (`ActionCache.scala:318`).

3. **Why they run concurrently.** `exportJars :== true` and `trackInternalDependencies :== TrackAlways` are the
   sbt 2 defaults (`Defaults.scala:248-251`), so `exportedProducts` is
   `trackedJarProductsImplTask` (`sbt/internal/ClasspathImpl.scala:129`), i.e. `packageBin.value`. With the
   override, `exportedProducts` therefore had two independent dependencies, `copyResources` and `packageBin`,
   both of which become runnable as soon as `compile` and `resources` finish. Any command that evaluates a
   module's `exportedProducts` while its copy is out of date (a downstream `dependsOn` module compiling, a
   `Test` classpath, `publishLocal` of everything) can list `classes/META-INF/services` while `copyResources`
   is mid-copy. The window is microseconds for an 81-byte SPI file (the staging file must exist both at
   `allSubpaths` and at the `isFile` check, and be gone before hashing), hence "rare, random module".

Refuted candidates:

* **The sbt 2 build cache restoring outputs into `classes`.** Restoring the `classes` dir zip
  (`DiskActionCacheStore.syncFile` / `unpackageDirZip`, `sbt/util/ActionCacheStore.scala:341-466`) writes with
  `Files.copy` / `Files.createSymbolicLink` directly onto the final name, and packing it
  (`ActionCache.installPackagedZip`) stages `classes.sbtdir.zip.<random>.sbtdir.zip.tmp` next to the zip.
  Neither creates `<name>.<8 hex>.tmp` inside `classes/META-INF/services`, and both run inside `compile`, which
  both racers depend on.
* **`Def.uncached` evaluating the copy twice.** sbt evaluates each task once per command; `Def.uncached` only
  disables the result cache. One copy is enough to race.
* **The `publish-local-for-tests` alias (sbt-kubuszok) or the Justfile's second `sbt -batch`.** The alias only
  publishes many modules in one command (lots of parallelism, a good trigger, not a cause), and the
  `sbt -batch 'print …/version'` call runs after it finishes. The failure also happens with a plain
  `integrationTests2_13/testFull`.

For sbt's own classpaths the copy is not needed at all: with `exportJars := true` downstream modules never see
`classDirectory` (they get the jar), and the jar's mappings include `resourceManaged` directly, because
`resourceDirectories = unmanagedResourceDirectories ++ managedResourceDirectories` (`Defaults.scala:652-656`)
and `managedResourceDirectories = Seq(resourceManaged)`. With `exportJars := false` (TrackAlways) the exported
classpath is `products`, which also contains `resourceManaged`. The one consumer that does need the copy is the
IDE: BSP's `scalacOptions` classpath for internal dependencies is each dependency's `classDirectory`
(`sbt/internal/server/BuildServerProtocol.scala:847-858`), not its jar or `resourceManaged`, so Metals'
presentation compiler only sees a sibling module's SPI files if they were copied into `classes/`.

### Is this an sbt bug?

Partly. sbt 2 still defines `copyResources` writing into `classDirectory`, and `packageBinMappings` even
de-duplicates resource copies found there, so it expects both to coexist, but nothing orders them and the
listing does not ignore staging files. Nothing in sbt 2.0.12 itself calls `copyResources` any more (only a
`// TODO: copyResources.value` in `makePickleProducts`), so a stock build does not hit it. Upstream options:
make `packageBinMappings` depend on `copyResources`, skip `IO.writeStaged` staging files when listing, or
tolerate files that vanish between listing and hashing. Any build that forces `copyResources` (as sbt 1
plugins commonly did) is exposed until then.

## The fix

Keep the copy into `classDirectory` (for the IDE), but run it **inside `Compile / products`** instead of as a
sibling of `packageBin`:

```scala
Compile / resourceGenerators += generateServiceFile.taskValue,
Compile / products := Def.uncached {
  (Compile / copyResources).value
  (Compile / products).value
}
```

`Def.uncached` makes sure the body is never skipped as a cache hit. `copyResources` runs anyway, because sbt
evaluates a task's `.value` dependencies before its body, but this keeps the override a plain passthrough.

### Why this is race-free (audit of sbt 2.0.12)

Every task that lists `classDirectory` or reads its files is now ordered after the copy, or cannot see a staging
file:

| reader | reads | ordered after the copy? |
|---|---|---|
| `packageBin / mappings` (`packageBinMappings`, `Defaults.scala:1749-1753`), hence `packageConfiguration`, `packageBin`, the jar `exportedProducts` (`ClasspathImpl.scala:129-158`), `publishLocal` | `products.value` | yes |
| `exportedProducts` with `exportJars := false`, TrackAlways (`ClasspathImpl.scala:108-113`) | `products.value` | yes |
| `exportedProductsIfMissing` (`ClasspathImpl.scala:114-126`) | globs only `*.class`; then either `products.value` or the bare directory path | yes, or reads no file contents; staging names never match `*.class` |
| `pickleProducts` (`Defaults.scala:4506-4517`), `run`/`console` classpath copies (`RunUtil.scala:184-365`, `Compiler.scala:375`) | `products.value` | yes |
| `tastyFiles` (`Defaults.scala:1003-1012`) | lists only `*.tasty` | not needed; staging names never match |
| `compile` (zinc output, packing `classes.sbtdir.zip`, cache restore) | the whole `classes` dir | `copyResources` depends on `compile` (via `resources` → `generateServiceFile`), so the module's compile is done before the copy starts |

The only caller of `copyResources` is this `products` override. In sbt 2.0.12 the key is defined at
`Defaults.scala:1167` and otherwise only appears in a `// TODO: copyResources.value` comment, so nothing else can
start the copy as an unordered sibling.

Side effects to be aware of (not races):

* When `compile` restores `classes` from the sbt 2 cache, `unpackageDirZip` deletes files missing from the
  cached manifest, which can include the copies. The next `products` re-copies them, because `Sync` treats a
  missing target as out of date. A copy present at compile time also ends up in `classes.sbtdir.zip`, which is
  harmless.
* The copies appear whenever something evaluates `products`: `packageBin`, `exportedProducts` (so any compile of
  a dependent module, including a BSP/Metals compile), `test`, `publishLocal`. A bare `<module>/compile` does
  not create them. The old `exportedProducts` override behaved the same way.

### What keeps working

* **Downstream sbt classpaths**: dependent modules get the jar, built from `products`, which includes
  `resourceManaged` (and the class-dir copies are de-duplicated by `packageBinMappings`).
* **Published jars** contain `META-INF/services/*`.
* **IDE**: `classes/META-INF/services/*` exists for BSP's `classDirectory`-based classpath.
* **Incremental / cache**: the generator writes only when the provider list changes. `Sync` copies only when
  its target is missing or out of date.

### Alternative: deletion only

Removing the `exportedProducts` override and **not** adding the `products` one also fixes the race: nothing
writes into `classDirectory` after `compile`. sbt's own classpaths and published jars are still correct.

The trade-off is the IDE. `classes/` never contains the SPI files, so Metals' presentation compiler, whose
classpath for sibling modules is their `classDirectory` via BSP, does not see sibling macro extensions. You
notice it as a macro in a dependent module that needs an extension (e.g. Iron/refined value types in
`integrationTests`, JSON-schema configs from circe/jsoniter in `tapirSchemaDerivation`) failing or deriving
differently **only in the editor** (hover, inline errors from the presentation compiler), while `sbt compile`
and BSP compile diagnostics are clean. Use deletion only in repos where nothing needs IDE-time ServiceLoader
discovery from sibling modules.

## How to verify

The race is too rare to reproduce reliably with 81-byte SPI files, so widen the window through the same code
path: give `copyResources` big files to copy, and delay the `packageBin` listing a little so it lands inside
the copy. In one sbt server, for a few `macroExtensionTraits` modules (`ironIntegration jsoniterDerivation
catsIntegration2_13 newtypeIntegration`):

```
session clear-all
set LocalProject("<m>") / Compile / unmanagedResourceDirectories += file("<dir with META-INF/stress/big{1..8}.bin, 16 MiB each>")
set LocalProject("<m>") / Compile / products := Def.uncached { val p = (LocalProject("<m>") / Compile / products).value; Thread.sleep(40L); p }
```

then loop N times:

1. Delete the `target/out/jvm/*/*/classes/META-INF/*` files, so the copy is out of date.
2. Change one small file in the extra resource dir, which forces a new jar.
3. Run `<m>/Compile/exportedProducts` for all 12 `macroExtensionTraits` modules in one `sbt --client` command.

Count the runs that print `file referenced by the build does not exist`.

| variant | iterations | failures | `classes/META-INF/services/*` |
|---|---|---|---|
| original (copy forced from `exportedProducts`) | 10 | 10, e.g. `kindlings-jsoniter-derivation/classes/META-INF/stress/big1.bin.c796cecd.tmp` from `jsoniterDerivation / Compile / packageBin / packageConfiguration` | present |
| deletion only | 30 | 0 | absent |
| **copy ordered inside `products` (chosen)** | 40 | 0 | present |

With the ordered copy, the injected sleep runs after `copyResources` has finished. The listing cannot overlap
the copy however the delays are tuned, so the result is structural, not timing luck. Without the sleep, the
16 MiB setup failed 1 in 25 iterations with the original override, the same order as CI.

Regression (chosen fix): run `cleanFull`, then `testFull` of `integrationTests`, `integrationTests2_13`,
`tapirSchemaDerivation` and `tapirSchemaDerivation2_13`. They compile against the ServiceLoader-discovered
providers (e.g. the Iron*Spec suites). Then run `publish-local-for-tests`, and
`unzip -l ~/.ivy2/local/com.kubuszok/kindlings-iron-integration_3/<ver>/jars/*.jar` shows
`META-INF/services/hearth.std.StandardMacroExtension`.

## Porting to other repositories

1. Check the sbt version (`project/build.properties`). Only sbt 2.x is affected. On sbt 1.x, `products` itself
   runs `copyResources` before packaging, so no change is needed there. Do not add the `products` override on
   sbt 1, because it would only duplicate what sbt 1 already does.
2. Search the build: `grep -rn copyResources project/ *.sbt`. Any task other than `products` that forces it
   (`exportedProducts`, `fullClasspath`, `compile`, ...) races packaging. In this setup it is the
   `Compile / exportedProducts := Def.uncached { (Compile / copyResources).value; ... }` block of
   `ServiceResourceGenPlugin`. Delete it.
3. Keep the generator wired as `Compile / resourceGenerators += <task>.taskValue`, writing into
   `Compile / resourceManaged`.
4. If the repo's developers use Metals/BSP and dependent modules load sibling extensions via ServiceLoader at
   compile time, add the `Compile / products := Def.uncached { (Compile / copyResources).value;
   (Compile / products).value }` override. Otherwise deletion alone is enough; see "Alternative: deletion only".
5. Check that the repo did not change the defaults the fix relies on: `exportJars` (`true` in sbt 2) and
   `trackInternalDependencies`/`exportToInternal` (`TrackAlways`). With `TrackIfMissing`/`NoTracking`, the
   exported classpath is `productDirectories` (`classDirectory` only) without evaluating `products`, so neither the
   copy nor `resourceManaged` would be on it.
6. Verify:
   - run `cleanFull`, then compile/test a module that `dependsOn` an SPI-providing module and relies on the provider;
   - run `publishLocal` and `unzip -l` the jar for `META-INF/services/`;
   - with the `products` override, `find target -path '*/classes/META-INF/services/*'` lists the copies after a
     package/test (without it, the result is empty).
   Optionally run the stress loop above.

Repo-specific: the `macroExtensionTraits` values and which modules set them (`build.sbt`), and the
compile-analysis parsing in `generateServiceFile`. As of this writing, `hearth` carries the same plugin but on
sbt 1.13.1 and without the `exportedProducts` override, so it is not affected.
