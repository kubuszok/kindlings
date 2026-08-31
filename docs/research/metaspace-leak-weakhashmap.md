# Metaspace / ClassLoader leak in Hearth's `platformSpecificServiceLoader`

**Issue**: [kubuszok/kindlings#199](https://github.com/kubuszok/kindlings/issues/199)  
**Root cause location**: `hearth/hearth/src/main/scala/hearth/loader.scala`, `platformSpecificServiceLoader` object  
**Date**: 2026-08-31

## Summary

Repeated `clean;compile` cycles in sbt cause monotonic metaspace growth (~28 MiB per cycle)
because old compiler `ClassLoader` instances are never garbage collected. The leak is a classic
`WeakHashMap` value→key retention bug in Hearth's service loader cache.

## Mechanism

`platformSpecificServiceLoader` (line 14 of `loader.scala`) is a JVM-level singleton `object`
that survives across compilation runs. It contains two caches:

```scala
private val serviceLoadersByClassLoader =
  new java.util.WeakHashMap[ClassLoader, HashMap[String, Tried[ServiceLoader[?]]]]()

private val servicesByClassLoader =
  new java.util.WeakHashMap[ClassLoader, HashMap[String, Tried[Any]]]()
```

Both use `WeakHashMap` keyed by `ClassLoader`, intending to allow the classloader (and its cache
entry) to be GC'd when the compiler discards it after a compilation cycle.

**However, the cached values hold strong references back to the key ClassLoader, preventing
collection:**

1. **`serviceLoadersByClassLoader`**: Values are `ServiceLoader` instances. `ServiceLoader`
   internally stores the `ClassLoader` it was created with (`ServiceLoader.load(clazz,
   classLoader)` stores `classLoader` in a field). This creates: value (`ServiceLoader`) →
   field (`classLoader`) → key (`ClassLoader`).

2. **`servicesByClassLoader`**: Values are instantiated extension singletons (e.g.,
   `IsCollectionProviderForScalaCollection`). Every `MacroExtension` subclass stores
   `private val Macro = classTag[Macro].runtimeClass.asInstanceOf[Class[Macro]]`
   (`MacroExtension.scala:38`). This `Class` object holds a strong reference to the
   `ClassLoader` that loaded it — the same classloader used as the `WeakHashMap` key.
   Chain: value (extension singleton) → field (`Macro: Class[_]`) → `ClassLoader` → key.

This is the textbook `WeakHashMap` anti-pattern: the value holds a strong ref to the key,
preventing the `WeakReference` from ever being enqueued.

## Why it manifests with Kindlings and not with Circe

Kindlings macros call `Environment.loadStandardExtensions()` →
`loadMacroExtensions[StandardMacroExtension]` → `platformSpecificServiceLoader.load(...)` every
expansion, populating both caches. Circe's derivation macros don't use Hearth's
extension/ServiceLoader system at all.

## Growth pattern

Each `clean;compile` cycle:
1. sbt creates a new macro classloader for the compiler run
2. Kindlings macros load extensions via `platformSpecificServiceLoader`, adding cache entries
   keyed by the new classloader
3. After compilation, sbt discards the old classloader — but the `WeakHashMap` values hold
   strong refs back to it via `Class` → `ClassLoader`
4. The old classloader, its `Class` objects, and all metaspace it loaded are retained

The user observed ~28 MiB growth per cycle, reaching 332 MiB after 3 cycles from a 182 MiB
baseline. JFR showed a dozen live `ScalaClassLoader$URLClassLoader` instances at shutdown,
retaining 141 MB of metaspace blocks.

## Reproduction

```bash
# In a project using kindlings auto-derivation for several types:
sbt --client "; clean; sub/compile; clean; sub/compile; clean; sub/compile"

# With JFR recording:
# -XX:StartFlightRecording=dumponexit=true,filename=dump.jfr
# Then: jfr print --events jdk.ClassLoaderStatistics dump.jfr
# Key metric: growing metaspace + retained URLClassLoader count
```

## Fix options (in Hearth)

**Option A — Purge stale entries on classloader change** (simplest):
In `getServiceLoader`/`getService`, detect that the current `classLoader` differs from the
previous one and explicitly `clear()` both maps (or remove all entries whose key ≠ current
classloader). Stale entries can never be reused anyway.

**Option B — Don't cache extension instances globally**:
Remove the `servicesByClassLoader` cache entirely. The `LoadStandardExtensionsOnce` pattern
in kindlings already guards against per-expansion duplicates. Reload from `ServiceLoader` each
compilation run. ServiceLoader cost is negligible vs. compilation time.

**Option C — Replace `WeakHashMap` with `ClassValue<T>`**:
`java.lang.ClassValue<T>` is GC-safe by design — the JDK manages lifecycle tied to the class.
Would need restructuring to key on a `Class` from the classloader rather than the `ClassLoader`
directly.

**Option D — Weak values too**:
Wrap cached values in `WeakReference` and re-create on miss. But this could cause excessive
ServiceLoader re-loads if the GC is aggressive.

Option A is recommended: minimal change, correct, no performance regression.
