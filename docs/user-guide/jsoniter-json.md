# Jsoniter JSON

Minimal JSON AST with optics and a hand-written `JsonValueCodec` for [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala). Provides a `Json` type for working with raw JSON -- parsing, constructing, navigating, and modifying -- without pulling in Circe or any of its Cats dependencies.

## Why this exists

Sometimes you need a raw JSON type -- to pass opaque JSON through your system, to manipulate it with lenses, or to bridge between jsoniter-scala and code that expects a JSON AST. The usual answer is [jsoniter-scala-circe](https://github.com/plokhotnyuk/jsoniter-scala/tree/master/jsoniter-scala-circe), which gives you `io.circe.Json` backed by jsoniter-scala's fast parser.

But `circe-core` pulls in half the Cats ecosystem (`cats-core`, `cats-kernel`, and their transitive dependencies). If you're not already using Circe or Cats, that's a lot of bytecode for a data structure you could define in a hundred lines.

`kindlings-jsoniter-json` extracts exactly what's needed: a sealed `Json` trait, precision-preserving `JsonNumber`, ordered `JsonObject`, lightweight optics, and a `JsonValueCodec[Json]` for jsoniter-scala's streaming API. Zero transitive dependencies beyond `jsoniter-scala-core`.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-jsoniter-json" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-jsoniter-json" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    ```

!!! note
    You also need `jsoniter-scala-core` as a runtime dependency:

    ```scala
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "{{ libraries.jsoniterScala }}"
    ```

## Quick start

??? example "Parsing, navigating, and modifying JSON"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterjson._
    import hearth.kindlings.jsoniterjson.codec.JsonCodec._
    import hearth.kindlings.jsoniterjson.optics._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    // Parse JSON from a string
    val json: Json = readFromString[Json]("""{"name":"Alice","age":30,"tags":["scala","fp"]}""")

    // Navigate with JsonPath
    val name = JsonPath.root.field("name").get(json)
    println(name.flatMap(_.asString))
    // expected output:
    // Some(Alice)

    // Modify nested values
    val updated = JsonPath.root.field("age").modify {
      case Json.Num(n) => Json.fromInt(n.toInt.getOrElse(0) + 1)
      case other => other
    }(json)
    println(writeToString(updated))
    // expected output:
    // {"name":"Alice","age":31,"tags":["scala","fp"]}
    ```

## JSON types

### `Json` -- the value type

A sealed trait with six cases:

| Case | Description | Example |
|------|-------------|---------|
| `Json.Null` | null value | `Json.Null` |
| `Json.Bool(value)` | boolean | `Json.True`, `Json.False` |
| `Json.Num(value)` | number (via `JsonNumber`) | `Json.fromInt(42)` |
| `Json.Str(value)` | string | `Json.fromString("hello")` |
| `Json.Arr(values)` | array (`Vector[Json]`) | `Json.arr(Json.fromInt(1), Json.fromInt(2))` |
| `Json.Obj(fields)` | object (via `JsonObject`) | `Json.obj("key" -> Json.fromString("val"))` |

Type-checking predicates: `isNull`, `isBoolean`, `isNumber`, `isString`, `isArray`, `isObject`

Safe extraction: `asBoolean`, `asNumber`, `asString`, `asArray`, `asObject` -- all return `Option`

Exhaustive fold:

```scala
json.fold(
  onNull    = "null",
  onBoolean = b => s"bool: $b",
  onNumber  = n => s"num: $n",
  onString  = s => s"str: $s",
  onArray   = vs => s"array of ${vs.size}",
  onObject  = o => s"object with ${o.size} fields"
)
```

### `JsonNumber` -- precision-preserving numbers

Numbers are stored as strings internally to avoid precision loss from `Double`/`Float` conversions. Conversion methods return `Option` to signal when the value doesn't fit:

| Method | Returns |
|--------|---------|
| `toInt` | `Option[Int]` |
| `toLong` | `Option[Long]` |
| `toDouble` | `Option[Double]` (rejects NaN/Infinity) |
| `toBigDecimal` | `Option[BigDecimal]` |
| `toBigInt` | `Option[BigInt]` |

### `JsonObject` -- ordered key-value pairs

Backed by `Vector[(String, Json)]` to preserve insertion order. Supports duplicate keys (last value wins on lookup).

| Operation | Description |
|-----------|-------------|
| `apply(key)` | `Option[Json]` -- look up a field |
| `add(key, value)` | Append a field |
| `remove(key)` | Remove all fields with the given key |
| `mapValues(f)` | Transform all values |
| `keys` / `values` / `toMap` | Standard conversions |

## Optics

Lightweight, zero-dependency optics for navigating and modifying JSON. No Monocle or Circe Optics needed.

### `JsonPath` -- chained navigation

Build paths from the root, then `get`, `set`, or `modify`:

```scala
import hearth.kindlings.jsoniterjson.optics._

JsonPath.root.field("users").index(0).field("name").get(json)     // Option[Json]
JsonPath.root.field("users").index(0).field("name").set(Json.fromString("Bob"))(json)  // Json
JsonPath.root.field("count").modify {
  case Json.Num(n) => Json.fromInt(n.toInt.getOrElse(0) + 1)
  case other => other
}(json) // Json
```

### `JsonOptic` -- composable accessors

Lower-level building blocks that `JsonPath` is built on:

| Factory | Description |
|---------|-------------|
| `JsonOptic.field(name)` | Access an object field |
| `JsonOptic.index(i)` | Access an array element |
| `JsonOptic.each` | Traverse all array elements (returns `JsonTraversal`) |

Compose with `andThen`:

```scala
val nameOfFirst = JsonOptic.field("users").andThen(JsonOptic.index(0)).andThen(JsonOptic.field("name"))
nameOfFirst.get(json) // Option[Json]
```

### `JsonTraversal` -- multi-value operations

Operates on all elements of an array:

```scala
// Get all values
JsonOptic.each.getAll(jsonArray)  // Vector[Json]

// Modify all values
JsonOptic.each.modify(transform)(jsonArray)  // Json
```

## jsoniter-scala integration

Import the codec to use `Json` with jsoniter-scala's `readFromString`/`writeToString`:

```scala
import hearth.kindlings.jsoniterjson.codec.JsonCodec._
import com.github.plokhotnyuk.jsoniter_scala.core._

val json: Json = readFromString[Json]("""{"key":"value"}""")
val str: String = writeToString(json)
```

The codec is hand-written against jsoniter-scala's streaming `JsonReader`/`JsonWriter` API, so it gets the same parsing performance as any other jsoniter-scala codec.

## Usage examples

??? example "Building JSON programmatically"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterjson._
    import hearth.kindlings.jsoniterjson.codec.JsonCodec._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    val json = Json.obj(
      "name" -> Json.fromString("Alice"),
      "age" -> Json.fromInt(30),
      "active" -> Json.True,
      "scores" -> Json.arr(Json.fromInt(95), Json.fromInt(87), Json.fromInt(92)),
      "address" -> Json.obj(
        "city" -> Json.fromString("New York"),
        "zip" -> Json.fromString("10001")
      )
    )

    println(writeToString(json))
    // expected output:
    // {"name":"Alice","age":30,"active":true,"scores":[95,87,92],"address":{"city":"New York","zip":"10001"}}
    ```

??? example "Navigating and modifying with JsonPath"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterjson._
    import hearth.kindlings.jsoniterjson.codec.JsonCodec._
    import hearth.kindlings.jsoniterjson.optics._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    val json = readFromString[Json]("""{
      "users": [
        {"name": "Alice", "role": "admin"},
        {"name": "Bob", "role": "user"}
      ]
    }""")

    // Read a nested value
    val firstRole = JsonPath.root.field("users").index(0).field("role").get(json)
    println(firstRole.flatMap(_.asString))
    // expected output:
    // Some(admin)

    // Modify a nested value
    val promoted = JsonPath.root.field("users").index(1).field("role")
      .set(Json.fromString("admin"))(json)
    val newRole = JsonPath.root.field("users").index(1).field("role").get(promoted)
    println(newRole.flatMap(_.asString))
    // expected output:
    // Some(admin)
    ```

??? example "Pattern matching with fold"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterjson._
    import hearth.kindlings.jsoniterjson.codec.JsonCodec._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    val values: Vector[Json] = Vector(
      Json.Null,
      Json.fromString("hello"),
      Json.fromInt(42),
      Json.True,
      Json.arr(Json.fromInt(1)),
      Json.obj("key" -> Json.fromString("value"))
    )

    values.foreach { json =>
      val desc = json.fold(
        onNull    = "null",
        onBoolean = b => s"bool($b)",
        onNumber  = n => s"num(${n.toInt.getOrElse(n)})",
        onString  = s => s"str($s)",
        onArray   = vs => s"arr(${vs.size} items)",
        onObject  = o => s"obj(${o.size} fields)"
      )
      println(desc)
    }
    // expected output:
    // null
    // str(hello)
    // num(42)
    // bool(true)
    // arr(1 items)
    // obj(1 fields)
    ```

??? example "Working with JsonObject"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-json:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterjson._
    import hearth.kindlings.jsoniterjson.codec.JsonCodec._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    // Build an object incrementally
    val obj = JsonObject.empty
      .add("host", Json.fromString("localhost"))
      .add("port", Json.fromInt(8080))
      .add("debug", Json.False)

    println(writeToString(Json.fromJsonObject(obj)))
    // expected output:
    // {"host":"localhost","port":8080,"debug":false}

    // Remove a field
    val production = obj.remove("debug")
    println(writeToString(Json.fromJsonObject(production)))
    // expected output:
    // {"host":"localhost","port":8080}

    // Look up a field
    println(obj("port").flatMap(_.asNumber).flatMap(_.toInt))
    // expected output:
    // Some(8080)
    ```

## Comparison with alternatives

| | kindlings-jsoniter-json | jsoniter-scala-circe | circe-core standalone |
|---|---|---|---|
| JSON AST | `hearth.kindlings.jsoniterjson.Json` | `io.circe.Json` | `io.circe.Json` |
| Parser | jsoniter-scala | jsoniter-scala | circe-jawn / circe-parser |
| Optics | Built-in (`JsonPath`, `JsonOptic`) | Circe Optics (separate dep) | Circe Optics (separate dep) |
| Transitive deps | `jsoniter-scala-core` only | `circe-core`, `cats-core`, `cats-kernel`, ... | `cats-core`, `cats-kernel`, ... |
| Cross-platform | JVM, Scala.js, Scala Native | JVM, Scala.js | JVM, Scala.js |
