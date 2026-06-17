# Tapir OpenAPI Jsoniter

Serialize the OpenAPI document that [tapir](https://tapir.softwaremill.com/) generates from your endpoints to JSON
using [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala) — **without pulling in Circe**.

## Why this exists

tapir turns a list of endpoints into an `sttp.apispec.openapi.OpenAPI` value via `tapir-openapi-docs`. To then write
that document as JSON, the usual route is `sttp-apispec`'s `openapi-circe` (often through `tapir-openapi-circe`), which
drags in `circe-core` and half of the Cats ecosystem just to render a static document.

`kindlings-tapir-openapi-jsoniter` replaces that last step. It provides hand-written, **Circe-free** jsoniter-scala
codecs for the whole `sttp-apispec` OpenAPI + JSON-Schema model (byte-for-byte compatible with `openapi-circe` — verified
in the test suite), plus a thin [`TapirOpenApi`](#the-tapir-bridge) bridge that goes straight from endpoints to JSON.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-tapir-openapi-jsoniter" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
    ```

!!! note
    The `TapirOpenApi` bridge depends on `tapir-openapi-docs`, which is published for **JVM and Scala.js only** (not
    Scala Native). The underlying jsoniter codecs (`OpenApiJsoniter`) are cross-platform, including Native — so on Native
    you can still serialize an `sttp.apispec.openapi.OpenAPI` you built another way.

## The tapir bridge

`TapirOpenApi` drives tapir's `OpenAPIDocsInterpreter` and serializes the result with this module's codec — one call, no
Circe on the classpath:

```scala
//> using scala {{ scala.2_13 }}
//> using dep com.kubuszok::kindlings-tapir-openapi-jsoniter:{{ kindlings_version() }}
//> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

import hearth.kindlings.tapiropenapijsoniter._
import sttp.apispec.openapi.Info
import sttp.tapir._

// any tapir endpoint(s):
val health = endpoint.get.in("health").out(stringBody)

// endpoints -> OpenAPI document -> JSON, serialized by jsoniter (not Circe):
val json: String = TapirOpenApi.endpointToJson(health, Info("Health API", "1.0"))

println(json.contains("\"title\":\"Health API\""))
// expected output:
// true
```

`TapirOpenApi` offers:

| Method                                       | Result                                                         |
|----------------------------------------------|----------------------------------------------------------------|
| `toOpenAPI(endpoint(s), info)`               | build the `OpenAPI` model (delegates to tapir's interpreter)   |
| `toOpenAPI(endpoint(s), title, version)`     | same, given a title and version                                |
| `toJson(doc)`                                | serialize an `OpenAPI` to JSON (OpenAPI 3.1)                    |
| `toJson30(doc)`                              | serialize using the OpenAPI 3.0.3 translation                  |
| `endpointToJson(endpoint, info)`             | one call: a single endpoint → JSON                             |
| `endpointsToJson(endpoints, info)`           | one call: several endpoints → JSON                             |
| `withOptions(opts)`                          | bind a custom `OpenAPIDocsOptions` for the interpreter         |

## Serializing an OpenAPI model directly

If you already have an `sttp.apispec.openapi.OpenAPI` (or a `Schema`, `SecurityScheme`, …) — built by tapir, parsed from
a file, or assembled by hand — use the codecs from `OpenApiJsoniter` directly. These are cross-platform (no tapir
needed):

```scala
import hearth.kindlings.tapiropenapijsoniter.OpenApiJsoniter
import com.github.plokhotnyuk.jsoniter_scala.core._
import sttp.apispec.openapi.OpenAPI

import OpenApiJsoniter.openapi_3_1._ // OpenAPI 3.1 codecs (no Circe involved — named after the OpenAPI version)

def render(doc: OpenAPI): String = writeToString(doc)
def parse(json: String): OpenAPI = readFromString[OpenAPI](json)
```

## OpenAPI 3.0 vs 3.1

The model is the same; only the JSON translation differs (`nullable`, `exclusiveMinimum/Maximum` as booleans, `example`,
`enum`-instead-of-`const`). Pick the entry point that matches the version you advertise:

- `OpenApiJsoniter.openapi_3_1` / `TapirOpenApi.toJson` — OpenAPI **3.1** (JSON Schema draft 2020-12), the default.
- `OpenApiJsoniter.openapi_3_0` / `TapirOpenApi.toJson30` — OpenAPI **3.0.3**.
- `OpenApiJsoniter.openapi_3_1_objectAny` — 3.1, encoding `AnySchema` as `{}` / `{"not":{}}` objects rather than booleans.
- `OpenApiJsoniter.custom(openApi30Flag, anyEncoding)` — build your own combination.

## Notes

- **Byte-for-byte parity with Circe.** The codecs reproduce `sttp-apispec`'s `openapi-circe` output exactly; the test
  suite cross-checks every scenario against circe and asserts a clean round-trip.
- **No Circe at runtime.** Nothing on the production classpath depends on Circe or Cats — serialization goes through
  jsoniter-scala and this module's `Json` AST.
- **Platforms.** Codecs: JVM, Scala.js, Scala Native. `TapirOpenApi` bridge: JVM and Scala.js (tracking
  `tapir-openapi-docs`).
