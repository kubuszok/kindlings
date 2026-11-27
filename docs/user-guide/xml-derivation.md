# XML Derivation

Original module -- derives `XmlEncoder`, `XmlDecoder`, and `XmlCodec` for case classes, sealed traits, Scala 3 enums, and more. Supports XML attributes, elements, text content, and wrapper elements via annotations.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-xml-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-xml-derivation" % "{{ kindlings_version() }}"
    ```

    You also need `scala-xml` and `scala-sax-parser` as runtime dependencies:

    ```scala
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-xml" % "{{ libraries.scalaXml }}",
      "com.kubuszok" %%% "scala-sax-parser" % "{{ libraries.scalaSaxParser }}"
    )
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}
    ```

## Quick start

??? example "Encoding and decoding a case class with attributes"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    case class Book(
      @xmlAttribute isbn: String,
      title: String,
      author: String
    )

    // Semi-automatic encoding
    val encoder: XmlEncoder[Book] = KindlingsXmlEncoder.derived[Book]
    val book = Book("978-0-13-468599-1", "The Scala Cookbook", "Alvin Alexander")
    val xml: scala.xml.Elem = encoder.encode(book, "book")
    println(xml)
    // expected output:
    // <book isbn="978-0-13-468599-1"><title>The Scala Cookbook</title><author>Alvin Alexander</author></book>

    // Semi-automatic decoding
    val decoder: XmlDecoder[Book] = KindlingsXmlDecoder.derived[Book]
    println(decoder.decode(xml))
    // expected output:
    // Right(Book(978-0-13-468599-1,The Scala Cookbook,Alvin Alexander))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsXmlEncoder.derived[A]` | `XmlEncoder[A]` | Semi-automatic encoder |
| `KindlingsXmlEncoder.encode[A](value, elementName)` | `scala.xml.Elem` | Inline encoding (no instance allocation) |
| `KindlingsXmlEncoder.toXmlString[A](value, elementName)` | `String` | Inline encoding to string |
| `KindlingsXmlEncoder.derived[A]` | `KindlingsXmlEncoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsXmlDecoder.derived[A]` | `XmlDecoder[A]` | Semi-automatic decoder |
| `KindlingsXmlDecoder.decode[A](elem)` | `Either[XmlDecodingError, A]` | Inline decoding |
| `KindlingsXmlDecoder.fromXmlString[A](xml)` | `Either[XmlDecodingError, A]` | Inline decoding from string |
| `KindlingsXmlDecoder.derived[A]` | `KindlingsXmlDecoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsXmlCodec.derived[A]` | `KindlingsXmlCodec[A]` | Semi-automatic codec (encoder + decoder) |
| `KindlingsXmlCodec.derived[A]` | `KindlingsXmlCodec[A]` | Sanely-automatic codec |

All methods take an implicit/using `XmlConfig` parameter (defaults to `XmlConfig.default`).

### Type hierarchy

`KindlingsXmlEncoder[A]` extends `XmlEncoder[A]` and `KindlingsXmlDecoder[A]` extends `XmlDecoder[A]`, so derived instances work anywhere the base types are expected. `KindlingsXmlCodec[A]` extends both `XmlEncoder[A]` and `XmlDecoder[A]`.

### Syntax extensions

Import `hearth.kindlings.xmlderivation.syntax._` (Scala 3: `syntax.*`) for extension methods — available on Scala 2.13
and Scala 3:

| Extension | Description |
|-----------|-------------|
| `value.toXmlString(elementName)` | Inline encode any value to XML string |
| `xmlString.fromXmlString[A]` | Inline decode XML string to `Either[XmlDecodingError, A]` |

### Error types

`XmlDecodingError` is a sealed hierarchy with these subtypes:

| Error type | Description |
|-----------|-------------|
| `MissingAttribute` | Required XML attribute not found |
| `MissingElement` | Required child element not found |
| `InvalidValue` | Value could not be parsed as the expected type |
| `UnexpectedElement` | Unknown child element encountered (available for hand-written decoders: derived decoders ignore child elements and attributes they do not know) |
| `MissingContent` | Expected text content not found |
| `UnknownDiscriminator` | ADT discriminator value not recognized |
| `MissingDiscriminator` | ADT discriminator attribute not found |
| `Multiple` | Aggregation of multiple decoding errors |
| `General` | Generic error message |

## Configuration

```scala
import hearth.kindlings.xmlderivation._

implicit val config: XmlConfig = XmlConfig.default
  .withAttributesByDefault
  .withSnakeCaseFieldNames
  .withDiscriminator("kind")
```

| Builder method | Description |
|---------------|-------------|
| `withAttributesByDefault` | Encode and decode single-value fields as XML attributes by default (see [Attribute mode](#attribute-mode)) |
| `withElementsByDefault` | Encode fields as XML child elements by default (this is the default) |
| `withFieldNameMapper(f)` | Custom field name transform |
| `withConstructorNameMapper(f)` | Custom constructor name transform (for ADT discrimination) |
| `withSnakeCaseFieldNames` | `fieldName` -> `field_name` |
| `withKebabCaseFieldNames` | `fieldName` -> `field-name` |
| `withPascalCaseFieldNames` | `fieldName` -> `FieldName` |
| `withScreamingSnakeCaseFieldNames` | `fieldName` -> `FIELD_NAME` |
| `withSnakeCaseConstructorNames` | `MyType` -> `my_type` in discriminator |
| `withKebabCaseConstructorNames` | `MyType` -> `my-type` in discriminator |
| `withDiscriminator(attr)` | ADT discriminator attribute name (default: `"type"`); `@xmlDiscriminator(attr)` on a sealed type sets it for that type only |
| `withNoDiscriminator` | Disable the discriminator: the subtype is an element named after it, inside the value's element (see [Sealed traits and enums](#sealed-traits-and-enums)) |
| `withEnumAsStrings` | Write enums whose cases are all singletons (case objects, parameterless Scala 3 enum cases, Java enums, `Enumeration`s) as the case name: the text of the element, or an attribute value. A sealed trait with a case class is then an error (see [Sealed traits and enums](#sealed-traits-and-enums)) |
| `withUseDefaults` | Use case class default values for missing fields (absent child elements and attributes) |
| `withTransientNone` | Skip `None` fields during encoding |
| `withTransientEmpty` | Skip empty collections during encoding (empty strings are still written) |

Decoding is lenient about absent optional data, so the output of `withTransientNone` / `withTransientEmpty` decodes
back to the same value whatever decoder configuration is used:

- an absent child element or attribute of an `Option` field decodes as `None` (with `withUseDefaults`, a field with a
  default value gets the default instead),
- an absent child element of a collection or map field decodes as an empty collection (unless `withUseDefaults` and a
  default value apply),
- any other absent element or attribute is still an error (`MissingElement` / `MissingAttribute`).

An empty value is present, not absent:

- an empty element or attribute value of a `String` field (`<name/>`, `label=""`) decodes as `""`, so `""` round-trips
  (an absent `String` element or attribute is still an error);
- an `Option` attribute is written as its value for `Some` (`Some("")` as `label=""`) and omitted for `None`, so both
  round-trip;
- an empty element of an `Option` field decodes as `None`, because that is how `None` is written without
  `withTransientNone` (`Some("")` is written the same way, so it decodes as `None`). With `withTransientNone`, `None`
  is not written at all, so an empty element decodes as `Some("")` for an `Option[String]` (and as `None` when the
  empty element is not a value of the inner type, e.g. for an `Option[Int]`).

### Attribute mode

With `withAttributesByDefault` (or `withDefaultFieldMode(XmlFieldMode.Attribute)`), a field without an annotation is
written as an attribute and read from an attribute when it holds a single text value:

- `String`, `Int`, `Long`, `Double`, `Float`, `Boolean`, `Byte`, `Short`, `Char`, `BigDecimal`, `BigInt`,
- a value type wrapping one of these (written as the unwrapped value: `id="5"`, not `id="UserId(5)"`),
- with `withEnumAsStrings`: an enum whose cases are all singletons (written as the case name),
- an `Option` of one of these (written as the value for `Some`, omitted for `None`).

Any other field stays a child element in both modes: collections, maps, case classes, sealed traits (and enums without
`withEnumAsStrings`), types with a user-provided `XmlEncoder`/`XmlDecoder`, and `Option`s of them. Nested case classes
use the same configuration, so their fields are attributes of their own element. `@xmlElement` makes a field a child
element and `@xmlAttribute` makes it an attribute whatever the default mode is (see [Annotations](#annotations) for what
`@xmlAttribute` accepts). Fields with `@xmlWrapper` or `@xmlUnwrapped` are child elements too. The decoder follows the same rule as the encoder, also when the configuration is only
known at runtime, so the output of an encoder decodes with a decoder that uses the same configuration.

An absent attribute is decoded like an absent element: `None` for an `Option` field, the default value with
`withUseDefaults`, `MissingAttribute` otherwise. As with `withTransientNone`, `None` is written as nothing, so with
`withUseDefaults` an `Option` field whose default is not `None` decodes an omitted `None` as its default.

### Whitespace

`String` values keep their whitespace exactly, in elements, attributes and `@xmlContent` text: `<name> a </name>`
decodes as `" a "`, a blank element as a blank `String`, so every `String` round-trips (a `Char` is kept exactly too,
so `' '` round-trips). For an `Option[String]`, only an empty element is `None`: `<name> </name>` is `Some(" ")`.

Values parsed from text are trimmed first, so `<age> 42 </age>` and `age=" 42 "` decode as `42`: numbers, `Boolean`,
`BigDecimal`, `BigInt` and value types wrapping them. Whitespace between elements is ignored.

`KindlingsXmlEncoder.toXmlString` (and the `toXmlString` syntax) indents nested elements, but writes elements that hold
text exactly, so its output decodes back to the same `String` values. `scala.xml.PrettyPrinter` does not: it collapses
runs of whitespace in text and attribute values, and moves text that does not fit the line width onto lines of its
own, inside the text-only element. Its output still decodes for every type parsed from text (numbers, `Boolean`, ...),
but `String` values can change, so do not use it (or other reformatting) on documents with `String` data whose
whitespace matters; use `toXmlString` or `elem.toString`. The XML parser also normalizes tabs and line breaks in
attribute values to spaces (as the XML specification requires), so keep multi-line `String`s in elements.

## Annotations

All in `hearth.kindlings.xmlderivation.annotations`.

| Annotation | Description |
|-----------|-------------|
| `@xmlAttribute` | The field is an XML attribute |
| `@xmlElement` | The field is a child element (overrides `withAttributesByDefault`) |
| `@xmlContent` | The field is the text content of the element; the other fields are attributes |
| `@xmlDiscriminator("name")` | On a sealed trait / Scala 3 enum: the name of its discriminator attribute, whatever the configuration says |
| `@xmlName("name")` | The name of the field's element/attribute, or of a sealed trait's subtype (used as it is: not transformed by `fieldNameMapper` / `constructorNameMapper`) |
| `@xmlWrapper("name")` | The field goes inside an outer element `name`; a collection is written there as one element per item, named after the field |
| `@xmlUnwrapped` | A collection is written as one element per item, named after the field, directly in the element (no element for the collection itself) |
| `@transientField` | The field is not written; when decoding it gets its default value (it must have one) |

```scala
import hearth.kindlings.xmlderivation.annotations._

case class Article(
  @xmlAttribute id: Int,
  @xmlContent body: String
)
// Encodes as: <article id="42">The article body text</article>
```

The encoder and the decoder place every field the same way, with a configuration known at compile time or only at
runtime, so the output of an encoder decodes with a decoder that uses the same configuration. Annotations that cannot
work are compile-time errors (listing every such field of the case class).

### Attributes and text content

`@xmlAttribute` and `@xmlContent` need a single text value:

- `String`, the numeric types, `Boolean`, `Char`, `BigDecimal`, `BigInt`, and value types wrapping them,
- with `withEnumAsStrings`: an enum whose cases are all singletons, written as the case name. Without
  `withEnumAsStrings` an enum is an element with a discriminator attribute (`<color type="Red"/>`), which neither an
  attribute nor text can hold, so `@xmlAttribute` / `@xmlContent` on an enum is then a compile-time error (enable
  `withEnumAsStrings`, or remove the annotation),
- a type with a user-provided `XmlEncoder`/`XmlDecoder` in scope: the attribute holds the text of the element the
  encoder writes, and the decoder gets an element with that text,
- an `Option` of one of these: `None` is an absent attribute / no text.

Anything else - a case class, a collection, a map, a sealed trait with case classes - is a compile-time error:

```
Field 'address' of Person: @xmlAttribute needs a single text value, Address is not one (an XML attribute cannot hold a
collection, a map or a case class). ... Remove @xmlAttribute to write the field as a child element
```

An element with text content has no child elements, so next to an `@xmlContent` field every other field is an
attribute (as with `@xmlAttribute`, whatever the default field mode is); a field that cannot be one, or that is
annotated with `@xmlElement`, `@xmlWrapper` or `@xmlUnwrapped`, is a compile-time error. Combining `@xmlAttribute`,
`@xmlElement`, `@xmlContent`, `@xmlWrapper` and `@xmlUnwrapped` on one field is an error too, except `@xmlElement` with
`@xmlWrapper`.

### Collections: default, `@xmlWrapper`, `@xmlUnwrapped`

| Field | XML |
|-------|-----|
| `tags: List[String]` | `<tags><item>a</item><item>b</item></tags>` |
| `@xmlWrapper("tags") tag: List[String]` | `<tags><tag>a</tag><tag>b</tag></tags>` |
| `@xmlUnwrapped tag: List[String]` | `<tag>a</tag><tag>b</tag>` |
| `props: Map[String, Int]` | `<props><entry key="a"><value>1</value></entry></props>` |
| `@xmlWrapper("props") prop: Map[String, Int]` | `<props><prop key="a"><value>1</value></prop></props>` |
| `@xmlUnwrapped prop: Map[String, Int]` | `<prop key="a"><value>1</value></prop>` |

- The item elements are named after the field: `@xmlName` or the field name transformed by `fieldNameMapper`. The
  wrapper name is used as it is. Only the elements with that name are items; they are read in document order.
- `@xmlWrapper` on a field that is not a collection puts its element inside the wrapper:
  `@xmlWrapper("meta") note: Option[String]` is `<meta><note>n</note></meta>`. A wrapped field is always a child
  element, also with `withAttributesByDefault`; the items inside use the configuration as everywhere else. Each
  wrapper holds one field (two fields with the same wrapper name are a compile-time error).
- An empty wrapped collection is an empty wrapper (`<tags/>`), a wrapped `None` an empty element in the wrapper
  (`<meta><note/></meta>`); `withTransientEmpty` / `withTransientNone` skip the wrapper instead.
- When decoding, an absent wrapper is an empty collection / `None` (a required field inside it is a
  `MissingElement`). With `withUseDefaults`, an absent wrapper is the field's default value; a wrapper that is present
  is what it holds, so an empty collection written as `<tags/>` is read back as empty, not as the default.
- An unwrapped collection without any item is an empty collection - or, with `withUseDefaults`, the field's default
  value (an empty unwrapped collection writes nothing, so it cannot be told apart from an absent one).
- `@xmlUnwrapped` needs a collection or a map (with `String` keys); anything else is a compile-time error.

### Sealed traits and enums

A sealed trait is written as the element of the subtype plus its name:

| Configuration | `Circle(5.0): Shape` as `shape` |
|---------------|--------------------------------|
| default | `<shape type="Circle"><radius>5.0</radius></shape>` |
| `withDiscriminator("kind")` | `<shape kind="Circle"><radius>5.0</radius></shape>` |
| `withNoDiscriminator` | `<shape><Circle><radius>5.0</radius></Circle></shape>` |

The name is the subtype's name transformed by `constructorNameMapper`, or the one set with `@xmlName` on the subtype.
Nested sealed traits are flattened: a leaf is written under its own name, whatever intermediate sealed traits it
extends (unless an intermediate type has a user-provided instance, which is then used).

`@xmlDiscriminator("kind")` on a sealed trait (or a Scala 3 enum) sets the discriminator attribute of that type, for
encoding and decoding, whatever the configuration says: `withDiscriminator(name)` does not rename it, and with
`withNoDiscriminator` the type keeps its attribute. Errors (`MissingDiscriminator`, `UnknownDiscriminator`) name the
attribute that is used.

```scala
@xmlDiscriminator("kind") sealed trait Shape
case class Circle(radius: Double) extends Shape
// Circle(5.0): Shape as shape: <shape kind="Circle"><radius>5.0</radius></shape>
```

It is the annotation of the type a value is encoded or decoded as that counts: with
`sealed trait Node; @xmlDiscriminator("inner") sealed trait Inner extends Node`, a field of type `Inner` uses `inner`,
while a field of type `Node` holding the same value uses the discriminator of `Node` (the configured one) - the decoder
has to know which attribute to read before it knows the subtype. A Java enum cannot carry the annotation (it is a Scala
annotation); it uses the configured discriminator.

An enum whose cases are all singletons - a sealed trait of case objects, a Scala 3 enum without parameters, a Java
enum, a `scala.Enumeration` - is written the same way by default, and as the case name with `withEnumAsStrings`:

| Configuration | `Red: Color` as `color` |
|---------------|-------------------------|
| default | `<color type="Red"/>` |
| `withNoDiscriminator` | `<color><Red/></color>` |
| `withEnumAsStrings` | `<color>Red</color>` |
| `withEnumAsStrings` with `@xmlAttribute` or `withAttributesByDefault` | `color="Red"` |
| `withEnumAsStrings` with `@xmlContent` | the text `Red` |

A name that is not a case is an `UnknownDiscriminator` error listing the known names.

`withEnumAsStrings` is only for such enums. Two things are errors:

- deriving, with `withEnumAsStrings`, an instance for a sealed trait that has a case class (or a Scala 3 enum case
  with parameters): `enumAsStrings is enabled but Shape has cases that are not singletons (Circle, Rectangle); ...`.
  A model that has both kinds needs an instance for such a type derived with a configuration without
  `enumAsStrings`, put in the implicit scope: user-provided instances are used as they are;
- `@xmlAttribute` / `@xmlContent` on an enum without `withEnumAsStrings` (see
  [Attributes and text content](#attributes-and-text-content)).

Both are compile-time errors when the macro knows the configuration. When the configuration is only known at runtime
(a parameter, a `var`, or - for Scala 2 macros - an `implicit val` defined in the code being compiled), the instance
is derived and fails when it is used with such a configuration: the encoder throws an `IllegalArgumentException`, the
decoder returns `Left(XmlDecodingError.General(...))`, both with the same message.

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: XmlConfig = XmlConfig.default
      .withDiscriminator("kind")
      .withSnakeCaseConstructorNames

    val encoder = KindlingsXmlEncoder.derived[Shape]
    val decoder = KindlingsXmlDecoder.derived[Shape]

    val shape: Shape = Circle(5.0)
    val xml = encoder.encode(shape, "shape")
    println(xml)
    // expected output:
    // <shape kind="circle"><radius>5.0</radius></shape>

    println(decoder.decode(xml))
    // expected output:
    // Right(Circle(5.0))
    ```

??? example "Attributes-by-default mode"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    implicit val config: XmlConfig = XmlConfig.default.withAttributesByDefault

    case class Point(x: Int, y: Int)
    case class Route(name: String, @xmlElement note: String, stops: List[Point])

    // Single-value fields become attributes
    val xml = KindlingsXmlEncoder.encode(Point(10, 20), "point")
    println(xml)
    // expected output:
    // <point x="10" y="20"/>

    // A collection stays a child element, @xmlElement makes a child element
    val route = Route("A", "scenic", List(Point(1, 2), Point(3, 4)))
    val routeXml = KindlingsXmlEncoder.encode(route, "route")
    println(routeXml)
    // expected output:
    // <route name="A"><note>scenic</note><stops><item x="1" y="2"/><item x="3" y="4"/></stops></route>

    // The decoder reads the same layout back
    println(KindlingsXmlDecoder.decode[Route](routeXml))
    // expected output:
    // Right(Route(A,scenic,List(Point(1,2), Point(3,4))))
    ```

??? example "Mixed attributes and elements"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    case class Product(
      @xmlAttribute id: String,
      @xmlAttribute category: String,
      name: String,
      description: String,
      tags: List[String]
    )

    val product = Product("P001", "books", "Scala in Depth", "Advanced Scala programming", List("scala", "jvm"))
    val xml = KindlingsXmlEncoder.encode(product, "product")
    println(xml)
    // expected output:
    // <product id="P001" category="books"><name>Scala in Depth</name><description>Advanced Scala programming</description><tags><item>scala</item><item>jvm</item></tags></product>
    ```

??? example "Content annotation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    case class Link(
      @xmlAttribute href: String,
      @xmlContent text: String
    )

    val link = Link("https://scala-lang.org", "Scala")
    val xml = KindlingsXmlEncoder.encode(link, "a")
    println(xml)
    // expected output:
    // <a href="https://scala-lang.org">Scala</a>

    val decoded = KindlingsXmlDecoder.decode[Link](xml)
    println(decoded)
    // expected output:
    // Right(Link(https://scala-lang.org,Scala))
    ```

??? example "Wrapped and unwrapped collections"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    case class Item(@xmlAttribute sku: String, name: String)
    case class Order(
      @xmlAttribute id: Int,
      @xmlWrapper("items") item: List[Item],
      @xmlUnwrapped tag: List[String]
    )

    val codec: KindlingsXmlCodec[Order] = KindlingsXmlCodec.derived[Order]

    val order = Order(1, List(Item("A1", "Keyboard"), Item("B2", "Mouse")), List("gift", "express"))
    val xml = codec.encode(order, "order")
    println(xml)
    // expected output:
    // <order id="1"><items><item sku="A1"><name>Keyboard</name></item><item sku="B2"><name>Mouse</name></item></items><tag>gift</tag><tag>express</tag></order>

    println(codec.decode(xml))
    // expected output:
    // Right(Order(1,List(Item(A1,Keyboard), Item(B2,Mouse)),List(gift, express)))

    // an empty wrapped collection is an empty wrapper, an empty unwrapped one writes nothing
    println(codec.encode(Order(2, Nil, Nil), "order"))
    // expected output:
    // <order id="2"><items/></order>

    // an absent wrapper is an empty collection
    println(codec.decode(<order id="3"/>))
    // expected output:
    // Right(Order(3,List(),List()))
    ```

??? example "Enums as strings"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._
    import hearth.kindlings.xmlderivation.annotations._

    sealed trait Color
    case object Red extends Color
    @xmlName("dark-blue") case object DarkBlue extends Color

    case class Swatch(color: Color, accents: List[Color])
    case class Paint(@xmlAttribute finish: Color, color: Color, accents: List[Color])

    // By default an enum is written like any sealed trait: with the discriminator attribute.
    println(KindlingsXmlEncoder.encode(Swatch(DarkBlue, List(Red, DarkBlue)), "swatch")(XmlConfig.default))
    // expected output:
    // <swatch><color type="dark-blue"/><accents><item type="Red"/><item type="dark-blue"/></accents></swatch>

    // With withEnumAsStrings it is the case name, which can also be an attribute (@xmlAttribute on an enum needs it).
    implicit val config: XmlConfig = XmlConfig.default.withEnumAsStrings

    val paint = Paint(Red, DarkBlue, List(Red, DarkBlue))
    val xml = KindlingsXmlEncoder.encode(paint, "paint")
    println(xml)
    // expected output:
    // <paint finish="Red"><color>dark-blue</color><accents><item>Red</item><item>dark-blue</item></accents></paint>

    println(KindlingsXmlDecoder.decode[Paint](xml))
    // expected output:
    // Right(Paint(Red,DarkBlue,List(Red, DarkBlue)))

    println(KindlingsXmlDecoder.decode[Color](<color>Pink</color>).isLeft)
    // expected output:
    // true
    ```

??? example "Sealed trait without a discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape
    case class Drawing(shapes: List[Shape])

    implicit val config: XmlConfig = XmlConfig.default.withNoDiscriminator

    val codec: KindlingsXmlCodec[Drawing] = KindlingsXmlCodec.derived[Drawing]

    val xml = codec.encode(Drawing(List(Circle(1.0), Rectangle(2.0, 3.0))), "drawing")
    println(xml)
    // expected output:
    // <drawing><shapes><item><Circle><radius>1.0</radius></Circle></item><item><Rectangle><width>2.0</width><height>3.0</height></Rectangle></item></shapes></drawing>

    println(codec.decode(xml))
    // expected output:
    // Right(Drawing(List(Circle(1.0), Rectangle(2.0,3.0))))
    ```

??? example "Recursive data types"

    ```scala
    //> using dep com.kubuszok::kindlings-xml-derivation:{{ kindlings_version() }}
    //> using dep org.scala-lang.modules::scala-xml:{{ libraries.scalaXml }}
    //> using dep com.kubuszok::scala-sax-parser:{{ libraries.scalaSaxParser }}

    import hearth.kindlings.xmlderivation._

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types just work -- no special setup needed
    val codec: KindlingsXmlCodec[TreeNode] = KindlingsXmlCodec.derived[TreeNode]

    val tree = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, List(TreeNode(4, Nil)))))
    val xml = codec.encode(tree, "tree")
    println(codec.decode(xml))
    // expected output:
    // Right(TreeNode(1,List(TreeNode(2,List()), TreeNode(3,List(TreeNode(4,List()))))))
    ```

## Debugging

Import the debug package to log the generated encoder/decoder during compilation:

```scala
import hearth.kindlings.xmlderivation.debug._
```

This enables logging for both `KindlingsXmlEncoder` and `KindlingsXmlDecoder` derivations.

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:xmlDerivation.logDerivation=true"
```
