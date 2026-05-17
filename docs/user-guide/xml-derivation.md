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
    val encoder: XmlEncoder[Book] = KindlingsXmlEncoder.derive[Book]
    val book = Book("978-0-13-468599-1", "The Scala Cookbook", "Alvin Alexander")
    val xml: scala.xml.Elem = encoder.encode(book, "book")
    println(xml)
    // <book isbn="978-0-13-468599-1"><title>The Scala Cookbook</title><author>Alvin Alexander</author></book>

    // Semi-automatic decoding
    val decoder: XmlDecoder[Book] = KindlingsXmlDecoder.derive[Book]
    println(decoder.decode(xml))
    // Right(Book(978-0-13-468599-1,The Scala Cookbook,Alvin Alexander))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsXmlEncoder.derive[A]` | `XmlEncoder[A]` | Semi-automatic encoder |
| `KindlingsXmlEncoder.encode[A](value, elementName)` | `scala.xml.Elem` | Inline encoding (no instance allocation) |
| `KindlingsXmlEncoder.toXmlString[A](value, elementName)` | `String` | Inline encoding to string |
| `KindlingsXmlEncoder.derived[A]` | `KindlingsXmlEncoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsXmlDecoder.derive[A]` | `XmlDecoder[A]` | Semi-automatic decoder |
| `KindlingsXmlDecoder.decode[A](elem)` | `Either[XmlDecodingError, A]` | Inline decoding |
| `KindlingsXmlDecoder.fromXmlString[A](xml)` | `Either[XmlDecodingError, A]` | Inline decoding from string |
| `KindlingsXmlDecoder.derived[A]` | `KindlingsXmlDecoder[A]` | Sanely-automatic (given/implicit) |
| `KindlingsXmlCodec.derive[A]` | `KindlingsXmlCodec[A]` | Semi-automatic codec (encoder + decoder) |
| `KindlingsXmlCodec.derived[A]` | `KindlingsXmlCodec[A]` | Sanely-automatic codec |

All methods take an implicit/using `XmlConfig` parameter (defaults to `XmlConfig.default`).

### Type hierarchy

`KindlingsXmlEncoder[A]` extends `XmlEncoder[A]` and `KindlingsXmlDecoder[A]` extends `XmlDecoder[A]`, so derived instances work anywhere the base types are expected. `KindlingsXmlCodec[A]` extends both `XmlEncoder[A]` and `XmlDecoder[A]`.

### Syntax extensions (Scala 3)

Import `hearth.kindlings.xmlderivation.syntax.*` for extension methods:

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
| `UnexpectedElement` | Unknown child element encountered |
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
| `withAttributesByDefault` | Encode fields as XML attributes by default |
| `withElementsByDefault` | Encode fields as XML child elements by default (this is the default) |
| `withFieldNameMapper(f)` | Custom field name transform |
| `withConstructorNameMapper(f)` | Custom constructor name transform (for ADT discrimination) |
| `withSnakeCaseFieldNames` | `fieldName` -> `field_name` |
| `withKebabCaseFieldNames` | `fieldName` -> `field-name` |
| `withPascalCaseFieldNames` | `fieldName` -> `FieldName` |
| `withScreamingSnakeCaseFieldNames` | `fieldName` -> `FIELD_NAME` |
| `withSnakeCaseConstructorNames` | `MyType` -> `my_type` in discriminator |
| `withKebabCaseConstructorNames` | `MyType` -> `my-type` in discriminator |
| `withDiscriminator(attr)` | ADT discriminator attribute name (default: `"type"`) |
| `withNoDiscriminator` | Disable discriminator (use wrapper element for ADTs) |
| `withEnumAsStrings` | Encode Scala 3 / Java enums as strings |
| `withUseDefaults` | Use case class default values for missing fields |
| `withTransientNone` | Skip `None` fields during encoding |
| `withTransientEmpty` | Skip empty collections during encoding |

## Annotations

| Annotation | Package | Description |
|-----------|---------|-------------|
| `@xmlAttribute` | `hearth.kindlings.xmlderivation.annotations` | Encode field as an XML attribute |
| `@xmlElement` | `hearth.kindlings.xmlderivation.annotations` | Encode field as an XML child element (overrides `withAttributesByDefault`) |
| `@xmlContent` | `hearth.kindlings.xmlderivation.annotations` | Encode field as the text content of the element |
| `@xmlName("name")` | `hearth.kindlings.xmlderivation.annotations` | Override the XML element/attribute name |
| `@xmlWrapper("name")` | `hearth.kindlings.xmlderivation.annotations` | Wrap field in an outer element |
| `@xmlUnwrapped` | `hearth.kindlings.xmlderivation.annotations` | Flatten nested structure |
| `@transientField` | `hearth.kindlings.xmlderivation.annotations` | Exclude field from encoding/decoding (must have default) |

```scala
import hearth.kindlings.xmlderivation.annotations._

case class Article(
  @xmlAttribute id: Int,
  @xmlContent body: String
)
// Encodes as: <article id="42">The article body text</article>
```

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

    val encoder = KindlingsXmlEncoder.derive[Shape]
    val decoder = KindlingsXmlDecoder.derive[Shape]

    val shape: Shape = Circle(5.0)
    val xml = encoder.encode(shape, "shape")
    println(xml)
    // <shape kind="circle"><radius>5.0</radius></shape>

    println(decoder.decode(xml))
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

    // All fields become attributes
    val xml = KindlingsXmlEncoder.encode(Point(10, 20), "point")
    println(xml)
    // <point x="10" y="20"/>
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
    // <product id="P001" category="books">
    //   <name>Scala in Depth</name>
    //   <description>Advanced Scala programming</description>
    //   <tags>scala</tags>
    //   <tags>jvm</tags>
    // </product>
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
    // <a href="https://scala-lang.org">Scala</a>

    val decoded = KindlingsXmlDecoder.decode[Link](xml)
    println(decoded)
    // Right(Link(https://scala-lang.org,Scala))
    ```

??? example "Recursive data types"

    ```scala
    import hearth.kindlings.xmlderivation._

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types just work -- no special setup needed
    val codec: KindlingsXmlCodec[TreeNode] = KindlingsXmlCodec.derive[TreeNode]

    val tree = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, List(TreeNode(4, Nil)))))
    val xml = codec.encode(tree, "tree")
    println(xml)

    println(codec.decode(xml))
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
