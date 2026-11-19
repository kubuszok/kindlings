package hearth.kindlings.benchmarks

import hearth.kindlings.xmlderivation.{KindlingsXmlDecoder, KindlingsXmlEncoder, XmlConfig, XmlDecoder, XmlEncoder}
import hearth.kindlings.xmlderivation.annotations.{xmlAttribute, xmlUnwrapped, xmlWrapper}
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

// An order with attributes, an enum as an attribute and as text, a wrapped and an unwrapped collection:
// <order id="1" status="Active"><customer>..</customer><state>Closed</state>
//   <items><item sku=".." quantity="2"><name>..</name><price>..</price></item>..</items><tag>a</tag><tag>b</tag></order>
sealed trait XmlOrderStatus
object XmlOrderStatus {
  case object Active extends XmlOrderStatus
  case object Suspended extends XmlOrderStatus
  case object Closed extends XmlOrderStatus
}
case class XmlLineItem(@xmlAttribute sku: String, @xmlAttribute quantity: Int, name: String, price: Double)
case class XmlOrder(
    @xmlAttribute id: Long,
    @xmlAttribute status: XmlOrderStatus,
    customer: String,
    state: XmlOrderStatus,
    @xmlWrapper("items") item: List[XmlLineItem],
    @xmlUnwrapped tag: List[String]
)

object XmlBenchmarkData {
  val order: XmlOrder = XmlOrder(
    id = 1234567L,
    status = XmlOrderStatus.Active,
    customer = "Alice Example",
    state = XmlOrderStatus.Closed,
    item = List(
      XmlLineItem("SKU-1", 2, "Keyboard", 49.99),
      XmlLineItem("SKU-2", 1, "Monitor", 199.5),
      XmlLineItem("SKU-3", 10, "Cable", 3.25),
      XmlLineItem("SKU-4", 4, "Mouse", 19.0)
    ),
    tag = List("priority", "gift", "export")
  )
}

object KindlingsXmlInstances {
  val simpleCCEncoder: XmlEncoder[SimpleCC] = KindlingsXmlEncoder.derived[SimpleCC]
  val simpleCCDecoder: XmlDecoder[SimpleCC] = KindlingsXmlDecoder.derived[SimpleCC]
  val addressEncoder: XmlEncoder[Address] = KindlingsXmlEncoder.derived[Address]
  val addressDecoder: XmlDecoder[Address] = KindlingsXmlDecoder.derived[Address]

  private val enumsAsStrings: XmlConfig = XmlConfig().withEnumAsStrings
  val orderEncoder: XmlEncoder[XmlOrder] = {
    implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
    KindlingsXmlEncoder.derived[XmlOrder]
  }
  val orderDecoder: XmlDecoder[XmlOrder] = {
    implicit val config: XmlConfig = XmlConfig().withEnumAsStrings
    KindlingsXmlDecoder.derived[XmlOrder]
  }
  // the same configuration, only known at runtime
  def orderEncoderWith(config: XmlConfig): XmlEncoder[XmlOrder] = {
    implicit val runtimeConfig: XmlConfig = config
    KindlingsXmlEncoder.derived[XmlOrder]
  }
  def orderDecoderWith(config: XmlConfig): XmlDecoder[XmlOrder] = {
    implicit val runtimeConfig: XmlConfig = config
    KindlingsXmlDecoder.derived[XmlOrder]
  }
  val runtimeOrderEncoder: XmlEncoder[XmlOrder] = orderEncoderWith(enumsAsStrings)
  val runtimeOrderDecoder: XmlDecoder[XmlOrder] = orderDecoderWith(enumsAsStrings)
}

/** The same documents written and read the way scala-xml code is usually written by hand: XML literals to build the
  * elements, `\`, `\@` and `.text` to read them, with missing or invalid values reported as a `Left`.
  */
object HandWrittenXml {

  def encodeSimpleCC(value: SimpleCC): scala.xml.Elem =
    <SimpleCC><name>{value.name}</name><age>{value.age.toString}</age><active>{
      value.active.toString
    }</active></SimpleCC>

  def encodeAddress(value: Address): scala.xml.Elem =
    <Address><street>{value.street}</street><city>{value.city}</city><zip>{value.zip}</zip><country>{
      value.country
    }</country></Address>

  private def statusName(status: XmlOrderStatus): String = status match {
    case XmlOrderStatus.Active    => "Active"
    case XmlOrderStatus.Suspended => "Suspended"
    case XmlOrderStatus.Closed    => "Closed"
  }

  def encodeLineItem(value: XmlLineItem): scala.xml.Elem = {
    <item sku={value.sku} quantity={value.quantity.toString}><name>{value.name}</name><price>{
      value.price.toString
    }</price></item>
  }

  def encodeOrder(value: XmlOrder): scala.xml.Elem = {
    <order id={value.id.toString} status={statusName(value.status)}><customer>{value.customer}</customer><state>{
      statusName(value.state)
    }</state><items>{value.item.map(encodeLineItem)}</items>{value.tag.map(tag => <tag>{tag}</tag>)}</order>
  }

  private def childText(elem: scala.xml.Node, name: String): Either[String, String] =
    (elem \ name).headOption match {
      case Some(child) => Right(child.text)
      case None        => Left(s"missing <$name> in <${elem.label}>")
    }

  private def attribute(elem: scala.xml.Node, name: String): Either[String, String] =
    elem.attribute(name) match {
      case Some(value) => Right(value.text)
      case None        => Left(s"missing attribute $name in <${elem.label}>")
    }

  private def toInt(text: String): Either[String, Int] =
    try Right(text.trim.toInt)
    catch { case _: NumberFormatException => Left(s"not an Int: $text") }
  private def toLong(text: String): Either[String, Long] =
    try Right(text.trim.toLong)
    catch { case _: NumberFormatException => Left(s"not a Long: $text") }
  private def toDouble(text: String): Either[String, Double] =
    try Right(text.trim.toDouble)
    catch { case _: NumberFormatException => Left(s"not a Double: $text") }
  private def toBoolean(text: String): Either[String, Boolean] = text.trim match {
    case "true"  => Right(true)
    case "false" => Right(false)
    case other   => Left(s"not a Boolean: $other")
  }
  private def toStatus(text: String): Either[String, XmlOrderStatus] = text.trim match {
    case "Active"    => Right(XmlOrderStatus.Active)
    case "Suspended" => Right(XmlOrderStatus.Suspended)
    case "Closed"    => Right(XmlOrderStatus.Closed)
    case other       => Left(s"not a status: $other")
  }

  def decodeSimpleCC(elem: scala.xml.Elem): Either[String, SimpleCC] = for {
    name <- childText(elem, "name")
    age <- childText(elem, "age").flatMap(toInt)
    active <- childText(elem, "active").flatMap(toBoolean)
  } yield SimpleCC(name, age, active)

  def decodeAddress(elem: scala.xml.Elem): Either[String, Address] = for {
    street <- childText(elem, "street")
    city <- childText(elem, "city")
    zip <- childText(elem, "zip")
    country <- childText(elem, "country")
  } yield Address(street, city, zip, country)

  def decodeLineItem(elem: scala.xml.Node): Either[String, XmlLineItem] = for {
    sku <- attribute(elem, "sku")
    quantity <- attribute(elem, "quantity").flatMap(toInt)
    name <- childText(elem, "name")
    price <- childText(elem, "price").flatMap(toDouble)
  } yield XmlLineItem(sku, quantity, name, price)

  private def traverse[A, B](
      nodes: scala.xml.NodeSeq
  )(f: scala.xml.Node => Either[String, B]): Either[String, List[B]] = {
    val builder = List.newBuilder[B]
    val iterator = nodes.iterator
    while (iterator.hasNext)
      f(iterator.next()) match {
        case Right(value) => builder += value
        case Left(error)  => return Left(error)
      }
    Right(builder.result())
  }

  def decodeOrder(elem: scala.xml.Elem): Either[String, XmlOrder] = for {
    id <- attribute(elem, "id").flatMap(toLong)
    status <- attribute(elem, "status").flatMap(toStatus)
    customer <- childText(elem, "customer")
    state <- childText(elem, "state").flatMap(toStatus)
    items <- traverse(elem \ "items" \ "item")(decodeLineItem)
    tags <- traverse(elem \ "tag")(tag => Right(tag.text))
  } yield XmlOrder(id, status, customer, state, items, tags)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class XmlEncodeBenchmark {

  @Benchmark def kindlingsSimpleCC(): scala.xml.Elem =
    KindlingsXmlInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC, "SimpleCC")

  @Benchmark def handWrittenSimpleCC(): scala.xml.Elem = HandWrittenXml.encodeSimpleCC(BenchmarkData.simpleCC)

  @Benchmark def kindlingsAddress(): scala.xml.Elem =
    KindlingsXmlInstances.addressEncoder.encode(BenchmarkData.address, "Address")

  @Benchmark def handWrittenAddress(): scala.xml.Elem = HandWrittenXml.encodeAddress(BenchmarkData.address)

  @Benchmark def kindlingsOrder(): scala.xml.Elem =
    KindlingsXmlInstances.orderEncoder.encode(XmlBenchmarkData.order, "order")

  @Benchmark def kindlingsRuntimeConfigOrder(): scala.xml.Elem =
    KindlingsXmlInstances.runtimeOrderEncoder.encode(XmlBenchmarkData.order, "order")

  @Benchmark def handWrittenOrder(): scala.xml.Elem = HandWrittenXml.encodeOrder(XmlBenchmarkData.order)
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(2)
class XmlDecodeBenchmark {

  private var simpleCCElem: scala.xml.Elem = _
  private var addressElem: scala.xml.Elem = _
  private var orderElem: scala.xml.Elem = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    // parsed from text, as the documents of an application are
    def parsed(elem: scala.xml.Elem): scala.xml.Elem = scala.xml.XML.loadString(elem.toString)
    simpleCCElem = parsed(KindlingsXmlInstances.simpleCCEncoder.encode(BenchmarkData.simpleCC, "SimpleCC"))
    addressElem = parsed(KindlingsXmlInstances.addressEncoder.encode(BenchmarkData.address, "Address"))
    orderElem = parsed(KindlingsXmlInstances.orderEncoder.encode(XmlBenchmarkData.order, "order"))
    require(HandWrittenXml.encodeOrder(XmlBenchmarkData.order) == orderElem)
    require(KindlingsXmlInstances.runtimeOrderEncoder.encode(XmlBenchmarkData.order, "order") == orderElem)
    require(HandWrittenXml.decodeOrder(orderElem) == Right(XmlBenchmarkData.order))
    require(KindlingsXmlInstances.orderDecoder.decode(orderElem) == Right(XmlBenchmarkData.order))
    require(KindlingsXmlInstances.runtimeOrderDecoder.decode(orderElem) == Right(XmlBenchmarkData.order))
    require(HandWrittenXml.decodeSimpleCC(simpleCCElem) == Right(BenchmarkData.simpleCC))
    require(HandWrittenXml.decodeAddress(addressElem) == Right(BenchmarkData.address))
  }

  @Benchmark def kindlingsSimpleCC(): Either[Any, SimpleCC] =
    KindlingsXmlInstances.simpleCCDecoder.decode(simpleCCElem)

  @Benchmark def handWrittenSimpleCC(): Either[Any, SimpleCC] = HandWrittenXml.decodeSimpleCC(simpleCCElem)

  @Benchmark def kindlingsAddress(): Either[Any, Address] =
    KindlingsXmlInstances.addressDecoder.decode(addressElem)

  @Benchmark def handWrittenAddress(): Either[Any, Address] = HandWrittenXml.decodeAddress(addressElem)

  @Benchmark def kindlingsOrder(): Either[Any, XmlOrder] = KindlingsXmlInstances.orderDecoder.decode(orderElem)

  @Benchmark def kindlingsRuntimeConfigOrder(): Either[Any, XmlOrder] =
    KindlingsXmlInstances.runtimeOrderDecoder.decode(orderElem)

  @Benchmark def handWrittenOrder(): Either[Any, XmlOrder] = HandWrittenXml.decodeOrder(orderElem)
}
