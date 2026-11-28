package hearth.kindlings.yamlderivation

import org.virtuslab.yaml.{ConstructError, LoadSettings, Node, YamlDecoder}

// Fixtures of DecodingErrorModesSpec (fail-fast vs accumulating decoding failures).

final case class ErrAddress(street: String, zip: Int)

final case class ErrPerson(
    name: String,
    age: Int,
    address: ErrAddress,
    tags: List[Int],
    scores: Map[String, Int],
    nick: Option[Int],
    retries: Int = 3
)

sealed trait ErrShape
final case class ErrCircle(radius: Double) extends ErrShape
final case class ErrRect(w: Int, h: Int) extends ErrShape

final case class ErrDrawing(title: String, shapes: List[ErrShape])

final case class ErrIntKeys(byId: Map[Int, ErrAddress])

/** A value whose (user-provided) decoder counts its calls: shows which fields were decoded. */
final case class Counted(value: Int)
object Counted {
  val calls = new java.util.concurrent.atomic.AtomicInteger(0)
  implicit val decoder: YamlDecoder[Counted] = new YamlDecoder[Counted] {
    def construct(node: Node)(implicit settings: LoadSettings = LoadSettings.empty): Either[ConstructError, Counted] = {
      val _ = calls.incrementAndGet()
      YamlDecoder.forInt.construct(node).map(Counted(_))
    }
  }
}

final case class ErrCounted(first: Int, second: Counted, third: Counted)

/** Instances derived with configurations known at compile time. */
object ErrorModeInstances {

  object FailFast {
    val person: KindlingsYamlDecoder[ErrPerson] = KindlingsYamlDecoder.derived[ErrPerson]
    val counted: KindlingsYamlDecoder[ErrCounted] = KindlingsYamlDecoder.derived[ErrCounted]
    val drawing: KindlingsYamlDecoder[ErrDrawing] = {
      implicit val config: YamlConfig = YamlConfig(discriminator = Some("type"))
      KindlingsYamlDecoder.derived[ErrDrawing]
    }
  }

  object Accumulating {
    implicit val config: YamlConfig = YamlConfig().withAccumulatingErrors
    val person: KindlingsYamlDecoder[ErrPerson] = KindlingsYamlDecoder.derived[ErrPerson]
    val counted: KindlingsYamlDecoder[ErrCounted] = KindlingsYamlDecoder.derived[ErrCounted]
    val intKeys: KindlingsYamlDecoder[ErrIntKeys] = KindlingsYamlDecoder.derived[ErrIntKeys]
  }

  object AccumulatingDiscriminator {
    implicit val config: YamlConfig = YamlConfig(discriminator = Some("type")).withAccumulatingErrors
    val drawing: KindlingsYamlDecoder[ErrDrawing] = KindlingsYamlDecoder.derived[ErrDrawing]
  }

  /** `config` is a parameter: the macro reads `accumulatingErrors` at runtime. */
  object Runtime {
    def person(config: YamlConfig): KindlingsYamlDecoder[ErrPerson] = {
      implicit val runtimeConfig: YamlConfig = config
      KindlingsYamlDecoder.derived[ErrPerson]
    }
    def counted(config: YamlConfig): KindlingsYamlDecoder[ErrCounted] = {
      implicit val runtimeConfig: YamlConfig = config
      KindlingsYamlDecoder.derived[ErrCounted]
    }
    def drawing(config: YamlConfig): KindlingsYamlDecoder[ErrDrawing] = {
      implicit val runtimeConfig: YamlConfig = config
      KindlingsYamlDecoder.derived[ErrDrawing]
    }
  }
}
