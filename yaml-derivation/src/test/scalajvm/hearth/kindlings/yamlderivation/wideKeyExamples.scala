package hearth.kindlings.yamlderivation

/** An enum of 100 case objects used as map keys, for the bytecode size checks. */
sealed trait WideKey
object WideKey {
  case object K00 extends WideKey
  case object K01 extends WideKey
  case object K02 extends WideKey
  case object K03 extends WideKey
  case object K04 extends WideKey
  case object K05 extends WideKey
  case object K06 extends WideKey
  case object K07 extends WideKey
  case object K08 extends WideKey
  case object K09 extends WideKey
  case object K10 extends WideKey
  case object K11 extends WideKey
  case object K12 extends WideKey
  case object K13 extends WideKey
  case object K14 extends WideKey
  case object K15 extends WideKey
  case object K16 extends WideKey
  case object K17 extends WideKey
  case object K18 extends WideKey
  case object K19 extends WideKey
  case object K20 extends WideKey
  case object K21 extends WideKey
  case object K22 extends WideKey
  case object K23 extends WideKey
  case object K24 extends WideKey
  case object K25 extends WideKey
  case object K26 extends WideKey
  case object K27 extends WideKey
  case object K28 extends WideKey
  case object K29 extends WideKey
  case object K30 extends WideKey
  case object K31 extends WideKey
  case object K32 extends WideKey
  case object K33 extends WideKey
  case object K34 extends WideKey
  case object K35 extends WideKey
  case object K36 extends WideKey
  case object K37 extends WideKey
  case object K38 extends WideKey
  case object K39 extends WideKey
  case object K40 extends WideKey
  case object K41 extends WideKey
  case object K42 extends WideKey
  case object K43 extends WideKey
  case object K44 extends WideKey
  case object K45 extends WideKey
  case object K46 extends WideKey
  case object K47 extends WideKey
  case object K48 extends WideKey
  case object K49 extends WideKey
  case object K50 extends WideKey
  case object K51 extends WideKey
  case object K52 extends WideKey
  case object K53 extends WideKey
  case object K54 extends WideKey
  case object K55 extends WideKey
  case object K56 extends WideKey
  case object K57 extends WideKey
  case object K58 extends WideKey
  case object K59 extends WideKey
  case object K60 extends WideKey
  case object K61 extends WideKey
  case object K62 extends WideKey
  case object K63 extends WideKey
  case object K64 extends WideKey
  case object K65 extends WideKey
  case object K66 extends WideKey
  case object K67 extends WideKey
  case object K68 extends WideKey
  case object K69 extends WideKey
  case object K70 extends WideKey
  case object K71 extends WideKey
  case object K72 extends WideKey
  case object K73 extends WideKey
  case object K74 extends WideKey
  case object K75 extends WideKey
  case object K76 extends WideKey
  case object K77 extends WideKey
  case object K78 extends WideKey
  case object K79 extends WideKey
  case object K80 extends WideKey
  case object K81 extends WideKey
  case object K82 extends WideKey
  case object K83 extends WideKey
  case object K84 extends WideKey
  case object K85 extends WideKey
  case object K86 extends WideKey
  case object K87 extends WideKey
  case object K88 extends WideKey
  case object K89 extends WideKey
  case object K90 extends WideKey
  case object K91 extends WideKey
  case object K92 extends WideKey
  case object K93 extends WideKey
  case object K94 extends WideKey
  case object K95 extends WideKey
  case object K96 extends WideKey
  case object K97 extends WideKey
  case object K98 extends WideKey
  case object K99 extends WideKey
}
final case class WideKeyMap(values: Map[WideKey, Int])

/** Key codecs derived for an enum of 100 case objects, with compile-time and runtime configurations. */
object YamlWideKeyInstances {
  val encoder: KindlingsYamlEncoder[WideKeyMap] = KindlingsYamlEncoder.derived[WideKeyMap]
  val decoder: KindlingsYamlDecoder[WideKeyMap] = KindlingsYamlDecoder.derived[WideKeyMap]
  def encoderWith(config: YamlConfig): KindlingsYamlEncoder[WideKeyMap] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlEncoder.derived[WideKeyMap]
  }
  def decoderWith(config: YamlConfig): KindlingsYamlDecoder[WideKeyMap] = {
    implicit val runtimeConfig: YamlConfig = config
    KindlingsYamlDecoder.derived[WideKeyMap]
  }
}
