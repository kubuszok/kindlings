package hearth.kindlings.oracle.jsoniter

import hearth.MacroSuite

/** jsoniter-scala's own test suite as an oracle: the fixtures, values and inputs of `JsonCodecMakerSpec`
  * (jsoniter-scala-macros/shared/src/test/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMakerSpec.scala,
  * v2.42.3), derived with `KindlingsJsonValueCodec.derived` (config known at compile time and only at runtime) and with
  * `JsonCodecMaker.make` (the equivalent `CodecMakerConfig`), compared on encoded JSON, accepted/rejected inputs (with
  * error messages and offsets) and `nullValue`. Groups are named after the config, cases after upstream's test or
  * fixture (`JU<name>` is upstream's `<name>`).
  *
  * As in `JsoniterOracleSpec`, each case lists the *exact* differences it expects, each a documented one
  * (`docs/user-guide/feature-parity.md`, "jsoniter-derivation").
  *
  * Not ported (yet): the tests of user-written codecs injected as implicits (the parity specs of jsoniter-derivation
  * cover user-provided field, leaf and key codecs, not as an oracle), of the name mapper functions
  * (`JsoniterConfigNameMappersSpec` in jsoniter-derivation uses upstream's vectors), of compile-time errors
  * (`CompileTimeValidationSpec` and the parity specs in jsoniter-derivation), of `WriterConfig`/`ReaderConfig`
  * (jsoniter-scala-core's, not the codecs'), and upstream's Scala-version-specific specs (new types, new enums, literal
  * types, generic tuples: see `JsoniterScala3OracleSpec` and jsoniter-derivation's `JsoniterScala3ParitySpec`).
  */
final class JsoniterUpstreamSpecOracleSpec extends MacroSuite {

  private def quoted(s: String): String =
    "\"" + s.flatMap {
      case '"'          => "\\\""
      case '\\'         => "\\\\"
      case '\n'         => "\\n"
      case '\r'         => "\\r"
      case '\t'         => "\\t"
      case '\b'         => "\\b"
      case '\f'         => "\\f"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c            => c.toString
    } + "\""

  private def check[A](codecs: => JoCodecs[A], samples: JoSamples[A])(expectedDifferences: String*): Unit = {
    val differences = JsoniterOracle.differences(codecs, samples.values, samples.inputs)
    assertEquals(
      differences,
      expectedDifferences.toList,
      s"Kindlings vs jsoniter-scala differences:\n${differences.map(d => "DIFF|" + quoted(d)).mkString(",\n")}"
    )
  }

  group("defaults") {
    test("primitives")(check(JuCases.defaults.primitives, JuSamples.primitives)())
    test("boxedPrimitives")(check(JuCases.defaults.boxedPrimitives, JuSamples.boxedPrimitives)())
    test("byteTop")(check(JuCases.defaults.byteTop, JuSamples.byteTop)())
    test("shortTop")(check(JuCases.defaults.shortTop, JuSamples.shortTop)())
    test("intTop")(check(JuCases.defaults.intTop, JuSamples.intTop)())
    test("longTop")(check(JuCases.defaults.longTop, JuSamples.longTop)())
    test("booleanTop")(check(JuCases.defaults.booleanTop, JuSamples.booleanTop)())
    test("charTop")(check(JuCases.defaults.charTop, JuSamples.charTop)())
    test("doubleTop")(check(JuCases.defaults.doubleTop, JuSamples.doubleTop)())
    test("floatTop")(check(JuCases.defaults.floatTop, JuSamples.floatTop)())
    test("jByteTop")(check(JuCases.defaults.jByteTop, JuSamples.jByteTop)())
    test("jShortTop")(check(JuCases.defaults.jShortTop, JuSamples.jShortTop)())
    test("jIntegerTop")(check(JuCases.defaults.jIntegerTop, JuSamples.jIntegerTop)())
    test("jLongTop")(check(JuCases.defaults.jLongTop, JuSamples.jLongTop)())
    test("jBooleanTop")(check(JuCases.defaults.jBooleanTop, JuSamples.jBooleanTop)())
    test("jCharacterTop")(check(JuCases.defaults.jCharacterTop, JuSamples.jCharacterTop)())
    test("jDoubleTop")(check(JuCases.defaults.jDoubleTop, JuSamples.jDoubleTop)())
    test("jFloatTop")(check(JuCases.defaults.jFloatTop, JuSamples.jFloatTop)())
    test("jByteMap")(check(JuCases.defaults.jByteMap, JuSamples.jByteMap)())
    test("jShortMap")(check(JuCases.defaults.jShortMap, JuSamples.jShortMap)())
    test("jIntegerMap")(check(JuCases.defaults.jIntegerMap, JuSamples.jIntegerMap)())
    test("jLongMap")(check(JuCases.defaults.jLongMap, JuSamples.jLongMap)())
    test("jBooleanMap")(check(JuCases.defaults.jBooleanMap, JuSamples.jBooleanMap)())
    test("jCharacterMap")(check(JuCases.defaults.jCharacterMap, JuSamples.jCharacterMap)())
    test("jDoubleMap")(check(JuCases.defaults.jDoubleMap, JuSamples.jDoubleMap)())
    test("jFloatMap")(check(JuCases.defaults.jFloatMap, JuSamples.jFloatMap)())
    test("standardTypes")(check(JuCases.defaults.standardTypes, JuSamples.standardTypes)())
    test("stringTop")(check(JuCases.defaults.stringTop, JuSamples.stringTop)())
    test("bigIntTop")(check(JuCases.defaults.bigIntTop, JuSamples.bigIntTop)())
    test("bigDecimalTop")(check(JuCases.defaults.bigDecimalTop, JuSamples.bigDecimalTop)())
    test("nonCaseClass")(check(JuCases.defaults.nonCaseClass, JuSamples.nonCaseClass)())
    test("javaTypes")(check(JuCases.defaults.javaTypes, JuSamples.javaTypes)())
    test("uuidTop")(check(JuCases.defaults.uuidTop, JuSamples.uuidTop)())
    test("uuidMap")(check(JuCases.defaults.uuidMap, JuSamples.uuidMap)())
    test("javaEnums")(check(JuCases.defaults.javaEnums, JuSamples.javaEnums)())
    test("javaEnumTop")(check(JuCases.defaults.javaEnumTop, JuSamples.javaEnumTop)())
    test("javaEnumMap")(check(JuCases.defaults.javaEnumMap, JuSamples.javaEnumMap)())
    test("enums")(check(JuCases.defaults.enums, JuSamples.enums)())
    test("celestialList")(check(JuCases.defaults.celestialList, JuSamples.celestialList)())
    test("celestialMap")(check(JuCases.defaults.celestialMap, JuSamples.celestialMap)())
    test("spectralList")(check(JuCases.defaults.spectralList, JuSamples.spectralList)())
    test("spectralMap")(check(JuCases.defaults.spectralMap, JuSamples.spectralMap)())
    test("locationTop")(check(JuCases.defaults.locationTop, JuSamples.locationTop)())
    test("locationMap")(check(JuCases.defaults.locationMap, JuSamples.locationMap)())
    test("cardRankList")(check(JuCases.defaults.cardRankList, JuSamples.cardRankList)())
    test("cardRankMap")(check(JuCases.defaults.cardRankMap, JuSamples.cardRankMap)())
    test("birdList")(check(JuCases.defaults.birdList, JuSamples.birdList)())
    test("birdMap")(check(JuCases.defaults.birdMap, JuSamples.birdMap)())
    test("options")(check(JuCases.defaults.options, JuSamples.options)())
    test("optionInt")(check(JuCases.defaults.optionInt, JuSamples.optionInt)())
    test("optionInner")(check(JuCases.defaults.optionInner, JuSamples.optionInner)())
    test("optionString")(check(JuCases.defaults.optionString, JuSamples.optionString)())
    test("arrayOption")(check(JuCases.defaults.arrayOption, JuSamples.arrayOption)())
    test("listOption")(check(JuCases.defaults.listOption, JuSamples.listOption)())
    test("setOption")(check(JuCases.defaults.setOption, JuSamples.setOption)())
    test("mapOption")(check(JuCases.defaults.mapOption, JuSamples.mapOption)())
    test("tuples")(check(JuCases.defaults.tuples, JuSamples.tuples)())
    test("tupleTop")(check(JuCases.defaults.tupleTop, JuSamples.tupleTop)())
    test("tupleAliases")(check(JuCases.defaults.tupleAliases, JuSamples.tupleAliases)())
    test("tupleAliasTop")(check(JuCases.defaults.tupleAliasTop, JuSamples.tupleTop)())
    test("arrays")(check(JuCases.defaults.arrays, JuSamples.arrays)())
    test("arrayArrayTop")(check(JuCases.defaults.arrayArrayTop, JuSamples.arrayArrayTop)())
    test("iterables")(check(JuCases.defaults.iterables, JuSamples.iterables)())
    // Hearth bug (docs/research/hearth-findings-jsoniter-parity-2026-11.md, 1): the `Factory` Hearth summons for an
    // abstract collection type is the one of its *element* type's companion when that is a subtype of it
    // (`mutable.IndexedSeq[mutable.ArrayStack[Float]]` is built as a `mutable.Stack`, jsoniter-scala: an `ArrayBuffer`).
    test("mutableIterables")(
      check(JuCases.defaults.mutableIterables, JuSamples.mutableIterables)(
        "decode {\"ml\":[[\"3\",\"2\",\"1\"]],\"ab\":[[4,4]],\"as\":[[6,5,6]],\"b\":[[7.7,7.7]],\"lb\":[[10,9]],\"is\":[[]],\"ub\":[[]]}: kindlings=accepted JUMutableIterables(ArrayBuffer(TreeSet(1, 2, 3)),ArrayBuffer...(201 chars) upstream=accepted JUMutableIterables(ArrayBuffer(TreeSet(1, 2, 3)),ArrayBuffer...(207 chars)",
        "decode {\"ml\":null,\"ab\":null,\"as\":null,\"b\":null,\"lb\":null,\"is\":null,\"ub\":null}: kindlings=accepted JUMutableIterables(ArrayBuffer(),ArrayBuffer(),ArraySeq(),ArrayBuffer(),ListBuffer(),Stack(),UnrolledBuffer()) upstream=accepted JUMutableIterables(ArrayBuffer(),ArrayBuffer(),ArraySeq(),ArrayBuffer(),ListBuffer(),ArrayBuffer(),UnrolledBuffer())",
        "decode {}: kindlings=accepted JUMutableIterables(ArrayBuffer(),ArrayBuffer(),ArraySeq(),ArrayBuffer(),ListBuffer(),Stack(),UnrolledBuffer()) upstream=accepted JUMutableIterables(ArrayBuffer(),ArrayBuffer(),ArraySeq(),ArrayBuffer(),ListBuffer(),ArrayBuffer(),UnrolledBuffer())",
        "decode {\"ml\":[[\"1\",\"2\",\"3\"]],\"ab\":[[4],[]],\"as\":[[5,6],[]],\"b\":[[7.7]],\"lb\":[[9,10]],\"is\":[[11.11,12.12]],\"ub\":[[13,14]]}: kindlings=accepted JUMutableIterables(ArrayBuffer(TreeSet(1, 2, 3)),ArrayBuffer...(247 chars) upstream=accepted JUMutableIterables(ArrayBuffer(TreeSet(1, 2, 3)),ArrayBuffer...(253 chars)"
      )
    )
    test("immutableIterables")(check(JuCases.defaults.immutableIterables, JuSamples.immutableIterables)())
    test("nonEmptyTop")(check(JuCases.defaults.nonEmptyTop, JuSamples.nonEmptyTop)())
    test("emptyIterables")(check(JuCases.defaults.emptyIterables, JuSamples.emptyIterables)())
    test("emptyIterables2")(check(JuCases.defaults.emptyIterables2, JuSamples.emptyIterables2)())
    test("mutableSetTop")(check(JuCases.defaults.mutableSetTop, JuSamples.mutableSetTop)())
    test("genericMaps")(check(JuCases.defaults.genericMaps, JuSamples.genericMaps)())
    // Hearth bug (docs/research/hearth-findings-jsoniter-parity-2026-11.md, 1): the `Factory` Hearth summons for an
    // abstract collection type is the one of its *element* type's companion when that is a subtype of it
    // (`mutable.Map[Float, mutable.ListMap[BigInt, String]]` is built as a `mutable.ListMap`, jsoniter-scala: a `HashMap`).
    test("mutableMaps")(
      check(JuCases.defaults.mutableMaps, JuSamples.mutableMaps)(
        "decode {\"hm\":null,\"m\":null,\"ohm\":null}: kindlings=accepted JUMutableMaps(HashMap(),ListMap(),OpenHashMap()) upstream=accepted JUMutableMaps(HashMap(),HashMap(),OpenHashMap())",
        "decode {\"hm\":{\"true\":{\"1.1\":1,\"1.10\":2}},\"m\":{},\"ohm\":{}}: kindlings=accepted JUMutableMaps(HashMap(true -> AnyRefMap(1.1 -> 2)),ListMap(),OpenHashMap()) upstream=accepted JUMutableMaps(HashMap(true -> AnyRefMap(1.1 -> 2)),HashMap(),OpenHashMap())",
        "decode {}: kindlings=accepted JUMutableMaps(HashMap(),ListMap(),OpenHashMap()) upstream=accepted JUMutableMaps(HashMap(),HashMap(),OpenHashMap())",
        "decode {\"hm\":{\"true\":{\"1.1\":1}},\"m\":{\"1.1\":{\"2\":\"2\"}},\"ohm\":{\"1.1\":{\"3\":3.3},\"2.2\":{}}}: kindlings=accepted JUMutableMaps(HashMap(true -> AnyRefMap(1.1 -> 1)),ListMap(1...(151 chars) upstream=accepted JUMutableMaps(HashMap(true -> AnyRefMap(1.1 -> 1)),HashMap(1...(151 chars)"
      )
    )
    test("immutableMaps")(check(JuCases.defaults.immutableMaps, JuSamples.immutableMaps)())
    test("emptyMaps")(check(JuCases.defaults.emptyMaps, JuSamples.emptyMaps)())
    test("linkedMapTop")(check(JuCases.defaults.linkedMapTop, JuSamples.linkedMapTop)())
    test("mutableLongMaps")(check(JuCases.defaults.mutableLongMaps, JuSamples.mutableLongMaps)())
    test("immutableIntLongMaps")(check(JuCases.defaults.immutableIntLongMaps, JuSamples.immutableIntLongMaps)())
    test("bitSets")(check(JuCases.defaults.bitSets, JuSamples.bitSets)())
    test("longIntBitTop")(check(JuCases.defaults.longIntBitTop, JuSamples.longIntBitTop)())
    test("immutableBitSetTop")(check(JuCases.defaults.immutableBitSetTop, JuSamples.immutableBitSetTop)())
    test("mutableBitSetTop")(check(JuCases.defaults.mutableBitSetTop, JuSamples.mutableBitSetTop)())
    test("cases")(check(JuCases.defaults.cases, JuSamples.cases)())
    test("nameOverridden")(check(JuCases.defaults.nameOverridden, JuSamples.nameOverridden)())
    test("stringified")(check(JuCases.defaults.stringified, JuSamples.stringified)())
    test("utf8")(check(JuCases.defaults.utf8, JuSamples.utf8)())
    test("operators")(check(JuCases.defaults.operators, JuSamples.operators)())
    test("defaults")(check(JuCases.defaults.defaults, JuSamples.defaults)())
    test("defaults2")(check(JuCases.defaults.defaults2, JuSamples.defaults2)())
    test("sharedEmptyMutableDefaults")(
      check(JuCases.defaults.sharedEmptyMutableDefaults, JuSamples.sharedEmptyMutableDefaults)()
    )
    test("polymorphicDefaults")(check(JuCases.defaults.polymorphicDefaults, JuSamples.polymorphicDefaults)())
    test("iterablesWithDefaults")(check(JuCases.defaults.iterablesWithDefaults, JuSamples.iterablesWithDefaults)())
    test("transient")(check(JuCases.defaults.transient, JuSamples.transient)())
    test("noneValues")(check(JuCases.defaults.noneValues, JuSamples.noneValues)())
    test("noneValuesList")(check(JuCases.defaults.noneValuesList, JuSamples.noneValuesList)())
    test("skipUnknown")(check(JuCases.defaults.skipUnknown, JuSamples.skipUnknown)())
    test("required32")(check(JuCases.defaults.required32, JuSamples.required32)())
    test("requiredAfterOptional")(check(JuCases.defaults.requiredAfterOptional, JuSamples.requiredAfterOptional)())
    test("valueClassTypes")(check(JuCases.defaults.valueClassTypes, JuSamples.valueClassTypes)())
    test("userIds")(check(JuCases.defaults.userIds, JuSamples.userIds)())
    test("userIdTop")(check(JuCases.defaults.userIdTop, JuSamples.userIdTop)())
    test("orderIdTop")(check(JuCases.defaults.orderIdTop, JuSamples.orderIdTop)())
    test("userIdArray")(check(JuCases.defaults.userIdArray, JuSamples.userIdArray)())
    test("orderIdArray")(check(JuCases.defaults.orderIdArray, JuSamples.orderIdArray)())
    test("orderUserMap")(check(JuCases.defaults.orderUserMap, JuSamples.orderUserMap)())
    test("userOrderMap")(check(JuCases.defaults.userOrderMap, JuSamples.userOrderMap)())
    test("aliasUserId")(check(JuCases.defaults.aliasUserId, JuSamples.aliasUserId)())
    test("aliasOrderId")(check(JuCases.defaults.aliasOrderId, JuSamples.aliasOrderId)())
    test("genericUserId")(check(JuCases.defaults.genericUserId, JuSamples.genericUserId)())
    test("genericOrderId")(check(JuCases.defaults.genericOrderId, JuSamples.genericOrderId)())
    test("nestedOptions")(check(JuCases.defaults.nestedOptions, JuSamples.nestedOptions)())
    test("eitherFoo")(check(JuCases.defaults.eitherFoo, JuSamples.eitherFoo)())
    test("javaTime")(check(JuCases.defaults.javaTime, JuSamples.javaTime)())
    test("javaTimeMaps")(check(JuCases.defaults.javaTimeMaps, JuSamples.javaTimeMaps)())
    test("dayOfWeekTop")(check(JuCases.defaults.dayOfWeekTop, JuSamples.dayOfWeekTop)())
    test("durationTop")(check(JuCases.defaults.durationTop, JuSamples.durationTop)())
    test("instantTop")(check(JuCases.defaults.instantTop, JuSamples.instantTop)())
    test("localDateTop")(check(JuCases.defaults.localDateTop, JuSamples.localDateTop)())
    test("localDateTimeTop")(check(JuCases.defaults.localDateTimeTop, JuSamples.localDateTimeTop)())
    test("localTimeTop")(check(JuCases.defaults.localTimeTop, JuSamples.localTimeTop)())
    test("monthTop")(check(JuCases.defaults.monthTop, JuSamples.monthTop)())
    test("monthDayTop")(check(JuCases.defaults.monthDayTop, JuSamples.monthDayTop)())
    test("offsetDateTimeTop")(check(JuCases.defaults.offsetDateTimeTop, JuSamples.offsetDateTimeTop)())
    test("offsetTimeTop")(check(JuCases.defaults.offsetTimeTop, JuSamples.offsetTimeTop)())
    test("periodTop")(check(JuCases.defaults.periodTop, JuSamples.periodTop)())
    test("yearTop")(check(JuCases.defaults.yearTop, JuSamples.yearTop)())
    test("yearMonthTop")(check(JuCases.defaults.yearMonthTop, JuSamples.yearMonthTop)())
    test("zonedDateTimeTop")(check(JuCases.defaults.zonedDateTimeTop, JuSamples.zonedDateTimeTop)())
    test("zoneIdTop")(check(JuCases.defaults.zoneIdTop, JuSamples.zoneIdTop)())
    test("zoneOffsetTop")(check(JuCases.defaults.zoneOffsetTop, JuSamples.zoneOffsetTop)())
    test("adtList")(check(JuCases.defaults.adtList, JuSamples.adtList)())
    test("aaa")(check(JuCases.defaults.aaa, JuSamples.aaa)())
    test("bbb")(check(JuCases.defaults.bbb, JuSamples.bbb)())
    test("ccc")(check(JuCases.defaults.ccc, JuSamples.ccc)())
    test("ddd")(check(JuCases.defaults.ddd, JuSamples.ddd)())
    test("extra")(check(JuCases.defaults.extra, JuSamples.extra)())
    test("nonCaseLeaves")(check(JuCases.defaults.nonCaseLeaves, JuSamples.nonCaseLeaves)())
    test("sealedClass")(check(JuCases.defaults.sealedClass, JuSamples.sealedClass)())
    test("namedLeaves")(check(JuCases.defaults.namedLeaves, JuSamples.namedLeaves)())
    test("deep")(check(JuCases.defaults.deep, JuSamples.deep)())
    test("mixed")(check(JuCases.defaults.mixed, JuSamples.mixed)())
    test("diamond")(check(JuCases.defaults.diamond, JuSamples.diamond)())
    test("basketApple")(check(JuCases.defaults.basketApple, JuSamples.basketApple)())
    test("basketOrange")(check(JuCases.defaults.basketOrange, JuSamples.basketOrange)())
    test("typeAliases")(check(JuCases.defaults.typeAliases, JuSamples.typeAliases)())
    test("aliasMap")(check(JuCases.defaults.aliasMap, JuSamples.aliasMap)())
    test("aliasList")(check(JuCases.defaults.aliasList, JuSamples.aliasList)())
    test("idArray")(check(JuCases.defaults.idArray, JuSamples.idArray)())
    test("firstOrder")(check(JuCases.defaults.firstOrder, JuSamples.firstOrder)())
    test("firstOrderId")(check(JuCases.defaults.firstOrderId, JuSamples.firstOrderId)())
    test("gadt2Array")(check(JuCases.defaults.gadt2Array, JuSamples.gadt2Array)())
    test("hk")(check(JuCases.defaults.hk, JuSamples.hk)())
    test("freeGadt")(check(JuCases.defaults.freeGadt, JuSamples.freeGadt)())
    test("auxiliary")(check(JuCases.defaults.auxiliary, JuSamples.auxiliary)())
    test("multiList")(check(JuCases.defaults.multiList, JuSamples.multiList)())
    test("multiListDefault")(check(JuCases.defaults.multiListDefault, JuSamples.multiListDefault)())
  }
  group("recursive") {
    test("recursive")(check(JuCases.recursive.recursive, JuSamples.recursive)())
    test("human")(check(JuCases.recursive.human, JuSamples.human)())
    test("hkOption")(check(JuCases.recursive.hkOption, JuSamples.hkOption)())
    test("hkSeq")(check(JuCases.recursive.hkSeq, JuSamples.hkSeq)())
  }
  group("stringified") {
    test("byteTop")(check(JuCases.stringified.byteTop, JuSamples.byteTop)())
    test("shortTop")(check(JuCases.stringified.shortTop, JuSamples.shortTop)())
    test("intTop")(check(JuCases.stringified.intTop, JuSamples.intTop)())
    test("longTop")(check(JuCases.stringified.longTop, JuSamples.longTop)())
    test("booleanTop")(check(JuCases.stringified.booleanTop, JuSamples.booleanTop)())
    test("doubleTop")(check(JuCases.stringified.doubleTop, JuSamples.doubleTop)())
    test("floatTop")(check(JuCases.stringified.floatTop, JuSamples.floatTop)())
    test("jByteTop")(check(JuCases.stringified.jByteTop, JuSamples.jByteTop)())
    test("jShortTop")(check(JuCases.stringified.jShortTop, JuSamples.jShortTop)())
    test("jIntegerTop")(check(JuCases.stringified.jIntegerTop, JuSamples.jIntegerTop)())
    test("jLongTop")(check(JuCases.stringified.jLongTop, JuSamples.jLongTop)())
    test("jBooleanTop")(check(JuCases.stringified.jBooleanTop, JuSamples.jBooleanTop)())
    test("jDoubleTop")(check(JuCases.stringified.jDoubleTop, JuSamples.jDoubleTop)())
    test("jFloatTop")(check(JuCases.stringified.jFloatTop, JuSamples.jFloatTop)())
    test("bigIntTop")(check(JuCases.stringified.bigIntTop, JuSamples.bigIntTop)())
    test("bigDecimalTop")(check(JuCases.stringified.bigDecimalTop, JuSamples.bigDecimalTop)())
    test("orderIdTop")(check(JuCases.stringified.orderIdTop, JuSamples.orderIdTop)())
    test("optionBigInt")(check(JuCases.stringified.optionBigInt, JuSamples.optionBigInt)())
    test("tuple3")(check(JuCases.stringified.tuple3, JuSamples.tuple3)())
    test("arrayArrayTop")(check(JuCases.stringified.arrayArrayTop, JuSamples.arrayArrayTop)())
    test("mutableSetTop")(check(JuCases.stringified.mutableSetTop, JuSamples.mutableSetTop)())
    test("linkedMapTop")(check(JuCases.stringified.linkedMapTop, JuSamples.linkedMapTop)())
    test("longIntBitTop")(check(JuCases.stringified.longIntBitTop, JuSamples.longIntBitTop)())
  }
  group("enumId") {
    test("enums")(check(JuCases.enumId.enums, JuSamples.enums)())
    test("celestialList")(check(JuCases.enumId.celestialList, JuSamples.celestialList)())
    test("celestialMap")(check(JuCases.enumId.celestialMap, JuSamples.celestialMap)())
    test("spectralList")(check(JuCases.enumId.spectralList, JuSamples.spectralList)())
    test("spectralMap")(check(JuCases.enumId.spectralMap, JuSamples.spectralMap)())
    test("locationTop")(check(JuCases.enumId.locationTop, JuSamples.locationTop)())
    test("locationMap")(check(JuCases.enumId.locationMap, JuSamples.locationMap)())
    test("enums2")(check(JuCases.enumId.enums2, JuSamples.enums2)())
    test("temperatureList")(check(JuCases.enumId.temperatureList, JuSamples.temperatureList)())
    test("temperatureMap")(check(JuCases.enumId.temperatureMap, JuSamples.temperatureMap)())
  }
  group("enumIdStringified") {
    test("locationTop")(check(JuCases.enumIdStringified.locationTop, JuSamples.locationTop)())
  }
  group("noDuplicationCheck") {
    test("standardTypes")(check(JuCases.noDuplicationCheck.standardTypes, JuSamples.standardTypes)())
    test("adtList")(check(JuCases.noDuplicationCheck.adtList, JuSamples.adtList)())
  }
  group("javaEnumSnakeCase") {
    test("javaEnums")(check(JuCases.javaEnumSnakeCase.javaEnums, JuSamples.javaEnums)())
  }
  group("javaEnumSnakeThenPascalCase") {
    test("javaEnums")(check(JuCases.javaEnumSnakeThenPascalCase.javaEnums, JuSamples.javaEnums)())
  }
  group("javaEnumPartial") {
    test("javaEnums")(check(JuCases.javaEnumPartial.javaEnums, JuSamples.javaEnums)())
  }
  group("noTransientNone") {
    test("options")(check(JuCases.noTransientNone.options, JuSamples.options)())
  }
  group("noTransientEmpty") {
    test("arrays")(check(JuCases.noTransientEmpty.arrays, JuSamples.arrays)())
    test("emptyIterables")(check(JuCases.noTransientEmpty.emptyIterables, JuSamples.emptyIterables)())
    test("emptyMaps")(check(JuCases.noTransientEmpty.emptyMaps, JuSamples.emptyMaps)())
  }
  group("makeWithRequiredCollectionFields") {
    test("arrays")(check(JuCases.makeWithRequiredCollectionFields.arrays, JuSamples.arrays)())
    test("emptyIterables")(check(JuCases.makeWithRequiredCollectionFields.emptyIterables, JuSamples.emptyIterables)())
    test("emptyMaps")(check(JuCases.makeWithRequiredCollectionFields.emptyMaps, JuSamples.emptyMaps)())
    test("iterablesWithDefaults")(
      check(JuCases.makeWithRequiredCollectionFields.iterablesWithDefaults, JuSamples.iterablesWithDefaults)()
    )
  }
  group("makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName") {
    test("arrays")(
      check(JuCases.makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName.arrays, JuSamples.arrays)()
    )
    test("customNameList")(
      check(
        JuCases.makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName.customNameList,
        JuSamples.customNameList
      )()
    )
  }
  group("nestedIterables") {
    test("nestedIterables")(check(JuCases.nestedIterables.nestedIterables, JuSamples.nestedIterables)())
  }
  group("eitherValueMapper") {
    test("eitherTop")(check(JuCases.eitherValueMapper.eitherTop, JuSamples.eitherTop)())
  }
  group("requireCollectionsNoTransientEmpty") {
    test("iterablesWithDefaults")(
      check(JuCases.requireCollectionsNoTransientEmpty.iterablesWithDefaults, JuSamples.iterablesWithDefaults)()
    )
  }
  group("makeWithRequiredDefaultFields") {
    test("iterablesWithDefaults")(
      check(JuCases.makeWithRequiredDefaultFields.iterablesWithDefaults, JuSamples.iterablesWithDefaults)()
    )
    test("defaults")(check(JuCases.makeWithRequiredDefaultFields.defaults, JuSamples.defaults)())
    test("multiListDefault")(
      check(JuCases.makeWithRequiredDefaultFields.multiListDefault, JuSamples.multiListDefault)()
    )
  }
  group("requireDefaultsNoTransientDefault") {
    test("iterablesWithDefaults")(
      check(JuCases.requireDefaultsNoTransientDefault.iterablesWithDefaults, JuSamples.iterablesWithDefaults)()
    )
    test("defaults")(check(JuCases.requireDefaultsNoTransientDefault.defaults, JuSamples.defaults)())
    test("transient")(check(JuCases.requireDefaultsNoTransientDefault.transient, JuSamples.transient)())
  }
  group("requireDefaultsAndCollections") {
    test("iterablesWithDefaults")(
      check(JuCases.requireDefaultsAndCollections.iterablesWithDefaults, JuSamples.iterablesWithDefaults)()
    )
  }
  group("setLimit10") {
    test("set")(check(JuCases.setLimit10.set, JuSamples.set)())
    test("mutableSet")(check(JuCases.setLimit10.mutableSet, JuSamples.mutableSet)())
  }
  group("mapLimit10") {
    test("map")(check(JuCases.mapLimit10.map, JuSamples.limitedMap)())
    test("longMap")(check(JuCases.mapLimit10.longMap, JuSamples.limitedLongMap)())
    test("intMap")(check(JuCases.mapLimit10.intMap, JuSamples.limitedIntMap)())
  }
  group("mapLimit10AsArray") {
    test("map")(check(JuCases.mapLimit10AsArray.map, JuSamples.limitedMap)())
    test("longMap")(check(JuCases.mapLimit10AsArray.longMap, JuSamples.limitedLongMap)())
    test("intMap")(check(JuCases.mapLimit10AsArray.intMap, JuSamples.limitedIntMap)())
  }
  group("noMapLimit") {
    test("map")(check(JuCases.noMapLimit.map, JuSamples.limitedMap)())
    test("longMap")(check(JuCases.noMapLimit.longMap, JuSamples.limitedLongMap)())
    test("intMap")(check(JuCases.noMapLimit.intMap, JuSamples.limitedIntMap)())
  }
  group("noMapLimitAsArray") {
    test("map")(check(JuCases.noMapLimitAsArray.map, JuSamples.limitedMap)())
    test("longMap")(check(JuCases.noMapLimitAsArray.longMap, JuSamples.limitedLongMap)())
    test("intMap")(check(JuCases.noMapLimitAsArray.intMap, JuSamples.limitedIntMap)())
  }
  group("mapAsArray") {
    test("genericMap")(check(JuCases.mapAsArray.genericMap, JuSamples.genericMap)())
    test("linkedMap")(check(JuCases.mapAsArray.linkedMap, JuSamples.linkedMap)())
    test("mutableLongMap")(check(JuCases.mapAsArray.mutableLongMap, JuSamples.mutableLongMap)())
    test("immutableLongMap")(check(JuCases.mapAsArray.immutableLongMap, JuSamples.immutableLongMap)())
    test("intMap")(check(JuCases.mapAsArray.intMap, JuSamples.intMap)())
    test("stringifiedMutableLongMaps")(
      check(JuCases.mapAsArray.stringifiedMutableLongMaps, JuSamples.stringifiedMutableLongMaps)()
    )
    test("stringifiedImmutableIntLongMaps")(
      check(JuCases.mapAsArray.stringifiedImmutableIntLongMaps, JuSamples.stringifiedImmutableIntLongMaps)()
    )
  }
  group("mapAsArrayStringified") {
    test("genericMap")(check(JuCases.mapAsArrayStringified.genericMap, JuSamples.genericMap)())
    test("linkedMap")(check(JuCases.mapAsArrayStringified.linkedMap, JuSamples.linkedMap)())
    test("mutableLongMap")(check(JuCases.mapAsArrayStringified.mutableLongMap, JuSamples.mutableLongMap)())
    test("immutableLongMap")(check(JuCases.mapAsArrayStringified.immutableLongMap, JuSamples.immutableLongMap)())
    test("intMap")(check(JuCases.mapAsArrayStringified.intMap, JuSamples.intMap)())
    test("mutableLongMapsList")(
      check(JuCases.mapAsArrayStringified.mutableLongMapsList, JuSamples.mutableLongMapsList)()
    )
    test("immutableIntLongMapsList")(
      check(JuCases.mapAsArrayStringified.immutableIntLongMapsList, JuSamples.immutableIntLongMapsList)()
    )
  }
  group("bitSetLimit1000") {
    test("immutableBitSetTop")(check(JuCases.bitSetLimit1000.immutableBitSetTop, JuSamples.immutableBitSetTop)())
  }
  group("renamed") {
    test("cases")(check(JuCases.renamed.cases, JuSamples.cases)())
  }
  group("renamedWithAlternatives") {
    test("cases")(check(JuCases.renamedWithAlternatives.cases, JuSamples.cases)())
  }
  group("renamedWithGuards") {
    test("cases")(check(JuCases.renamedWithGuards.cases, JuSamples.cases)())
  }
  group("camelCase") {
    test("cases")(check(JuCases.camelCase.cases, JuSamples.cases)())
  }
  group("snakeCase") {
    test("cases")(check(JuCases.snakeCase.cases, JuSamples.cases)())
  }
  group("snakeCase2") {
    test("cases")(check(JuCases.snakeCase2.cases, JuSamples.cases)())
  }
  group("kebabCase") {
    test("cases")(check(JuCases.kebabCase.cases, JuSamples.cases)())
  }
  group("kebabCase2") {
    test("cases")(check(JuCases.kebabCase2.cases, JuSamples.cases)())
  }
  group("pascalCase") {
    test("cases")(check(JuCases.pascalCase.cases, JuSamples.cases)())
  }
  group("suffixMapper") {
    test("stringified")(check(JuCases.suffixMapper.stringified, JuSamples.stringified)())
  }
  group("noTransientDefault") {
    test("defaults")(check(JuCases.noTransientDefault.defaults, JuSamples.defaults)())
    test("transient")(check(JuCases.noTransientDefault.transient, JuSamples.transient)())
  }
  group("noTransientEmptyNone") {
    test("defaults")(check(JuCases.noTransientEmptyNone.defaults, JuSamples.defaults)())
  }
  group("scalaTransient") {
    test("scalaTransient")(check(JuCases.scalaTransient.scalaTransient, JuSamples.scalaTransient)())
  }
  group("scalaTransientNoTransientDefault") {
    test("scalaTransient")(check(JuCases.scalaTransientNoTransientDefault.scalaTransient, JuSamples.scalaTransient)())
  }
  group("scalaTransientRequireDefaults") {
    test("scalaTransient")(check(JuCases.scalaTransientRequireDefaults.scalaTransient, JuSamples.scalaTransient)())
  }
  group("strict") {
    test("skipUnknown")(check(JuCases.strict.skipUnknown, JuSamples.skipUnknown)())
    test("namedLeaves")(check(JuCases.strict.namedLeaves, JuSamples.namedLeaves)())
  }
  group("inlineOneValueClasses") {
    test("oneValueClassTypes")(check(JuCases.inlineOneValueClasses.oneValueClassTypes, JuSamples.oneValueClassTypes)())
    test("transientOneValueClassTypes")(
      check(JuCases.inlineOneValueClasses.transientOneValueClassTypes, JuSamples.transientOneValueClassTypes)()
    )
    test("transientMap")(check(JuCases.inlineOneValueClasses.transientMap, JuSamples.transientMap)())
    test("dependentMap")(check(JuCases.inlineOneValueClasses.dependentMap, JuSamples.dependentMap)())
    test("oneUserIdTop")(check(JuCases.inlineOneValueClasses.oneUserIdTop, JuSamples.oneUserIdTop)())
    test("oneOrderIdTop")(check(JuCases.inlineOneValueClasses.oneOrderIdTop, JuSamples.oneOrderIdTop)())
    test("oneUserIdArray")(check(JuCases.inlineOneValueClasses.oneUserIdArray, JuSamples.oneUserIdArray)())
    test("oneOrderIdArray")(check(JuCases.inlineOneValueClasses.oneOrderIdArray, JuSamples.oneOrderIdArray)())
    test("oneUserIds")(check(JuCases.inlineOneValueClasses.oneUserIds, JuSamples.oneUserIds)())
    test("oneUserIdsT")(check(JuCases.inlineOneValueClasses.oneUserIdsT, JuSamples.oneUserIdsT)())
    test("oneUserIdsF")(check(JuCases.inlineOneValueClasses.oneUserIdsF, JuSamples.oneUserIdsF)())
  }
  group("skipNestedOptionValues") {
    test("modelList")(check(JuCases.skipNestedOptionValues.modelList, JuSamples.modelList)())
  }
  group("makeWithSkipNestedOptionValues") {
    test("modelList")(check(JuCases.makeWithSkipNestedOptionValues.modelList, JuSamples.modelList)())
  }
  group("noDiscriminator") {
    test("eitherFoo")(check(JuCases.noDiscriminator.eitherFoo, JuSamples.eitherFoo)())
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case
    // object is also read from `{"Leaf":{}}`, so an object given for an ADT of case objects only is read as such
    // (and fails with Kindlings' errors) where jsoniter-scala expects a string.
    test("weaponList")(
      check(JuCases.noDiscriminator.weaponList, JuSamples.weaponList)(
        "decode [{\"Axe\":{}}]: kindlings=accepted List(Axe) upstream=rejected: expected '\"', offset: 0x00000001",
        "decode [{\"type\":\"Axe\"}]: kindlings=rejected: illegal discriminator, offset: 0x00000008 upstream=rejected: expected '\"', offset: 0x00000001"
      )
    )
    test("gadtArray")(check(JuCases.noDiscriminator.gadtArray, JuSamples.gadtArray)())
    test("adtList")(check(JuCases.noDiscriminator.adtList, JuSamples.adtList)())
  }
  group("makeWithoutDiscriminator") {
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case
    // object is also read from `{"Leaf":{}}`, so an object given for an ADT of case objects only is read as such
    // (and fails with Kindlings' errors) where jsoniter-scala expects a string.
    test("weaponList")(
      check(JuCases.makeWithoutDiscriminator.weaponList, JuSamples.weaponList)(
        "decode [{\"Axe\":{}}]: kindlings=accepted List(Axe) upstream=rejected: expected '\"', offset: 0x00000001",
        "decode [{\"type\":\"Axe\"}]: kindlings=rejected: illegal discriminator, offset: 0x00000008 upstream=rejected: expected '\"', offset: 0x00000001"
      )
    )
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case
    // object is also read from `{"Leaf":{}}`, so an object given for an ADT of case objects only is read as such
    // (here the case object `C` among case classes).
    test("extra")(
      check(JuCases.makeWithoutDiscriminator.extra, JuSamples.extra)(
        "decode {\"C\":{\"extra\":\"should be ignored\"}}: kindlings=accepted C upstream=rejected: illegal discriminator, offset: 0x00000004"
      )
    )
    test("versionList")(check(JuCases.makeWithoutDiscriminator.versionList, JuSamples.versionList)())
    test("gadtArray")(check(JuCases.makeWithoutDiscriminator.gadtArray, JuSamples.gadtArray)())
  }
  group("discriminatorT") {
    test("adtList")(check(JuCases.discriminatorT.adtList, JuSamples.adtList)())
  }
  group("discriminatorAnywhere") {
    test("adtList")(check(JuCases.discriminatorAnywhere.adtList, JuSamples.adtList)())
  }
  group("discriminatorTAnywhere") {
    test("adtList")(check(JuCases.discriminatorTAnywhere.adtList, JuSamples.adtList)())
  }
  group("alwaysEmitDiscriminator") {
    test("aaa")(check(JuCases.alwaysEmitDiscriminator.aaa, JuSamples.aaa)())
    test("bbb")(check(JuCases.alwaysEmitDiscriminator.bbb, JuSamples.bbb)())
    test("ccc")(check(JuCases.alwaysEmitDiscriminator.ccc, JuSamples.ccc)())
    test("ddd")(check(JuCases.alwaysEmitDiscriminator.ddd, JuSamples.ddd)())
  }
  group("alwaysEmitDiscriminatorStrict") {
    test("aaa")(check(JuCases.alwaysEmitDiscriminatorStrict.aaa, JuSamples.aaa)())
    test("bbb")(check(JuCases.alwaysEmitDiscriminatorStrict.bbb, JuSamples.bbb)())
    test("ccc")(check(JuCases.alwaysEmitDiscriminatorStrict.ccc, JuSamples.ccc)())
    test("ddd")(check(JuCases.alwaysEmitDiscriminatorStrict.ddd, JuSamples.ddd)())
  }
  group("makeOpenapiLike") {
    test("enumeration")(check(JuCases.makeOpenapiLike.enumeration, JuSamples.enumeration)())
  }
  group("makeOpenapiLikeHint") {
    test("enumeration")(check(JuCases.makeOpenapiLikeHint.enumeration, JuSamples.enumeration)())
  }
  group("makeOpenapiLikeHintMapper") {
    test("enumeration")(check(JuCases.makeOpenapiLikeHintMapper.enumeration, JuSamples.enumeration)())
  }
  group("makeOpenapiLikeWithoutDiscriminator") {
    // Documented superset (jsoniter-derivation guide, "ADTs, enums and tuples"): without a discriminator a case
    // object is also read from `{"Leaf":{}}`, so an object given for an ADT of case objects only is read as such
    // (and fails with Kindlings' errors) where jsoniter-scala expects a string.
    test("enumeration")(
      check(JuCases.makeOpenapiLikeWithoutDiscriminator.enumeration, JuSamples.enumeration)(
        "decode {\"type\":\"EnumValue1\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"hint\":\"EnumValue2\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"hint\":\"enum_value_1\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"EnumValue1\":{}}: kindlings=accepted EnumValue1 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"hint\":\"EnumValue3\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"type\":\"enum_value_1\"}: kindlings=rejected: illegal discriminator, offset: 0x00000007 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {}: kindlings=rejected: expected '\"', offset: 0x00000001 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {: kindlings=rejected: unexpected end of input, offset: 0x00000001 upstream=rejected: expected '\"', offset: 0x00000000",
        "decode {\"a\":1,}: kindlings=rejected: illegal discriminator, offset: 0x00000004 upstream=rejected: expected '\"', offset: 0x00000000"
      )
    )
  }
  group("makeCirceLike") {
    test("base")(check(JuCases.makeCirceLike.base, JuSamples.circeBase)())
  }
  group("makeCirceLikeSnakeCased") {
    test("base")(check(JuCases.makeCirceLikeSnakeCased.base, JuSamples.circeBase)())
  }
  group("circeLikeObjects") {
    test("enumerationList")(check(JuCases.circeLikeObjects.enumerationList, JuSamples.enumerationList)())
  }
  group("decodingOnly") {
    test("intTop")(check(JuCases.decodingOnly.intTop, JuSamples.intTop)())
  }
  group("encodingOnly") {
    test("intTop")(check(JuCases.encodingOnly.intTop, JuSamples.intTop)())
  }
  group("discriminatorTStrict") {
    test("customNameList")(check(JuCases.discriminatorTStrict.customNameList, JuSamples.customNameList)())
  }
  group("leafNameMapper") {
    test("customNameList")(check(JuCases.leafNameMapper.customNameList, JuSamples.customNameList)())
  }
  group("fullLeafNames") {
    test("nestedAdtList")(check(JuCases.fullLeafNames.nestedAdtList, JuSamples.nestedAdtList)())
  }
  group("nonAsciiDiscriminator") {
    test("nonAsciiList")(check(JuCases.nonAsciiDiscriminator.nonAsciiList, JuSamples.nonAsciiList)())
  }
  group("discriminatorZoneId") {
    test("timeZoneList")(check(JuCases.discriminatorZoneId.timeZoneList, JuSamples.timeZoneList)())
  }
  group("discriminatorHint") {
    test("hintFoo")(check(JuCases.discriminatorHint.hintFoo, JuSamples.hintFoo)())
  }

  // Cases the value/input comparison of `check` cannot express.

  /** The elements of what both Kindlings codecs observe (the same) which differ from what upstream's observes. */
  private def differingElements[A](
      codecs: JoCodecs[A]
  )(observe: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A] => Product): List[String] = {
    val kindlings = observe(codecs.optimized)
    assertEquals(observe(codecs.runtime), kindlings, "runtime vs optimized")
    kindlings.productIterator.toList.zip(observe(codecs.upstream).productIterator.toList).zipWithIndex.collect {
      case ((k, u), index) if k != u => s"_${index + 1}: kindlings=$k upstream=$u"
    }
  }

  private def all[A](
      codecs: JoCodecs[A]
  )(observe: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A] => Any): Unit = {
    val upstream = observe(codecs.upstream)
    assertEquals(observe(codecs.optimized), upstream, "optimized vs upstream")
    assertEquals(observe(codecs.runtime), upstream, "runtime vs upstream")
  }

  group("iterators (Spec: 'case classes with Iterators', 'top-level Iterators', empty Iterators)") {
    // An `Iterator` can be written once: each codec writes what it read.
    def roundTrip[A](codecs: JoCodecs[A], encoders: JoCodecs[A])(jsons: String*): Unit = jsons.foreach { json =>
      def run(
          decoder: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A],
          encoder: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A]
      ): String =
        try
          JsoniterOracle.encode(
            encoder,
            com.github.plokhotnyuk.jsoniter_scala.core.readFromString(json)(decoder)
          )
        catch { case e: com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException => s"rejected: ${e.getMessage}" }
      val upstream = run(codecs.upstream, encoders.upstream)
      assertEquals(run(codecs.optimized, encoders.optimized), upstream, s"optimized vs upstream for $json")
      assertEquals(run(codecs.runtime, encoders.runtime), upstream, s"runtime vs upstream for $json")
    }
    val fieldInputs = Seq(
      """{"s":[["1","2","3"]],"i":[4,5,6]}""",
      """{"s":[[]],"i":[]}""",
      """{"s":[],"i":[]}""",
      """{}""",
      """{"s":null,"i":null}""",
      """{"s":[null],"i":[4]}""",
      """{"s":[["1",]],"i":[4]}"""
    )
    test("fields")(roundTrip(JuCases.defaults.iterators, JuCases.defaults.iterators)(fieldInputs*))
    test("fields, transientEmpty off")(
      roundTrip(JuCases.noTransientEmpty.iterators, JuCases.noTransientEmpty.iterators)(fieldInputs*)
    )
    test("top level")(
      roundTrip(JuCases.defaults.iteratorTop, JuCases.defaults.iteratorTop)(
        """[[1,2,3],[4,5,6]]""",
        "[]",
        "[[]]",
        "null",
        "[null]",
        "[[1,]]"
      )
    )
    test("nullValue is an empty iterator") {
      all(JuCases.defaults.iteratorTop)(_.nullValue.hasNext)
      all(JuCases.defaults.iterators)(_.nullValue == null)
    }
  }

  group("null values (Spec: 'throw exception in attempt to serialize null values', '... for ADTs')") {
    def thrown[A](value: A)(codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[A]): String =
      try { val _ = com.github.plokhotnyuk.jsoniter_scala.core.writeToString(value)(codec); "written" }
      catch { case e: Throwable => e.getClass.getName }
    test("a null case class")(all(JuCases.defaults.standardTypes)(thrown[JUStandardTypes](null)))
    test("a null ADT value in a list") {
      all(JuCases.defaults.adtList)(thrown(List[JUAdtBase](null)))
      all(JuCases.noDiscriminator.adtList)(thrown(List[JUAdtBase](null)))
      all(JuCases.discriminatorAnywhere.adtList)(thrown(List[JUAdtBase](null)))
    }
    test("null fields") {
      all(JuCases.defaults.standardTypes)(thrown(JUStandardTypes(null, BigInt(1), BigDecimal(1))))
      all(JuCases.defaults.standardTypes)(thrown(JUStandardTypes("s", null, BigDecimal(1))))
      all(JuCases.defaults.standardTypes)(thrown(JUStandardTypes("s", BigInt(1), null)))
      all(JuCases.defaults.javaTypes)(thrown(JUJavaTypes(null)))
      // Known difference: a `null` Java enum value fails with a `MatchError` (jsoniter-scala: `NullPointerException`).
      val javaEnumNull = thrown(JUJavaEnums(null, null)) _
      assertEquals(javaEnumNull(JuCases.defaults.javaEnums.upstream), "java.lang.NullPointerException")
      assertEquals(javaEnumNull(JuCases.defaults.javaEnums.optimized), "scala.MatchError")
      assertEquals(javaEnumNull(JuCases.defaults.javaEnums.runtime), "scala.MatchError")
      all(JuCases.defaults.enums)(thrown(JUEnums(null)))
    }
  }

  group(
    "enumerations with values added later (Spec: '... added after parsing of the stable ones', '... between ids')"
  ) {
    def observations[A](codecs: JoCodecs[A])(values: Seq[A], inputs: Seq[String]): Unit = {
      values.foreach(value => all(codecs)(JsoniterOracle.encode(_, value)))
      inputs.foreach(input => all(codecs)(JsoniterOracle.decode(_, input)))
    }
    test("names and ids, as values and as keys") {
      val stable = JUCelestialBody.values.toList
      // the stable values are parsed (and cached by the codecs) first
      observations(JuCases.defaults.celestialList)(
        Seq(stable),
        Seq(stable.map(_.toString).mkString("[\"", "\",\"", "\"]"))
      )
      observations(JuCases.enumId.celestialList)(Seq(stable), Seq(stable.map(_.id).mkString("[", ",", "]")))
      observations(JuCases.defaults.celestialMap)(Nil, Seq("""{"Earth":3}"""))
      observations(JuCases.enumId.celestialMap)(Nil, Seq(s"""{"${JUCelestialBody.Earth.id}":3}"""))
      val ceres = JUCelestialBody.extra("Ceres")
      val pluto = JUCelestialBody.extra("Pluto")
      val all = stable ++ List(ceres, pluto)
      observations(JuCases.defaults.celestialList)(
        Seq(all),
        Seq(all.map(_.toString).mkString("[\"", "\",\"", "\"]"), """["Earth","Eris"]""")
      )
      observations(JuCases.enumId.celestialList)(Seq(all), Seq(all.map(_.id).mkString("[", ",", "]"), "[2,100]"))
      observations(JuCases.defaults.celestialMap)(
        Seq(Map(JUCelestialBody.Earth -> 3, pluto -> 9)),
        Seq("""{"Earth":3,"Pluto":9}""", """{"Eris":1}""")
      )
      observations(JuCases.enumId.celestialMap)(
        Seq(Map(JUCelestialBody.Earth -> 3, pluto -> 9)),
        Seq(s"""{"${JUCelestialBody.Earth.id}":3,"${pluto.id}":9}""", """{"100":1}""")
      )
    }
    test("ids between the ids of the stable values") {
      observations(JuCases.defaults.spectralList)(Seq(List(JUSpectralClass.O, JUSpectralClass.G)), Seq("""["O","G"]"""))
      observations(JuCases.enumId.spectralList)(Seq(List(JUSpectralClass.O, JUSpectralClass.G)), Seq("[0,5]"))
      observations(JuCases.defaults.spectralMap)(Nil, Seq("""{"G":1}"""))
      observations(JuCases.enumId.spectralMap)(Nil, Seq("""{"5":1}"""))
      val b = JUSpectralClass.extra(2, "B")
      observations(JuCases.defaults.spectralList)(
        Seq(List(JUSpectralClass.O, b, JUSpectralClass.G)),
        Seq("""["O","B","G"]""", """["A"]""")
      )
      observations(JuCases.enumId.spectralList)(
        Seq(List(JUSpectralClass.O, b, JUSpectralClass.G)),
        Seq("[0,2,5]", "[3]")
      )
      observations(JuCases.defaults.spectralMap)(Seq(Map(b -> 2, JUSpectralClass.G -> 1)), Seq("""{"B":2,"G":1}"""))
      observations(JuCases.enumId.spectralMap)(Seq(Map(b -> 2, JUSpectralClass.G -> 1)), Seq("""{"2":2,"5":1}"""))
    }
    test("extra values of an enumeration in a field (Spec: 'serialize and deserialize enumerations')") {
      val galileo = JULocationType.extra("Galileo2")
      observations(JuCases.defaults.enums)(Seq(JUEnums(galileo)), Seq("""{"lt":"Galileo2"}"""))
      observations(JuCases.enumId.enums)(Seq(JUEnums(galileo)), Seq(s"""{"lt":${galileo.id}}"""))
      observations(JuCases.enumId.enums2)(Seq(JUEnums2(galileo)), Seq(s"""{"lt":"${galileo.id}"}"""))
    }
  }

  group("default values (Spec: 'deserialize default values of case classes that defined for fields')") {
    test("shared empty mutable defaults are not mutated by decoding") {
      val json = """{"ab":[1],"lb":[2],"mm":{"VVV":3},"lm":{"4":5},"ms":[6]}"""
      all(JuCases.defaults.sharedEmptyMutableDefaults)(JsoniterOracle.decode(_, json))
      assert(JUSharedEmptyMutables.ab.isEmpty)
      assert(JUSharedEmptyMutables.lb.isEmpty)
      assert(JUSharedEmptyMutables.mm.isEmpty)
      assert(JUSharedEmptyMutables.lm.isEmpty)
      assert(JUSharedEmptyMutables.ms.isEmpty)
    }
    test("defaults are evaluated for every decoded value (Spec: '... without memorization')") {
      all(JuCases.defaults.defaults3) { codec =>
        (1 to 4).map(_ => com.github.plokhotnyuk.jsoniter_scala.core.readFromString("{}")(codec)).toSet.size
      }
      all(JuCases.defaults.defaults2) { codec =>
        val first = com.github.plokhotnyuk.jsoniter_scala.core.readFromString("{}")(codec)
        val second = com.github.plokhotnyuk.jsoniter_scala.core.readFromString("{}")(codec)
        (
          first.ab eq second.ab,
          first.mm eq second.mm,
          first.lm eq second.lm,
          first.ms eq second.ms,
          first.mbs eq second.mbs
        )
      }
    }
  }

  group("int and long maps (Spec: 'deserialize long/int maps with unordered, negative and duplicated keys')") {
    test("long maps") {
      val rnd = new scala.util.Random(42)
      Seq(1, 2, 3, 15, 16, 17, 18, 33, 79, 80, 81, 100, 1000).foreach { n =>
        (0 to 2).foreach { _ =>
          val kvs = (0 until n).map { i =>
            val k = rnd.nextInt(5) match {
              case 0 => rnd.nextLong()
              case 1 => (rnd.nextInt(n * 2) - n).toLong
              case 2 => if (rnd.nextBoolean()) Long.MinValue else Long.MaxValue
              case 3 => rnd.nextInt().toLong << 32
              case _ => i.toLong
            }
            (k, rnd.nextInt())
          }
          val asObject = kvs.map(kv => s""""${kv._1}":${kv._2}""").mkString("{", ",", "}")
          val asArray = kvs.map(kv => s"[${kv._1},${kv._2}]").mkString("[", ",", "]")
          def observe(json: String)(
              codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[collection.immutable.LongMap[Int]]
          ): Any = {
            val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(json)(codec)
            (read.toList, com.github.plokhotnyuk.jsoniter_scala.core.writeToString(read)(codec))
          }
          all(JuCases.noMapLimit.longMap)(observe(asObject))
          all(JuCases.noMapLimitAsArray.longMap)(observe(asArray))
        }
      }
    }
    test("int maps") {
      val rnd = new scala.util.Random(42)
      Seq(1, 2, 3, 15, 16, 17, 18, 33, 100, 1000).foreach { n =>
        (0 to 2).foreach { _ =>
          val kvs = (0 until n).map { i =>
            val k = rnd.nextInt(4) match {
              case 0 => rnd.nextInt()
              case 1 => rnd.nextInt(n * 2) - n
              case 2 => if (rnd.nextBoolean()) Int.MinValue else Int.MaxValue
              case _ => i
            }
            (k, rnd.nextInt())
          }
          val asObject = kvs.map(kv => s""""${kv._1}":${kv._2}""").mkString("{", ",", "}")
          val asArray = kvs.map(kv => s"[${kv._1},${kv._2}]").mkString("[", ",", "]")
          def observe(json: String)(
              codec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[collection.immutable.IntMap[Int]]
          ): Any = {
            val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(json)(codec)
            (read.toList, com.github.plokhotnyuk.jsoniter_scala.core.writeToString(read)(codec))
          }
          all(JuCases.noMapLimit.intMap)(observe(asObject))
          all(JuCases.noMapLimitAsArray.intMap)(observe(asArray))
        }
      }
    }
  }

  group("gaps") {
    // Hearth (Scala 2) reports the case field of a back-quoted name which starts or ends with a space (upstream's
    // `Operators` fixture: \` =<>!#%^&|*/\\~+-:$ \`) under another name than its constructor parameter, so the
    // field is not found. See docs/research/hearth-findings-jsoniter-parity-2026-11.md (6).
    test("a field name which starts or ends with a space is not derivable on Scala 2") {
      val errors =
        compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[JUSpacedName]")
      assertEquals(errors.contains("should be defined as 'val' or 'var'"), !JoScalaVersion.isScala3, errors)
    }
  }

  group("gaps (continued)") {
    // Hearth (Scala 3) fails on a Java enum nested in a Java class (upstream's `Levels.InnerLevel` fixture). See
    // docs/research/hearth-findings-jsoniter-parity-2026-11.md (3).
    test("a Java enum nested in a Java class is not derivable on Scala 3") {
      val errors =
        compileErrors("hearth.kindlings.jsoniterderivation.KindlingsJsonValueCodec.derived[java.lang.Thread.State]")
      assertEquals(errors.contains("expected a term symbol"), JoScalaVersion.isScala3, errors)
    }
  }

  group("collections keep their implementation and ordering") {
    test("sorted collections are rebuilt as sorted ones") {
      val json =
        """{"l":[["1"]],"nl":[[1]],"q":[[4]],"is":[[7,6,5]],"s":[[9.9,8.9]],"v":[[10,11]]}"""
      all(JuCases.defaults.immutableIterables) { codec =>
        val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(json)(codec)
        (
          read.l.head.getClass.getName,
          read.q.getClass.getName,
          read.is.head.getClass.getName,
          read.s.getClass.getName,
          read.s.head.getClass.getName,
          read.v.getClass.getName
        )
      }
    }
    test("mutable collections are rebuilt as the declared implementations") {
      val json =
        """{"ml":[["3","2","1"]],"ab":[[4]],"as":[[6,5]],"b":[[7.7]],"lb":[[10,9]],"is":[[1.5]],"ub":[[1]]}"""
      val differences = differingElements(JuCases.defaults.mutableIterables) { codec =>
        val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(json)(codec)
        (
          read.ml.getClass.getName,
          read.ml.head.getClass.getName,
          read.ab.getClass.getName,
          read.ab.head.getClass.getName,
          read.as.getClass.getName,
          read.as.head.getClass.getName,
          read.b.getClass.getName,
          read.b.head.getClass.getName,
          read.lb.getClass.getName,
          read.lb.head.getClass.getName,
          read.is.getClass.getName,
          read.is.head.getClass.getName,
          read.ub.getClass.getName,
          read.ub.head.getClass.getName
        )
      }
      // Hearth bug (docs/research/hearth-findings-jsoniter-parity-2026-11.md, 1): `mutable.IndexedSeq[ArrayStack[_]]`
      // is built with the `Factory` of its element type's companion (`mutable.Stack`).
      assertEquals(
        differences,
        List("_11: kindlings=scala.collection.mutable.Stack upstream=scala.collection.mutable.ArrayBuffer")
      )
    }
    test("maps are rebuilt as the declared implementations") {
      val differences = differingElements(JuCases.defaults.mutableMaps) { codec =>
        val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
          """{"hm":{"true":{"1.1":1}},"m":{"1.1":{"2":"2"}},"ohm":{"1.1":{"3":3.3},"2.2":{}}}"""
        )(codec)
        (
          read.hm.getClass.getName,
          read.hm.head._2.getClass.getName,
          read.m.getClass.getName,
          read.m.head._2.getClass.getName,
          read.ohm.getClass.getName,
          read.ohm.head._2.getClass.getName
        )
      }
      // Hearth bug (docs/research/hearth-findings-jsoniter-parity-2026-11.md, 1): `mutable.Map[_, mutable.ListMap[_, _]]`
      // is built with the `Factory` of its value type's companion (`mutable.ListMap`).
      assertEquals(
        differences,
        List("_3: kindlings=scala.collection.mutable.ListMap upstream=scala.collection.mutable.HashMap")
      )
      all(JuCases.defaults.immutableMaps) { codec =>
        val read = com.github.plokhotnyuk.jsoniter_scala.core.readFromString(
          """{"m":{"1":1.1},"hm":{"2":{"V":2},"3":{"X":3}},"sm":{"4":{"4":4.4},"5":{}}}"""
        )(codec)
        (
          read.m.getClass.getName,
          read.hm.getClass.getName,
          read.hm.head._2.getClass.getName,
          read.sm.getClass.getName,
          read.sm.head._2.getClass.getName
        )
      }
    }
  }
}
