package hearth.kindlings.xmlderivation

import hearth.kindlings.xmlderivation.annotations.{xmlUnwrapped, xmlWrapper}

// An enum of 100 case objects: its name <-> value code must not be too large for the JIT.
sealed trait XmlWideEnum
case object XmlWideCase0 extends XmlWideEnum
case object XmlWideCase1 extends XmlWideEnum
case object XmlWideCase2 extends XmlWideEnum
case object XmlWideCase3 extends XmlWideEnum
case object XmlWideCase4 extends XmlWideEnum
case object XmlWideCase5 extends XmlWideEnum
case object XmlWideCase6 extends XmlWideEnum
case object XmlWideCase7 extends XmlWideEnum
case object XmlWideCase8 extends XmlWideEnum
case object XmlWideCase9 extends XmlWideEnum
case object XmlWideCase10 extends XmlWideEnum
case object XmlWideCase11 extends XmlWideEnum
case object XmlWideCase12 extends XmlWideEnum
case object XmlWideCase13 extends XmlWideEnum
case object XmlWideCase14 extends XmlWideEnum
case object XmlWideCase15 extends XmlWideEnum
case object XmlWideCase16 extends XmlWideEnum
case object XmlWideCase17 extends XmlWideEnum
case object XmlWideCase18 extends XmlWideEnum
case object XmlWideCase19 extends XmlWideEnum
case object XmlWideCase20 extends XmlWideEnum
case object XmlWideCase21 extends XmlWideEnum
case object XmlWideCase22 extends XmlWideEnum
case object XmlWideCase23 extends XmlWideEnum
case object XmlWideCase24 extends XmlWideEnum
case object XmlWideCase25 extends XmlWideEnum
case object XmlWideCase26 extends XmlWideEnum
case object XmlWideCase27 extends XmlWideEnum
case object XmlWideCase28 extends XmlWideEnum
case object XmlWideCase29 extends XmlWideEnum
case object XmlWideCase30 extends XmlWideEnum
case object XmlWideCase31 extends XmlWideEnum
case object XmlWideCase32 extends XmlWideEnum
case object XmlWideCase33 extends XmlWideEnum
case object XmlWideCase34 extends XmlWideEnum
case object XmlWideCase35 extends XmlWideEnum
case object XmlWideCase36 extends XmlWideEnum
case object XmlWideCase37 extends XmlWideEnum
case object XmlWideCase38 extends XmlWideEnum
case object XmlWideCase39 extends XmlWideEnum
case object XmlWideCase40 extends XmlWideEnum
case object XmlWideCase41 extends XmlWideEnum
case object XmlWideCase42 extends XmlWideEnum
case object XmlWideCase43 extends XmlWideEnum
case object XmlWideCase44 extends XmlWideEnum
case object XmlWideCase45 extends XmlWideEnum
case object XmlWideCase46 extends XmlWideEnum
case object XmlWideCase47 extends XmlWideEnum
case object XmlWideCase48 extends XmlWideEnum
case object XmlWideCase49 extends XmlWideEnum
case object XmlWideCase50 extends XmlWideEnum
case object XmlWideCase51 extends XmlWideEnum
case object XmlWideCase52 extends XmlWideEnum
case object XmlWideCase53 extends XmlWideEnum
case object XmlWideCase54 extends XmlWideEnum
case object XmlWideCase55 extends XmlWideEnum
case object XmlWideCase56 extends XmlWideEnum
case object XmlWideCase57 extends XmlWideEnum
case object XmlWideCase58 extends XmlWideEnum
case object XmlWideCase59 extends XmlWideEnum
case object XmlWideCase60 extends XmlWideEnum
case object XmlWideCase61 extends XmlWideEnum
case object XmlWideCase62 extends XmlWideEnum
case object XmlWideCase63 extends XmlWideEnum
case object XmlWideCase64 extends XmlWideEnum
case object XmlWideCase65 extends XmlWideEnum
case object XmlWideCase66 extends XmlWideEnum
case object XmlWideCase67 extends XmlWideEnum
case object XmlWideCase68 extends XmlWideEnum
case object XmlWideCase69 extends XmlWideEnum
case object XmlWideCase70 extends XmlWideEnum
case object XmlWideCase71 extends XmlWideEnum
case object XmlWideCase72 extends XmlWideEnum
case object XmlWideCase73 extends XmlWideEnum
case object XmlWideCase74 extends XmlWideEnum
case object XmlWideCase75 extends XmlWideEnum
case object XmlWideCase76 extends XmlWideEnum
case object XmlWideCase77 extends XmlWideEnum
case object XmlWideCase78 extends XmlWideEnum
case object XmlWideCase79 extends XmlWideEnum
case object XmlWideCase80 extends XmlWideEnum
case object XmlWideCase81 extends XmlWideEnum
case object XmlWideCase82 extends XmlWideEnum
case object XmlWideCase83 extends XmlWideEnum
case object XmlWideCase84 extends XmlWideEnum
case object XmlWideCase85 extends XmlWideEnum
case object XmlWideCase86 extends XmlWideEnum
case object XmlWideCase87 extends XmlWideEnum
case object XmlWideCase88 extends XmlWideEnum
case object XmlWideCase89 extends XmlWideEnum
case object XmlWideCase90 extends XmlWideEnum
case object XmlWideCase91 extends XmlWideEnum
case object XmlWideCase92 extends XmlWideEnum
case object XmlWideCase93 extends XmlWideEnum
case object XmlWideCase94 extends XmlWideEnum
case object XmlWideCase95 extends XmlWideEnum
case object XmlWideCase96 extends XmlWideEnum
case object XmlWideCase97 extends XmlWideEnum
case object XmlWideCase98 extends XmlWideEnum
case object XmlWideCase99 extends XmlWideEnum

// A case class with 100 defaulted fields in wrappers, unwrapped, and enums as elements and attributes.
case class XmlWideAnnotated(
    @xmlWrapper("w0") f0: List[Int] = Nil,
    @xmlUnwrapped f1: List[String] = Nil,
    f2: XmlWideEnum = XmlWideCase2,
    f3: Option[XmlWideEnum] = None,
    @xmlWrapper("w4") f4: Option[String] = None,
    @xmlWrapper("w5") f5: List[Int] = Nil,
    @xmlUnwrapped f6: List[String] = Nil,
    f7: XmlWideEnum = XmlWideCase7,
    f8: Option[XmlWideEnum] = None,
    @xmlWrapper("w9") f9: Option[String] = None,
    @xmlWrapper("w10") f10: List[Int] = Nil,
    @xmlUnwrapped f11: List[String] = Nil,
    f12: XmlWideEnum = XmlWideCase12,
    f13: Option[XmlWideEnum] = None,
    @xmlWrapper("w14") f14: Option[String] = None,
    @xmlWrapper("w15") f15: List[Int] = Nil,
    @xmlUnwrapped f16: List[String] = Nil,
    f17: XmlWideEnum = XmlWideCase17,
    f18: Option[XmlWideEnum] = None,
    @xmlWrapper("w19") f19: Option[String] = None,
    @xmlWrapper("w20") f20: List[Int] = Nil,
    @xmlUnwrapped f21: List[String] = Nil,
    f22: XmlWideEnum = XmlWideCase22,
    f23: Option[XmlWideEnum] = None,
    @xmlWrapper("w24") f24: Option[String] = None,
    @xmlWrapper("w25") f25: List[Int] = Nil,
    @xmlUnwrapped f26: List[String] = Nil,
    f27: XmlWideEnum = XmlWideCase27,
    f28: Option[XmlWideEnum] = None,
    @xmlWrapper("w29") f29: Option[String] = None,
    @xmlWrapper("w30") f30: List[Int] = Nil,
    @xmlUnwrapped f31: List[String] = Nil,
    f32: XmlWideEnum = XmlWideCase32,
    f33: Option[XmlWideEnum] = None,
    @xmlWrapper("w34") f34: Option[String] = None,
    @xmlWrapper("w35") f35: List[Int] = Nil,
    @xmlUnwrapped f36: List[String] = Nil,
    f37: XmlWideEnum = XmlWideCase37,
    f38: Option[XmlWideEnum] = None,
    @xmlWrapper("w39") f39: Option[String] = None,
    @xmlWrapper("w40") f40: List[Int] = Nil,
    @xmlUnwrapped f41: List[String] = Nil,
    f42: XmlWideEnum = XmlWideCase42,
    f43: Option[XmlWideEnum] = None,
    @xmlWrapper("w44") f44: Option[String] = None,
    @xmlWrapper("w45") f45: List[Int] = Nil,
    @xmlUnwrapped f46: List[String] = Nil,
    f47: XmlWideEnum = XmlWideCase47,
    f48: Option[XmlWideEnum] = None,
    @xmlWrapper("w49") f49: Option[String] = None,
    @xmlWrapper("w50") f50: List[Int] = Nil,
    @xmlUnwrapped f51: List[String] = Nil,
    f52: XmlWideEnum = XmlWideCase52,
    f53: Option[XmlWideEnum] = None,
    @xmlWrapper("w54") f54: Option[String] = None,
    @xmlWrapper("w55") f55: List[Int] = Nil,
    @xmlUnwrapped f56: List[String] = Nil,
    f57: XmlWideEnum = XmlWideCase57,
    f58: Option[XmlWideEnum] = None,
    @xmlWrapper("w59") f59: Option[String] = None,
    @xmlWrapper("w60") f60: List[Int] = Nil,
    @xmlUnwrapped f61: List[String] = Nil,
    f62: XmlWideEnum = XmlWideCase62,
    f63: Option[XmlWideEnum] = None,
    @xmlWrapper("w64") f64: Option[String] = None,
    @xmlWrapper("w65") f65: List[Int] = Nil,
    @xmlUnwrapped f66: List[String] = Nil,
    f67: XmlWideEnum = XmlWideCase67,
    f68: Option[XmlWideEnum] = None,
    @xmlWrapper("w69") f69: Option[String] = None,
    @xmlWrapper("w70") f70: List[Int] = Nil,
    @xmlUnwrapped f71: List[String] = Nil,
    f72: XmlWideEnum = XmlWideCase72,
    f73: Option[XmlWideEnum] = None,
    @xmlWrapper("w74") f74: Option[String] = None,
    @xmlWrapper("w75") f75: List[Int] = Nil,
    @xmlUnwrapped f76: List[String] = Nil,
    f77: XmlWideEnum = XmlWideCase77,
    f78: Option[XmlWideEnum] = None,
    @xmlWrapper("w79") f79: Option[String] = None,
    @xmlWrapper("w80") f80: List[Int] = Nil,
    @xmlUnwrapped f81: List[String] = Nil,
    f82: XmlWideEnum = XmlWideCase82,
    f83: Option[XmlWideEnum] = None,
    @xmlWrapper("w84") f84: Option[String] = None,
    @xmlWrapper("w85") f85: List[Int] = Nil,
    @xmlUnwrapped f86: List[String] = Nil,
    f87: XmlWideEnum = XmlWideCase87,
    f88: Option[XmlWideEnum] = None,
    @xmlWrapper("w89") f89: Option[String] = None,
    @xmlWrapper("w90") f90: List[Int] = Nil,
    @xmlUnwrapped f91: List[String] = Nil,
    f92: XmlWideEnum = XmlWideCase92,
    f93: Option[XmlWideEnum] = None,
    @xmlWrapper("w94") f94: Option[String] = None,
    @xmlWrapper("w95") f95: List[Int] = Nil,
    @xmlUnwrapped f96: List[String] = Nil,
    f97: XmlWideEnum = XmlWideCase97,
    f98: Option[XmlWideEnum] = None,
    @xmlWrapper("w99") f99: Option[String] = None
)
