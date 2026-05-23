package hearth.kindlings.xmlderivation.internal.runtime

import hearth.kindlings.xmlderivation.XmlDecodingError

private[kindlings] class XmlCollectionBuildException(val errors: List[XmlDecodingError])
    extends RuntimeException("XML collection decoding errors")
