package hearth.kindlings.ubjsonderivation

final class UBJsonReaderException(val message: String, val position: Int)
    extends RuntimeException(s"$message (at position $position)")
