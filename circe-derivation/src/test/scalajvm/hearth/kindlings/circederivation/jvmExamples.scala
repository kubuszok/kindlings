package hearth.kindlings.circederivation

// java.time field test type — JVM-only because java.time is not available on JS/Native
case class WithInstant(name: String, ts: java.time.Instant)
