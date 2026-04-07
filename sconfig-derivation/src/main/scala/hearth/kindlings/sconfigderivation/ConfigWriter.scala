package hearth.kindlings.sconfigderivation

import org.ekrich.config.{ConfigValue, ConfigValueFactory}

/** Type class for encoding a value of type `A` into a [[org.ekrich.config.ConfigValue]]. */
trait ConfigWriter[A] {
  def to(value: A): ConfigValue

  def contramap[B](f: B => A): ConfigWriter[B] = new ConfigWriter[B] {
    def to(b: B): ConfigValue = ConfigWriter.this.to(f(b))
  }
}

/** Companion. Built-in instances live here directly (not in mixed-in traits). See
  * [[ConfigReader]] for the rationale.
  */
object ConfigWriter extends ConfigWriterCompanionCompat {

  def apply[A](implicit w: ConfigWriter[A]): ConfigWriter[A] = w

  /** Special type — if its implicit is in scope then macros will log the derivation process. */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation

  // --- Built-in primitive instances ---

  implicit val stringWriter: ConfigWriter[String] = new ConfigWriter[String] {
    def to(s: String): ConfigValue = ConfigValueFactory.fromAnyRef(s)
  }
  implicit val booleanWriter: ConfigWriter[Boolean] = new ConfigWriter[Boolean] {
    def to(b: Boolean): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Boolean.valueOf(b))
  }
  implicit val intWriter: ConfigWriter[Int] = new ConfigWriter[Int] {
    def to(i: Int): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(i))
  }
  implicit val longWriter: ConfigWriter[Long] = new ConfigWriter[Long] {
    def to(l: Long): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Long.valueOf(l))
  }
  implicit val shortWriter: ConfigWriter[Short] = new ConfigWriter[Short] {
    def to(s: Short): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Short.valueOf(s))
  }
  implicit val byteWriter: ConfigWriter[Byte] = new ConfigWriter[Byte] {
    def to(b: Byte): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Byte.valueOf(b))
  }
  implicit val floatWriter: ConfigWriter[Float] = new ConfigWriter[Float] {
    def to(f: Float): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Float.valueOf(f))
  }
  implicit val doubleWriter: ConfigWriter[Double] = new ConfigWriter[Double] {
    def to(d: Double): ConfigValue = ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(d))
  }
  implicit val charWriter: ConfigWriter[Char] = new ConfigWriter[Char] {
    def to(c: Char): ConfigValue = ConfigValueFactory.fromAnyRef(c.toString)
  }
  implicit val bigDecimalWriter: ConfigWriter[BigDecimal] = new ConfigWriter[BigDecimal] {
    def to(bd: BigDecimal): ConfigValue = ConfigValueFactory.fromAnyRef(bd.toString)
  }
  implicit val bigIntWriter: ConfigWriter[BigInt] = new ConfigWriter[BigInt] {
    def to(bi: BigInt): ConfigValue = ConfigValueFactory.fromAnyRef(bi.toString)
  }

  implicit def optionWriter[A](implicit inner: ConfigWriter[A]): ConfigWriter[Option[A]] =
    new ConfigWriter[Option[A]] {
      def to(opt: Option[A]): ConfigValue = opt match {
        case Some(a) => inner.to(a)
        case None    => ConfigValueFactory.fromAnyRef(null)
      }
    }

  // Collection / map writers are synthesised by the macro directly via
  // `HandleAsCollectionRule` and `HandleAsMapRule`; we do not expose them as implicits
  // on the companion because generic implicits with unbounded type parameters cause
  // Scala 2 implicit search to diverge.
}
