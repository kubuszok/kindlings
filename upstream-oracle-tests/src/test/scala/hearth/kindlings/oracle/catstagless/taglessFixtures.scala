package hearth.kindlings.oracle.catstagless

// Tagless-final algebras shared by the cats-tagless-derivation (Kindlings) and cats-tagless (upstream) sides of
// `CatsTaglessOracleSpec`, one method shape per algebra so that an unsupported shape is isolated.

trait TgSimple[F[_]] {
  def get(id: Int): F[String]
  def count: F[Int]
  def put(id: Int, name: String): F[Unit]
  def label(x: Int): String
}

trait TgCurried[F[_]] {
  def curried(a: Int)(b: String): F[String]
}

trait TgByName[F[_]] {
  def byName(x: => Int): F[Int]
}

trait TgGeneric[F[_]] {
  def generic[A](a: A): F[List[A]]
}

trait TgDefault[F[_]] {
  def withDefault(a: Int, b: Int = 3): F[Int]
}

trait TgVarargs[F[_]] {
  def sum(xs: Int*): F[Int]
}

trait TgConsumer[F[_]] {
  def consume(data: F[Int]): String
}

trait TgTransform[F[_]] {
  def transform(input: F[Int]): F[String]
}
