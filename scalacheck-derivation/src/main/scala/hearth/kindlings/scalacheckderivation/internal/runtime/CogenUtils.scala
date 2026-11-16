package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.{Cogen, Gen}
import org.scalacheck.rng.Seed

object CogenUtils {

  /** Identity Cogen — returns seed unchanged. Used for singletons and empty case classes. */
  def cogenIdentity[A]: Cogen[A] =
    Cogen((seed, _) => seed)

  /** Perturbs the seed like a case class without fields does in scalacheck-shapeless and scalacheck-derived: with
    * `Cogen[Unit]`. Used for singletons (case objects, parameterless enum cases) and case classes without fields.
    */
  def cogenNoFields[A]: Cogen[A] =
    Cogen((seed, _) => Cogen.cogenUnit.perturb(seed, ()))

  /** Like ScalaCheck's `Cogen.cogenOption`. */
  def cogenOption[A](innerCogen: Cogen[A]): Cogen[Option[A]] =
    Cogen((seed, opt) => opt.fold(seed)(a => innerCogen.perturb(seed.next, a)))

  /** Perturbs the seed with every element, reading the value through the provider's `asIterable` (so containers that
    * are not Scala `Iterable`s, e.g. `cats.data.NonEmptyList`, work).
    */
  def cogenCollectionWithIterable[Item, A](elemCogen: Cogen[Item], toIterable: A => Iterable[Item]): Cogen[A] =
    // Like ScalaCheck's `Cogen.it`: the final `next` marks the end of the collection, so nested collections with the
    // same elements in different groupings (`List(List(1), Nil)` vs `List(Nil, List(1))`) perturb differently.
    Cogen { (seed, value) =>
      toIterable(value).foldLeft(seed)((s, elem) => elemCogen.perturb(s, elem)).next
    }

  /** Cogen for a map entry: perturbs the seed with the key, then the value. Pair access is provided by the macro. */
  def cogenPair[Pair, K, V](keyCogen: Cogen[K], valueCogen: Cogen[V], key: Pair => K, value: Pair => V): Cogen[Pair] =
    Cogen((seed, p) => valueCogen.perturb(keyCogen.perturb(seed, key(p)), value(p)))

  /** Perturbs the seed with every field in order and then - when `terminate` - with `Cogen[Unit]`, like
    * scalacheck-shapeless and scalacheck-derived do for case classes (tuples, like ScalaCheck's `Cogen.tupleN`, do
    * not).
    */
  def cogenCaseClass[A](fieldCogens: List[Cogen[Any]], numFields: Int, terminate: Boolean): Cogen[A] = {
    val fields = cogenCaseClass[A](fieldCogens, numFields)
    if (terminate) Cogen((seed, value) => Cogen.cogenUnit.perturb(fields.perturb(seed, value), ()))
    else fields
  }

  def cogenCaseClass[A](fieldCogens: List[Cogen[Any]], numFields: Int): Cogen[A] =
    Cogen { (seed, value) =>
      val product = value.asInstanceOf[Product]
      var s = seed
      var i = 0
      while (i < numFields) {
        s = fieldCogens(i).perturb(s, product.productElement(i))
        i += 1
      }
      s
    }

  /** Cogen for a `FunctionN` like ScalaCheck's `Cogen.functionN`: applies the function to arguments generated from the
    * seed and perturbs the seed with the result. The instances are by-name (and memoized) so that recursive types are
    * not evaluated while the Cogen is built.
    */
  def cogenFunction[F](arity: Int, argGens: => List[Gen[Any]], resultCogen: => Cogen[Any]): Cogen[F] = {
    lazy val gens = argGens.toArray
    lazy val cogen = resultCogen
    Cogen { (seed0, function) =>
      var seed = seed0
      val args = new Array[Any](gens.length)
      var i = 0
      while (i < gens.length) {
        args(i) = gens(i).pureApply(Gen.Parameters.default, seed)
        seed = seed.next
        i += 1
      }
      cogen.perturb(seed, FunctionArity(arity, function, args))
    }
  }

  /** Cogen for a `PartialFunction` like ScalaCheck's `cogenPartialFunction`: the Cogen of its lifted `A => Option[B]`.
    */
  def cogenPartialFunction[A, B](argGen: => Gen[A], resultCogen: => Cogen[Option[B]]): Cogen[PartialFunction[A, B]] =
    cogenFunction[A => Option[B]](1, List(argGen.asInstanceOf[Gen[Any]]), resultCogen.asInstanceOf[Cogen[Any]])
      .contramap(_.lift)

  /** Lazy Cogen — defers evaluation of the underlying Cogen until perturb is called. Breaks infinite recursion for
    * recursive types where Cogen.perturb is strict.
    */
  def cogenLazy[A](cogen: => Cogen[A]): Cogen[A] =
    Cogen((seed, a) => cogen.perturb(seed, a))

  /** Cogen for value types: unwrap to inner type and delegate to inner Cogen. */
  def cogenMapped(innerCogen: Cogen[Any], unwrap: Any => Any): Cogen[Any] =
    Cogen { (seed, value) =>
      innerCogen.perturb(seed, unwrap(value))
    }

  /** Cogen for an enum/sealed trait: perturbs the seed with the matching case's Cogen and then with the index of the
    * value's case (from the macro-generated `ordinal`, a real pattern match), like scalacheck-derived. It uses neither
    * `hashCode` (identity-based for Java enums and function fields, so not reproducible across runs) nor try-each-case
    * dispatch.
    */
  def cogenEnumByOrdinal[A](caseCogens: List[Cogen[A]], ordinal: A => Int): Cogen[A] = {
    lazy val cogens = caseCogens.toArray
    Cogen { (seed, value) =>
      val index = ordinal(value)
      if (index < 0 || index >= cogens.length) seed
      // Like scalacheck-derived: the case's Cogen first, then the case's index.
      else Cogen.cogenInt.perturb(cogens(index).perturb(seed, value), index)
    }
  }

  /** Perturbs the seed with every element order-independently, for sets and maps whose equal values may iterate in
    * different orders: each element perturbs a fixed seed, the results are summed (commutative), and the input seed is
    * perturbed with the element count and the sum.
    */
  def cogenUnorderedCollection[Item, A](elemCogen: Cogen[Item], toIterable: A => Iterable[Item]): Cogen[A] =
    Cogen { (seed, value) =>
      var sum = 0L
      var count = 0
      val it = toIterable(value).iterator
      while (it.hasNext) {
        sum += elemCogen.perturb(Seed(0L), it.next()).long._1
        count += 1
      }
      Cogen.cogenLong.perturb(Cogen.cogenInt.perturb(seed, count), sum)
    }
}
