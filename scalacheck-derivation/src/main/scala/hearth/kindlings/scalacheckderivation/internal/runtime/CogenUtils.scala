package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.{Cogen, Gen}
import org.scalacheck.rng.Seed

object CogenUtils {

  /** Identity Cogen — returns seed unchanged. Used for singletons and empty case classes. */
  def cogenIdentity[A]: Cogen[A] =
    Cogen((seed, _) => seed)

  def cogenOption[A](innerCogen: Cogen[A]): Cogen[Option[A]] =
    Cogen { (seed, opt) =>
      opt match {
        case None    => seed.next
        case Some(a) => innerCogen.perturb(seed.next, a)
      }
    }

  /** Perturbs the seed with every element, reading the value through the provider's `asIterable` (so containers that
    * are not Scala `Iterable`s, e.g. `cats.data.NonEmptyList`, work).
    */
  def cogenCollectionWithIterable[Item, A](elemCogen: Cogen[Item], toIterable: A => Iterable[Item]): Cogen[A] =
    Cogen { (seed, value) =>
      toIterable(value).foldLeft(seed)((s, elem) => elemCogen.perturb(s, elem))
    }

  /** Cogen for a map entry: perturbs the seed with the key, then the value. Pair access is provided by the macro. */
  def cogenPair[Pair, K, V](keyCogen: Cogen[K], valueCogen: Cogen[V], key: Pair => K, value: Pair => V): Cogen[Pair] =
    Cogen((seed, p) => valueCogen.perturb(keyCogen.perturb(seed, key(p)), value(p)))

  @deprecated(
    "Kept for binary compatibility of code compiled against kindlings 0.3.x; the macro no longer emits it",
    "0.3.3"
  )
  def cogenCollection(elemCogen: Cogen[Any]): Cogen[Any] =
    Cogen { (seed, value) =>
      val coll = value.asInstanceOf[Iterable[Any]]
      coll.foldLeft(seed)((s, elem) => elemCogen.perturb(s, elem))
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

  /** Cogen for Map: perturbs seed with each key-value pair. */
  @deprecated(
    "Kept for binary compatibility of code compiled against kindlings 0.3.x; the macro no longer emits it",
    "0.3.3"
  )
  def cogenMap(keyCogen: Cogen[Any], valueCogen: Cogen[Any]): Cogen[Any] =
    Cogen { (seed, value) =>
      val map = value.asInstanceOf[Map[Any, Any]]
      map.foldLeft(seed) { case (s, (k, v)) =>
        valueCogen.perturb(keyCogen.perturb(s, k), v)
      }
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

  /** Cogen for an enum/sealed trait: perturbs the seed with the index of the value's case (from the macro-generated
    * `ordinal`, a real pattern match) and then with the matching case's Cogen. Unlike [[cogenEnum]] it uses neither
    * `hashCode` (identity-based for Java enums and function fields, so not reproducible across runs) nor try-each-case
    * dispatch.
    */
  def cogenEnumByOrdinal[A](caseCogens: List[Cogen[A]], ordinal: A => Int): Cogen[A] = {
    lazy val cogens = caseCogens.toArray
    Cogen { (seed, value) =>
      val index = ordinal(value)
      if (index < 0 || index >= cogens.length) seed
      else cogens(index).perturb(Cogen.cogenInt.perturb(seed, index), value)
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

  @deprecated(
    "Kept for binary compatibility of code compiled against kindlings 0.3.x; the macro no longer emits it",
    "0.3.3"
  )
  def cogenEnum[A](caseCogens: List[Cogen[A]]): Cogen[A] = {
    def perturbEnum(seed: org.scalacheck.rng.Seed, value: A): org.scalacheck.rng.Seed = {
      // Perturb with both class name (type discriminator) and value hash (instance discriminator).
      // Class name alone is insufficient for Java enums where all constants share the same class.
      val typeSeed = Cogen.cogenLong.perturb(seed, value.getClass.getName.hashCode.toLong)
      val ordinalSeed = Cogen.cogenLong.perturb(typeSeed, value.hashCode().toLong)
      val iter = caseCogens.iterator
      while (iter.hasNext) {
        val caseCogen = iter.next()
        try return caseCogen.perturb(ordinalSeed, value)
        catch { case _: ClassCastException => () }
      }
      ordinalSeed
    }
    Cogen(perturbEnum)
  }
}
