package hearth.kindlings.scalacheckderivation.internal.runtime

import org.scalacheck.Cogen

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
  def cogenMap(keyCogen: Cogen[Any], valueCogen: Cogen[Any]): Cogen[Any] =
    Cogen { (seed, value) =>
      val map = value.asInstanceOf[Map[Any, Any]]
      map.foldLeft(seed) { case (s, (k, v)) =>
        valueCogen.perturb(keyCogen.perturb(s, k), v)
      }
    }

  /** Cogen for value types: unwrap to inner type and delegate to inner Cogen. */
  def cogenMapped(innerCogen: Cogen[Any], unwrap: Any => Any): Cogen[Any] =
    Cogen { (seed, value) =>
      innerCogen.perturb(seed, unwrap(value))
    }

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
