package hearth.kindlings.catsintegration.internal.compiletime

import hearth.MacroCommons
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Registers IsCollection providers for Cats data types: NonEmptyList, NonEmptyVector, NonEmptyChain, Chain. Registers
  * IsMap provider (via IsCollection) for NonEmptyMap. Registers IsCollection provider for NonEmptySet.
  *
  * All `Expr.quote` blocks are inside helper methods where element types are regular type parameters (not
  * path-dependent from `elem.Underlying`). This avoids Scala 2 reification failures ("not found: value elem").
  */
final class CatsCollectionAndMapProviders extends StandardMacroExtension { loader =>

  @scala.annotation.nowarn("msg=is never used")
  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    val ListType = Type.Ctor1.of[List]
    val BuilderType = Type.Ctor2.of[scala.collection.mutable.Builder]
    val OrderCtor = Type.Ctor1.of[cats.kernel.Order]
    val Tuple2Ctor = Type.Ctor2.of[Tuple2]

    // --- Helper methods with regular type parameters (safe for cross-quotes on Scala 2) ---

    @scala.annotation.nowarn("msg=is never used")
    def mkNEL[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
      implicit val eType: Type[E] = elemType
      implicit val aType: Type[A] = AT
      implicit val listEType: Type[List[E]] = ListType[E]
      implicit val builderType: Type[scala.collection.mutable.Builder[E, List[E]]] = BuilderType[E, List[E]]

      Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
        override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
          Expr.quote(Expr.splice(value).asInstanceOf[cats.data.NonEmptyList[E]].toList)
        override type CtorResult = List[E]
        implicit override val CtorResult: Type[CtorResult] = listEType
        override def factory: Expr[scala.collection.Factory[E, CtorResult]] = Expr.quote {
          new scala.collection.Factory[E, List[E]] {
            override def newBuilder: scala.collection.mutable.Builder[E, List[E]] = List.newBuilder[E]
            override def fromSpecific(it: IterableOnce[E]): List[E] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[E, CtorResult], A] =
          CtorLikeOf.EitherStringOrValue(
            (builder: Expr[scala.collection.mutable.Builder[E, List[E]]]) =>
              Expr.quote {
                val list = Expr.splice(builder).result()
                list match {
                  case head :: tail => Right(cats.data.NonEmptyList(head, tail).asInstanceOf[A])
                  case Nil          => Left("Cannot create NonEmptyList from empty collection")
                }
              },
            None
          )
      })
    }

    @scala.annotation.nowarn("msg=is never used")
    def mkNEV[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
      implicit val eType: Type[E] = elemType
      implicit val aType: Type[A] = AT
      implicit val listEType: Type[List[E]] = ListType[E]
      implicit val builderType: Type[scala.collection.mutable.Builder[E, List[E]]] = BuilderType[E, List[E]]

      Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
        override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
          Expr.quote(Expr.splice(value).asInstanceOf[cats.data.NonEmptyVector[E]].toVector)
        override type CtorResult = List[E]
        implicit override val CtorResult: Type[CtorResult] = listEType
        override def factory: Expr[scala.collection.Factory[E, CtorResult]] = Expr.quote {
          new scala.collection.Factory[E, List[E]] {
            override def newBuilder: scala.collection.mutable.Builder[E, List[E]] = List.newBuilder[E]
            override def fromSpecific(it: IterableOnce[E]): List[E] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[E, CtorResult], A] =
          CtorLikeOf.EitherStringOrValue(
            (builder: Expr[scala.collection.mutable.Builder[E, List[E]]]) =>
              Expr.quote {
                val list = Expr.splice(builder).result()
                if (list.nonEmpty)
                  Right(cats.data.NonEmptyVector.fromVectorUnsafe(list.toVector).asInstanceOf[A])
                else Left("Cannot create NonEmptyVector from empty collection")
              },
            None
          )
      })
    }

    @scala.annotation.nowarn("msg=is never used")
    def mkNEC[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
      implicit val eType: Type[E] = elemType
      implicit val aType: Type[A] = AT
      implicit val listEType: Type[List[E]] = ListType[E]
      implicit val builderType: Type[scala.collection.mutable.Builder[E, List[E]]] = BuilderType[E, List[E]]

      Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
        override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
          Expr.quote(
            hearth.kindlings.catsintegration.internal.runtime.CatsConversions
              .nonEmptyChainToIterable[E](Expr.splice(value))
          )
        override type CtorResult = List[E]
        implicit override val CtorResult: Type[CtorResult] = listEType
        override def factory: Expr[scala.collection.Factory[E, CtorResult]] = Expr.quote {
          new scala.collection.Factory[E, List[E]] {
            override def newBuilder: scala.collection.mutable.Builder[E, List[E]] = List.newBuilder[E]
            override def fromSpecific(it: IterableOnce[E]): List[E] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[E, CtorResult], A] =
          CtorLikeOf.EitherStringOrValue(
            (builder: Expr[scala.collection.mutable.Builder[E, List[E]]]) =>
              Expr.quote {
                hearth.kindlings.catsintegration.internal.runtime.CatsConversions
                  .buildNonEmptyChain(Expr.splice(builder).result().asInstanceOf[List[Any]])
                  .asInstanceOf[Either[String, A]]
              },
            None
          )
      })
    }

    @scala.annotation.nowarn("msg=is never used")
    def mkChain[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
      implicit val eType: Type[E] = elemType
      implicit val aType: Type[A] = AT
      implicit val listEType: Type[List[E]] = ListType[E]
      implicit val builderType: Type[scala.collection.mutable.Builder[E, List[E]]] = BuilderType[E, List[E]]

      Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
        override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
          Expr.quote(Expr.splice(value).asInstanceOf[cats.data.Chain[E]].toList)
        override type CtorResult = List[E]
        implicit override val CtorResult: Type[CtorResult] = listEType
        override def factory: Expr[scala.collection.Factory[E, CtorResult]] = Expr.quote {
          new scala.collection.Factory[E, List[E]] {
            override def newBuilder: scala.collection.mutable.Builder[E, List[E]] = List.newBuilder[E]
            override def fromSpecific(it: IterableOnce[E]): List[E] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[E, CtorResult], A] =
          CtorLikeOf.PlainValue(
            (builder: Expr[scala.collection.mutable.Builder[E, List[E]]]) =>
              Expr.quote {
                cats.data.Chain.fromSeq(Expr.splice(builder).result()).asInstanceOf[A]
              },
            None
          )
      })
    }

    @scala.annotation.nowarn("msg=is never used|is unchecked")
    def mkNEM[A, K0, V0](
        AT: Type[A],
        keyType: Type[K0],
        valueType: Type[V0],
        orderK: Expr[cats.kernel.Order[K0]]
    ): IsCollection[A] = {
      implicit val aType: Type[A] = AT
      implicit val iPairType: Type[(K0, V0)] = Tuple2Ctor[K0, V0](using keyType, valueType)
      implicit val listPairType: Type[List[(K0, V0)]] = ListType[(K0, V0)]
      implicit val builderType: Type[scala.collection.mutable.Builder[(K0, V0), List[(K0, V0)]]] =
        BuilderType[(K0, V0), List[(K0, V0)]]

      Existential[IsCollectionOf[A, *], (K0, V0)](new IsMapOf[A, (K0, V0)] {
        override def asIterable(value: Expr[A]): Expr[Iterable[(K0, V0)]] =
          Expr.quote {
            hearth.kindlings.catsintegration.internal.runtime.CatsConversions
              .nonEmptyMapToIterable(Expr.splice(value))
              .asInstanceOf[Iterable[(K0, V0)]]
          }
        override type CtorResult = List[(K0, V0)]
        implicit override val CtorResult: Type[CtorResult] = listPairType
        override def factory: Expr[scala.collection.Factory[(K0, V0), CtorResult]] = Expr.quote {
          new scala.collection.Factory[(K0, V0), List[(K0, V0)]] {
            override def newBuilder: scala.collection.mutable.Builder[(K0, V0), List[(K0, V0)]] =
              List.newBuilder[(K0, V0)]
            override def fromSpecific(it: IterableOnce[(K0, V0)]): List[(K0, V0)] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[(K0, V0), CtorResult], A] =
          CtorLikeOf.EitherStringOrValue(
            (builder: Expr[scala.collection.mutable.Builder[(K0, V0), List[(K0, V0)]]]) =>
              Expr.quote {
                hearth.kindlings.catsintegration.internal.runtime.CatsConversions
                  .buildNonEmptyMap(
                    Expr.splice(builder).result().asInstanceOf[List[(Any, Any)]],
                    Expr.splice(orderK).asInstanceOf[cats.kernel.Order[Any]].toOrdering
                  )
                  .asInstanceOf[Either[String, A]]
              },
            None
          )
        override type Key = K0
        implicit override val Key: Type[Key] = keyType
        override type Value = V0
        implicit override val Value: Type[Value] = valueType
        override def key(pair: Expr[(K0, V0)]): Expr[Key] =
          Expr.quote(Expr.splice(pair)._1)
        override def value(pair: Expr[(K0, V0)]): Expr[Value] =
          Expr.quote(Expr.splice(pair)._2)
        override def pair(key: Expr[Key], value: Expr[Value]): Expr[(K0, V0)] =
          Expr.quote((Expr.splice(key), Expr.splice(value)))
      })
    }

    @scala.annotation.nowarn("msg=is never used")
    def mkNES[A, E0](
        AT: Type[A],
        elemType: Type[E0],
        orderElem: Expr[cats.kernel.Order[E0]]
    ): IsCollection[A] = {
      implicit val eType: Type[E0] = elemType
      implicit val aType: Type[A] = AT
      implicit val listElemType: Type[List[E0]] = ListType[E0]
      implicit val builderType: Type[scala.collection.mutable.Builder[E0, List[E0]]] =
        BuilderType[E0, List[E0]]

      Existential[IsCollectionOf[A, *], E0](new IsCollectionOf[A, E0] {
        override def asIterable(value: Expr[A]): Expr[Iterable[E0]] =
          Expr.quote(
            hearth.kindlings.catsintegration.internal.runtime.CatsConversions
              .nonEmptySetToIterable[E0](Expr.splice(value))
          )
        override type CtorResult = List[E0]
        implicit override val CtorResult: Type[CtorResult] = listElemType
        override def factory: Expr[scala.collection.Factory[E0, CtorResult]] = Expr.quote {
          new scala.collection.Factory[E0, List[E0]] {
            override def newBuilder: scala.collection.mutable.Builder[E0, List[E0]] = List.newBuilder[E0]
            override def fromSpecific(it: IterableOnce[E0]): List[E0] = List.from(it)
          }
        }
        override def build: CtorLikeOf[scala.collection.mutable.Builder[E0, CtorResult], A] =
          CtorLikeOf.EitherStringOrValue(
            (builder: Expr[scala.collection.mutable.Builder[E0, List[E0]]]) =>
              Expr.quote {
                hearth.kindlings.catsintegration.internal.runtime.CatsConversions
                  .buildNonEmptySet(
                    Expr.splice(builder).result().asInstanceOf[List[Any]],
                    Expr.splice(orderElem).asInstanceOf[cats.kernel.Order[Any]].toOrdering
                  )
                  .asInstanceOf[Either[String, A]]
              },
            None
          )
      })
    }

    // --- Provider registrations ---

    // --- NonEmptyList ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#NonEmptyList"

      private lazy val NELCtor = {
        val impl = Type.Ctor1.of[cats.data.NonEmptyList]
        Type.Ctor1.fromUntyped[cats.data.NonEmptyList](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        NELCtor.unapply(tpe) match {
          case Some(elem) => ProviderResult.Matched(mkNEL(tpe, elem.Underlying))
          case None       => skipped(s"${tpe.prettyPrint} is not a NonEmptyList")
        }
    })

    // --- NonEmptyVector ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#NonEmptyVector"

      private lazy val NEVCtor = {
        val impl = Type.Ctor1.of[cats.data.NonEmptyVector]
        Type.Ctor1.fromUntyped[cats.data.NonEmptyVector](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        NEVCtor.unapply(tpe) match {
          case Some(elem) => ProviderResult.Matched(mkNEV(tpe, elem.Underlying))
          case None       => skipped(s"${tpe.prettyPrint} is not a NonEmptyVector")
        }
    })

    // --- NonEmptyChain ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#NonEmptyChain"

      private lazy val NECCtor = {
        val impl = Type.Ctor1.of[cats.data.NonEmptyChain]
        Type.Ctor1.fromUntyped[cats.data.NonEmptyChain](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        NECCtor.unapply(tpe) match {
          case Some(elem) => ProviderResult.Matched(mkNEC(tpe, elem.Underlying))
          case None       => skipped(s"${tpe.prettyPrint} is not a NonEmptyChain")
        }
    })

    // --- Chain ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#Chain"

      private lazy val ChainCtor = {
        val impl = Type.Ctor1.of[cats.data.Chain]
        Type.Ctor1.fromUntyped[cats.data.Chain](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        ChainCtor.unapply(tpe) match {
          case Some(elem) => ProviderResult.Matched(mkChain(tpe, elem.Underlying))
          case None       => skipped(s"${tpe.prettyPrint} is not a Chain")
        }
    })

    // --- NonEmptyMap ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#NonEmptyMap"

      private lazy val NEMCtor = {
        val impl = Type.Ctor2.of[cats.data.NonEmptyMap]
        Type.Ctor2.fromUntyped[cats.data.NonEmptyMap](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used|is unchecked|Unused import")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        NEMCtor.unapply(tpe) match {
          case Some((keyType, valueType)) =>
            import keyType.Underlying as K
            import valueType.Underlying as V

            implicit val orderKType: Type[cats.kernel.Order[K]] = OrderCtor[K]
            val orderK = Expr.summonImplicit[cats.kernel.Order[K]].toOption match {
              case Some(o) => o
              case None    =>
                return skipped(
                  s"${tpe.prettyPrint} is a NonEmptyMap but Order[${keyType.Underlying.prettyPrint}] not found"
                )
            }

            ProviderResult.Matched(mkNEM(tpe, keyType.Underlying, valueType.Underlying, orderK))

          case None => skipped(s"${tpe.prettyPrint} is not a NonEmptyMap")
        }
    })

    // --- NonEmptySet ---
    IsCollection.registerProvider(new IsCollection.Provider {
      override def name: String = s"${loader.getClass.getName}#NonEmptySet"

      private lazy val NESCtor = {
        val impl = Type.Ctor1.of[cats.data.NonEmptySet]
        Type.Ctor1.fromUntyped[cats.data.NonEmptySet](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        NESCtor.unapply(tpe) match {
          case Some(elem) =>
            import elem.Underlying as Elem

            implicit val orderElemType: Type[cats.kernel.Order[Elem]] = OrderCtor[Elem]
            val orderElem = Expr.summonImplicit[cats.kernel.Order[Elem]].toOption match {
              case Some(o) => o
              case None    =>
                return skipped(
                  s"${tpe.prettyPrint} is a NonEmptySet but Order[${elem.Underlying.prettyPrint}] not found"
                )
            }

            ProviderResult.Matched(mkNES(tpe, elem.Underlying, orderElem))

          case None => skipped(s"${tpe.prettyPrint} is not a NonEmptySet")
        }
    })
  }
}
