package hearth.kindlings.oracle.circe

// Scala 3 fixtures of the circe oracle (see circeFixtures.scala for why they live in the main sources).

/** Parameterless cases only: an enumeration (`ConfiguredEnumCodec`). */
enum CiPlanet {
  case Mercury, Venus, EarthLike
}

/** An enum with parameterised and parameterless cases (`ConfiguredCodec`). */
enum CiVehicle {
  case Car(wheelCount: Int)
  case Bike
  case Truck(loadTons: Double, axleCount: Int)
}

final case class CiGarage(vehicles: List[CiVehicle], favourite: Option[CiPlanet])
