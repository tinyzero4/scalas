package observatory

import observatory.Common._
import observatory.Visualization.predictTemperature


/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val grid = generateGridLocations.aggregate(Map[GridLocation, Temperature]())(
      (agg, loc) => agg + (loc -> predictTemperature(temperatures, Location(loc.lat, loc.lon))),
      (agg1, agg2) => agg1 ++ agg2
    )

    grid(_)
  }

  private def generateGridLocations: Iterable[GridLocation] =
    for {
      y <- GRID_LAT_MIN to GRID_LAT_MAX
      x <- GRID_LON_MIN to GRID_LON_MAX
      location = GridLocation(y, x)
    } yield location

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess.par.map(makeGrid)

    val averages = generateGridLocations.par.aggregate(Map[GridLocation, Temperature]())(
      (agg, loc) => agg + (loc -> grids.map(_ (loc)).sum / grids.size),
      (agg1, agg2) => agg1 ++ agg2
    )

    averages(_)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)
    val deviations = generateGridLocations.par.aggregate(Map[GridLocation, Temperature]())(
      (agg, loc) => agg + (loc -> (grid(loc) - normals(loc))),
      (agg1, agg2) => agg1 ++ agg2
    )

    deviations(_)
  }


}

