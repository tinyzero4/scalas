package observatory

import java.nio.file.Paths

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.generateTiles

object Main extends App {

  override def main(args: Array[String]): Unit = {
    computeDeviations()
  }

  private def computeDeviations(): Unit = {
    val colors: List[(Double, Color)] = List(
      (7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255))
    )

    val temperatures = for {
      year <- 1975 to 1989
      temps = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
    } yield temps

    val normals = Manipulation.average(temperatures)

    println("Normals are calculated")

    val data: Iterable[(Year, GridLocation => Temperature)] = for {
      year <- 1990 to 1900
      yearTemps = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
      grid = Manipulation.deviation(yearTemps, normals)
    } yield (year, grid)

    println("Deviations are calculated")

    generateTiles(data, (year:Year, tile:Tile, grid: GridLocation => Temperature) => {
      val image = Visualization2.visualizeGrid (grid, colors, tile, 2)
      val path = Paths.get (s"./target/deviations/$year/${tile.zoom}")
      path.toFile.mkdirs ()
      image.output (Paths.get (s"$path/${tile.x}-${tile.y}.png") )
    })
  }

  private def computeTemperatures(): Unit = {
    val colors: List[(Double, Color)] = List(
      (-60, Color(0, 0, 0)),
      (-50, Color(33, 0, 107)),
      (-27, Color(255, 0, 255)),
      (-15, Color(0, 0, 255)),
      (0, Color(0, 255, 255)),
      (12, Color(255, 255, 0)),
      (32, Color(255, 0, 0)),
      (60, Color(255, 255, 255))
    )

    val data = for {
      year <- 1975 to 1975
      temps = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
    } yield (year, temps)

    generateTiles(data, saveTile(colors))
  }

  private def saveTile(colors: List[(Double, Color)])(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {
    val path = Paths.get(s"./target/temperatures/$year/${tile.zoom}")
    path.toFile.mkdirs()
    Interaction.tile(data, colors, tile, 2).output(Paths.get(s"$path/${tile.x}-${tile.y}.png"))
  }

}
