package observatory

import java.nio.file.Paths

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.generateTiles

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val year = 2015

    val temps = locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))
    generateTiles(Seq((year, temps)), saveTile)
  }

  private def saveTile(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {
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

    Interaction.tile(data, colors, tile).output(Paths.get(s"./target/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png"))
  }

}
