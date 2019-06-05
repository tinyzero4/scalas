package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Common.{ALPHA_LEVEL, SUB_TILE_ZOOM, TILE_SIZE}
import observatory.Visualization.{BROKEN_PIXEL, interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      180 * atan(sinh(PI - tile.y * 2 * PI / pow(2, tile.zoom))) / PI,
      (tile.x * 360 / pow(2, tile.zoom)) - 180
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile, scaleFactor:Int = 1): Image = {
    val tileSize = TILE_SIZE / scaleFactor
    val locations = for {
      y <- (tileSize * tile.y) until (tileSize * tile.y + tileSize)
      x <- (tileSize * tile.x) until (tileSize * tile.x + tileSize)
    } yield Tile(x, y, tile.zoom + SUB_TILE_ZOOM - (scaleFactor - 1))

    val pixels = Array.fill(tileSize * tileSize) {
      BROKEN_PIXEL
    }

    locations.indices.par.foreach(i => {
      val location = tileLocation(locations(i))
      val color = interpolateColor(colors, predictTemperature(temperatures, location))
      pixels(i) = Pixel(color.red, color.green, color.blue, ALPHA_LEVEL)
    })

    Image(tileSize, tileSize, pixels).scale(scaleFactor)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Year, Data)], generateImage: (Year, Tile, Data) => Unit): Unit = {
    yearlyData.par.foreach(i => {
      for {
        zoom <- 0 to 3
        x <- 0 until pow(2, zoom).toInt
        y <- 0 until pow(2, zoom).toInt
      } yield generateImage(i._1, Tile(x, y, zoom), i._2)
    })
  }

}
