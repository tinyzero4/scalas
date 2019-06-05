package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Common._
import observatory.Interaction.tileLocation
import observatory.Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(point: CellPoint, d00: Temperature, d01: Temperature, d10: Temperature, d11: Temperature): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d10 * point.x * (1 - point.y) + d01 * (1 - point.x) * point.y + d11 * point.x * point.y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(grid: GridLocation => Temperature, colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    visualizeGrid(grid, colors, tile, 1)
  }

  def visualizeGrid(grid: GridLocation => Temperature, colors: Iterable[(Temperature, Color)], tile: Tile, scaleFactor: Int): Image = {
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
      val color = interpolateColor(grid, colors, location)
      pixels(i) = Pixel(color.red, color.green, color.blue, ALPHA_LEVEL)
    })

    Image(tileSize, tileSize, pixels).scale(scaleFactor)
  }

  private def interpolateColor(grid: GridLocation => Temperature, colors: Iterable[(Double, Color)], location: Location): Color = {
    val latCeil = resolveGridLatValue(math.ceil(location.lat)).toInt
    val latFloor = resolveGridLatValue(math.floor(location.lat)).toInt
    val lonCeil = resolveGridLonValue(math.ceil(location.lon)).toInt
    val lonFloor = resolveGridLonValue(math.floor(location.lon)).toInt

    val d00 = grid(GridLocation(latCeil, lonFloor))
    val d10 = grid(GridLocation(latCeil, lonCeil))
    val d01 = grid(GridLocation(latFloor, lonFloor))
    val d11 = grid(GridLocation(latFloor, lonCeil))

    val point = CellPoint(location.lon - lonFloor, location.lat - latFloor)
    Visualization.interpolateColor(colors, bilinearInterpolation(point, d00, d01, d10, d11))
  }

  private def resolveGridLonValue(lon: Double): Double = {
    if (lon < GRID_LON_MIN) GRID_LON_MIN
    else if (lon > GRID_LON_MAX) GRID_LON_MAX
    else lon
  }

  private def resolveGridLatValue(lat: Double): Double = {
    if (lat < GRID_LAT_MIN) GRID_LAT_MIN
    else if (lat > GRID_LAT_MAX) GRID_LAT_MAX
    else lat
  }

}
