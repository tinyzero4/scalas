package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}
import Common._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private[observatory] val MIN_DISTANCE = 1.0
  private[observatory] val IMAGE_WIDTH_PX = 360
  private[observatory] val IMAGE_HEIGHT_PX = 180
  private[observatory] val UNDEFINED = Integer.MIN_VALUE
  private[observatory] val BROKEN_PIXEL = Pixel(0, 0, 0, ALPHA_LEVEL)


  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    type Acc = (Double, Double, Double, Temperature)

    def seq(acc: Acc, loc: (Location, Temperature)): Acc = {
      if (acc._1 <= MIN_DISTANCE) acc
      else {

        val dist = greatestCircleDistance(location, loc._1)
        if (dist <= MIN_DISTANCE) (dist, ZERO, ZERO, loc._2)
        else {
          val w = 1.0 / pow(dist, POWER)
          (dist, acc._2 + w * loc._2, acc._3 + w, ZERO)
        }
      }
    }

    def combine(v1: Acc, v2: Acc): Acc = {
      if (v1._1 <= MIN_DISTANCE) v1
      else if (v2._1 <= MIN_DISTANCE) v2
      else (min(v1._1, v2._1), v1._2 + v2._2, v1._3 + v2._3, 0.0)
    }

    val result = temperatures.aggregate((Double.MaxValue, ZERO, ZERO, ZERO))(seq, combine)

    if (result._1 <= MIN_DISTANCE) result._4
    else result._2 / result._3
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    if (points.isEmpty) throw new IllegalArgumentException("no source points")

    type Point = (Temperature, Color)

    def linearInterpolate(x0: Double, x1: Double, y0: Int, y1: Int, x: Double): Int =
      rint(((y0 * (x1 - x)) + (y1 * (x - x0))) / (x1 - x0)).toInt

    val res = points.foldLeft((None: Option[Point], None: Option[Point], None: Option[Point]))((acc, p) => {
      if (acc._3.isDefined) acc
      else if (p._1 == value) (None, None, Some(p))
      else if (p._1 < value && (acc._1.isEmpty || acc._1.exists(_._1 < p._1))) (Some(p), acc._2, acc._3)
      else if (p._1 > value && (acc._2.isEmpty || acc._2.exists(_._1 > p._1))) (acc._1, Some(p), acc._3)
      else acc
    })

    if (res._3.isDefined) res._3.head._2
    else if (res._2.isEmpty) res._1.head._2
    else if (res._1.isEmpty) res._2.head._2
    else {
      val p0 = res._1.head
      val p1 = res._2.head
      Color(
        linearInterpolate(p0._1, p1._1, p0._2.red, p1._2.red, value),
        linearInterpolate(p0._1, p1._1, p0._2.green, p1._2.green, value),
        linearInterpolate(p0._1, p1._1, p0._2.blue, p1._2.blue, value)
      )
    }
  }


  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], scaleFactor:Int = 1): Image = {
    val width = IMAGE_WIDTH_PX / scaleFactor
    val height = IMAGE_HEIGHT_PX / scaleFactor

    val pixels = Array.fill(width * height){BROKEN_PIXEL}

    pixels.indices.par.foreach(i => {
      val y = i % width
      val x = i / width
      val location = Location(LAT_MAX - y, x - LON_MAX)
      val color = interpolateColor(colors, predictTemperature(temperatures, location))
      pixels(i) = Pixel(color.red, color.green, color.blue, ALPHA_LEVEL)
    })

    Image(width, height, pixels).scale(scaleFactor)
  }

  private[observatory] def greatestCircleDistance(loc1: Location, loc2: Location): Double = {
    def _sin(d: Double): Double = sin(toRadians(d))
    def _cos(d: Double): Double = cos(toRadians(d))

    if (loc1 == loc2) 0
    else {
      val deltaLonInRad = toRadians(abs(loc1.lon - loc2.lon))
      acos(_sin(loc1.lat) * _sin(loc2.lat) + _cos(loc1.lat) * _cos(loc2.lat) * cos(deltaLonInRad)) * RADIUS
    }
  }

}

