
import scala.collection.mutable.ArrayBuffer

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = {
      try {
        data(y * width + x) = c
      } catch {
        case e:Exception => {
          println(e)
        }
      }
    }
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (radius == 0) {
      src(x, y)
    } else {
      val pixels = for {
        x <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
        y <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
        rgba = src(x, y)
      } yield {
        (red(rgba), green(rgba), blue(rgba), alpha(rgba))
      }
      val pixelsCount = pixels.length
      val sum = pixels.foldLeft((0.0d, 0.0d, 0.0d, 0.0d)) {
        (r, xi) => (r._1 + xi._1, r._2 +xi._2, r._3 + xi._3, r._4 + xi._4)
      }

      rgba(
        red((sum._1 / pixelsCount).toInt),
        green((sum._2 / pixelsCount).toInt),
        blue((sum._3 / pixelsCount).toInt),
        alpha((sum._4 / pixelsCount).toInt)
      )
    }
  }

  def rangeBy(width:Int, numTasks: Int):Int = {
    if (width % numTasks == 0) {
      width / numTasks
    } else {
      1 + width / numTasks
    }
  }
}
