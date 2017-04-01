
import common._

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
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    // here is the functional way to do it
    //    val rgbas = for {
    //      col <- clamp(x - radius, 0, src.width - 1) until clamp(x + radius + 1, 0, src.width - 1)
    //      row <- clamp(y - radius, 0, src.height - 1) until clamp(y + radius + 1, 0, src.height - 1)
    //    } yield src.apply(col, row)
    //
    //    val channels = rgbas.map(r => (red(r), green(r), blue(r), alpha(r)))
    //
    //    val total = channels.fold((0, 0, 0, 0))((x, y) => (x._1 + y._1, x._2 + y._2, x._3 + y._3, x._4 + y._4))
    //    val r = total._1 / rgbas.size
    //    val g = total._2 / rgbas.size
    //    val b = total._3 / rgbas.size
    //    val a = total._4 / rgbas.size
    //
    //    rgba(r, g, b, a)

    var firstCol = clamp(x - radius, 0, src.width - 1)
    var row = clamp(y - radius, 0, src.height - 1)
    var size = 0
    var r, g, b, a = 0

    while (row <= clamp(y + radius, 0, src.height - 1)) {
      var col = firstCol
      while (col <= clamp(x + radius, 0, src.width - 1)) {
        val rgba = src.apply(col, row)
        r = r + red(rgba)
        g = g + green(rgba)
        b = b + blue(rgba)
        a = a + alpha(rgba)
        size = size + 1
        col = col + 1
      }

      row = row + 1
    }

    if (size > 0) {
      r = r / size
      g = g / size
      b = b / size
      a = a / size
    }

    rgba(r, g, b, a)
  }

}
