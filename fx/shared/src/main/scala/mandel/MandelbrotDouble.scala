package mandel

import spire.math.Rational
import spire.syntax.cfor._

/** Implementation of the Mandelbrot set using Double. */
class MandelbrotDouble(val maxIterations: Int) extends Mandelbrot {

  def iterate(cR: Double, cI: Double): Int = {
    var zR = cR
    var zI = cI
    var n = 0
    while (n < maxIterations) {
      val zR2 = zR * zR
      val zI2 = zI * zI
      if (zR2 + zI2 > 4.0) return n
      zI = 2 * zR * zI + cI
      zR = zR2 - zI2 + cR
      n += 1
    }
    -1
  }

  def plot(image: ImageWriter, viewpoint: Viewpoint): Unit = {
    import viewpoint.{centerX, centerY, halfWidth}
    val halfHeight: Rational = (halfWidth * image.height) / image.width
    val pixelSize = (halfWidth.toDouble * 2)/image.width
    val left = (centerX - halfWidth).toDouble
    val top = (centerY - halfHeight).toDouble

    cforRange(0 until image.width) { x =>
      val xd = left + x * pixelSize
      cforRange(0 until image.height) { y =>
        val yd = top + y * pixelSize
        iterate(xd, yd) match {
          case -1 => image.setPixel(x, y, 0, 0, 0)
          case n => image.setPixel(x, y, 0, (n*16) % 256, 255)
        }
      }
    }
  }

}
