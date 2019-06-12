package mandel

import spire.algebra.{Field, Order}
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.field._
import spire.syntax.order._

/** Implementation of the Mandelbrot set using Float. 
  * 
  * @param skip How many pixels to skip horizontally or vertically.
  */
class MandelbrotGeneric[@specialized(Float, Double) A:Field:Order]
  (val maxIterations: Int) extends Mandelbrot {

  val four = Field[A].fromInt(4)

  def twice(a: A): A = a + a

  def fromRational(r: Rational): A = Field[A].fromBigInt(r.numerator.toBigInt) / Field[A].fromBigInt(r.denominator.toBigInt)

  def iterate(cR: A, cI: A): Int = {
    var zR = cR
    var zI = cI
    var n = 0
    while (n < maxIterations) {
      val zR2 = zR * zR
      val zI2 = zI * zI
      if (zR2 + zI2 > four) return n
      zI = twice(zR * zI) + cI
      zR = zR2 - zI2 + cR
      n += 1
    }
    -1
  }

  def plot(image: ImageWriter, viewpoint: Viewpoint): Unit = {
    import viewpoint.{centerX, centerY, halfWidth}
    val halfHeight: Rational = (halfWidth * image.height) / image.width
    val pixelSize = fromRational((halfWidth * 2)/image.width)
    val left: A = fromRational(centerX - halfWidth)
    val top: A = fromRational(centerY - halfHeight)
    cforRange(0 until image.width) { x =>
      val xd = left + pixelSize * Field[A].fromInt(x)
      cforRange(0 until image.height) { y =>
        val yd = top + pixelSize * Field[A].fromInt(y)
        iterate(xd, yd) match {
          case -1 => image.setPixel(x, y, 0, 0, 0)
          case n => image.setPixel(x, y, 0, (n*16) % 256, 255)
        }
      }
    }
  }

}
