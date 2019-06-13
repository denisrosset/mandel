package mandel

import spire.math.Rational

/** Describes a viewpoint in the Mandelbrot set, given by exact coordinates
  * of the center and the displayed half width (i.e. horizontal radius).
  * 
  * Handles conversion between display coordinates (in pixels, storage Int) 
  * and the Mandelbrot set coordinates (stored as exact Rational numbers).
  * 
  * By convention, coordinates increase when going down and to the right.
  * 
  * The top-left pixel is (0, 0) and the bottom-right pixel is (width-1, height-1).
  */
case class Viewpoint(centerX: Rational, centerY: Rational, halfWidth: Rational) {

  def coord(x: Int, y: Int, w: Int, h: Int): (Rational, Rational) = {
    val halfHeight = (halfWidth * h)/w
    val pixelSize = (halfWidth * 2)/w
    val cx = centerX - halfWidth + pixelSize * x
    val cy = centerY - halfHeight + pixelSize * y
    (cx, cy)
  }

  /** Zooms on the coordinates (x, y) in an image with dimensions (w, h). */
  def zoomOnPixel(x: Int, y: Int, w: Int, h: Int): Viewpoint = {
    val (cx, cy) = coord(x, y, w, h)
    Viewpoint(cx, cy, halfWidth / 10)
  }

  /** Translates the viewpoint by the shift equivalent to the one given
    * in (shiftX, shiftY) pixel units.
    */
  def translate(shiftX: Int, shiftY: Int, w: Int, h: Int): Viewpoint = {
    val factor = (halfWidth * 2)/w
    val cx = centerX + factor * shiftX
    val cy = centerY + factor * shiftY
    Viewpoint(cx, cy, halfWidth)
  }

}

object Viewpoint {

  val default = Viewpoint(-Rational(1, 2), Rational.zero, Rational(3, 2))

  val floatLimit = Viewpoint(Rational(-23736827, 41943040), Rational(-1676523, 2621440), Rational(3, 262144))

  val doubleLimit = Viewpoint(Rational(-57604895564971893L, 90071992547409920L), Rational(-57604895564971893L, 90071992547409920L), Rational(3, 281474976710656L))

/*  val bird = Viewpoint(Rational(0.3750001200618655), Rational(-0.2166393884377127), Rational(0.000000000002))

  val spiral = Viewpoint(Rational(-0.7746806106269039),
    Rational(-0.1374168856037867),
    Rational(2e-12)
  )*/
}
