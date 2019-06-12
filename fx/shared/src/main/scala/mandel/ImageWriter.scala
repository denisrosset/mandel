package mandel

/** Abstraction describing an image buffer, used by the different
  * backends: JVM (Scala FX), JS (DOM) and Native (SDL).
  */
trait ImageWriter {
  def height: Int
  def width: Int
  /** Sets the pixel at position (x, y) to the RGB color (r, g, b)
    * where r,g,b are in the range 0-255.
    */
  def setPixel(x: Int, y: Int, r: Int, g: Int, b: Int): Unit
}
