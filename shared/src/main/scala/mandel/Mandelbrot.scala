package mandel

import spire.math.Rational

trait Mandelbrot {

  /** Maximum iterations before considering the point is inside the set. */
  def maxIterations: Int

  /** Computes and draws the Mandelbrot set through the given
    * viewpoint using the given image writer. */
  def plot(image: ImageWriter, viewpoint: Viewpoint): Unit

}
