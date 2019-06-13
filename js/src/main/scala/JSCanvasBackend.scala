package mandel

import org.scalajs.dom
import scala.util.Random
import spire.implicits._

object JSCanvasBackend {

  val WIDTH = 640
  val HEIGHT = 480
  var mandelbrot: Mandelbrot = new MandelbrotDouble(500)
  var viewpoint: Viewpoint = Viewpoint.default

  def setup(canvas: dom.html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    val imageData = ctx.getImageData(0, 0, WIDTH, HEIGHT)
    var down = false
    var viewpoint0 = viewpoint
    var x0 = 0
    var y0 = 0
    object image extends ImageWriter {
      def width: Int = WIDTH
      def height: Int = HEIGHT
      def setPixel(x: Int, y: Int, r: Int, g: Int, b: Int): Unit = {
        imageData.data((y*WIDTH+x)*4) = r
        imageData.data((y*WIDTH+x)*4+1) = g
        imageData.data((y*WIDTH+x)*4+2) = b
        imageData.data((y*WIDTH+x)*4+3) = 255
      }
    }
    dom.document.onkeydown = (k: dom.KeyboardEvent) => {
      def printme(str: String) = {
        draw()
        ctx.font = "12px Arial"
        ctx.fillText(str, 30, 40)
      }
      k.keyCode match {
      case '1' =>
        mandelbrot = new MandelbrotFloat(500)
        printme("Switch to Float")
      case '2' =>
        mandelbrot = new MandelbrotDouble(500)
        printme("Switch to Double")
      case '3' =>
        mandelbrot = new MandelbrotGeneric[Float](500)
        printme("Switch to generic Float")
      case '4' =>
        mandelbrot = new MandelbrotGeneric[Double](500)
        printme("Switch to generic Double")
      case '5' =>
        mandelbrot = new MandelbrotGeneric[DoubleDouble](500)
        printme("Switch to generic DoubleDouble")
      case _ =>
      }
    }
    canvas.ondblclick = (e: dom.MouseEvent) => {
      viewpoint = viewpoint.zoomOnPixel(e.clientX.toInt, e.clientY.toInt, WIDTH, HEIGHT)
      draw()
    }
    canvas.onmousedown = (e: dom.MouseEvent) => {
      x0 = e.clientX.toInt
      y0 = e.clientY.toInt
      viewpoint0 = viewpoint
      down = true
    }
    canvas.onmousemove = (e: dom.MouseEvent) => {
      if (down) {
        viewpoint = viewpoint0.translate(x0 - e.clientX.toInt, y0 - e.clientY.toInt, WIDTH, HEIGHT)
        draw()
      }
    }
    canvas.onmouseup = (e: dom.MouseEvent) => {
      down = false
    }
    canvas.oncontextmenu = (e: dom.MouseEvent) => {
      viewpoint = Viewpoint.default
      draw()
      false
    }
    def draw(): Unit = {
      val t0 = System.nanoTime
      mandelbrot.plot(image, viewpoint)
      ctx.putImageData(imageData, 0, 0)
      val t1 = System.nanoTime
      val fps = (10*1000*1000*1000.0/(t1.toDouble-t0.toDouble)).toInt / 10.0
      ctx.font = "12px Arial"
      ctx.fillText(s"$fps fps", 30, 20)
    }
    draw()
  }
  def main(args: Array[String]): Unit = {
    val canvas = dom.document.getElementById("canvas")
      .asInstanceOf[dom.html.Canvas]
    setup(canvas)
  }
}
