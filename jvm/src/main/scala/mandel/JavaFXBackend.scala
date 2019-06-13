package mandel

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.{Group, Scene}
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.PixelWriter
import scalafx.scene.input.KeyEvent
import scalafx.scene.paint.{Stop, CycleMethod, LinearGradient, Color}
import scalafx.scene.text.Text
import scalafx.stage.StageStyle
import scalafx.animation.AnimationTimer
import scala.util.control.Breaks._
import scalafx.scene.input.{MouseButton, MouseEvent, KeyEvent, KeyCode}

import spire.algebra._
import spire.math._
import spire.implicits._

object JavaFXBackend extends JFXApp {

  val WIDTH = 640
  val HEIGHT = 480
  var mandelbrot: Mandelbrot = new MandelbrotGeneric[Double](500)

  stage = new PrimaryStage {
    title = "Mandelbrot"
    resizable = false
    initStyle(StageStyle.UTILITY)
    scene = new Scene(WIDTH, HEIGHT) {
      val fpsLabel = new Text {
        text = "0 fps"
        translateX = 10
        translateY = 20
        fill = Color.WHITE
      }
      var viewpoint = Viewpoint.default
      val canvas = new Canvas(WIDTH, HEIGHT)
      val gc = canvas.graphicsContext2D
      val group = new Group {
        var x0 = 0
        var y0 = 0
        var viewpoint0 = viewpoint
        focusTraversable = true
        onKeyPressed = (k: KeyEvent) => k.code match {
          case KeyCode.Digit1 =>
            println("Switch to Float")
            mandelbrot = new MandelbrotFloat(500)
          case KeyCode.Digit2 =>
            println("Switch to Double")
            mandelbrot = new MandelbrotDouble(500)
          case KeyCode.Digit3 =>
            println("Switch to generic Float")
            mandelbrot = new MandelbrotGeneric[Float](500)
          case KeyCode.Digit4 =>
            println("Switch to generic Double")
            mandelbrot = new MandelbrotGeneric[Double](500)
          case KeyCode.Digit5 =>
            println("Switch to generic DoubleDouble")
            mandelbrot = new MandelbrotGeneric[DoubleDouble](500)
          case _ =>
        }
        onMousePressed = (e: MouseEvent) => {
          x0 = e.x.toInt
          y0 = e.y.toInt
          viewpoint0 = viewpoint
        }
        onMouseClicked = (e: MouseEvent) => {
          if (e.clickCount > 1 && e.button == MouseButton.Primary) viewpoint = viewpoint.zoomOnPixel(e.x.toInt, e.y.toInt, WIDTH, HEIGHT)
          if (e.button == MouseButton.Secondary) viewpoint = Viewpoint.default
        }
        onMouseDragged = (e: MouseEvent) => {
          viewpoint = viewpoint0.translate(x0 - e.x.toInt, y0 - e.y.toInt, WIDTH, HEIGHT)
        }
        children = List(canvas, fpsLabel)
      }

      val writer = gc.getPixelWriter
      var lastTime = 0L
      var delta = 0D
      var fps = "0 fps"
      AnimationTimer { ns =>
        if (lastTime > 0) {
          delta = (ns - lastTime) / 1e9
          fps = s"${Math.round(10/delta)/10.0} fps"
          fpsLabel.text = fps
          object image extends ImageWriter {
            def width: Int = WIDTH
            def height: Int = HEIGHT
            def setPixel(x: Int, y: Int, r: Int, g: Int, b: Int): Unit = {
              writer.setArgb(x, y, (255 << 24) + (r << 16) + (g << 8) + b)
            }
          }
          mandelbrot.plot(image, viewpoint)
        }
        lastTime = ns
      } start()
      content = group
    }
  }

}
