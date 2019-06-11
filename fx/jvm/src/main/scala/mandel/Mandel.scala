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
import scalafx.scene.input.MouseEvent

import spire.syntax.cfor._
import spire.math.Rational

object MandelbrotSet {

  val WIDTH = 640
  val HEIGHT = 480
  val MAX_ITERATIONS = 200
  val default: MandelbrotSet = MandelbrotSetDouble(-Rational(1, 2), Rational.zero, Rational(3, 2))

}

trait MandelbrotSet {
  def centerR: Rational
  def centerI: Rational
  def radius: Rational
  def plot(writer: PixelWriter): Unit
  def zoomOnPixel(x: Int, y: Int): MandelbrotSet
  def translate(x: Int, y: Int): MandelbrotSet
  def updated(newCenterR: Rational, newCenterI: Rational, newRadius: Rational): MandelbrotSet
}

case class MandelbrotSetDouble(
  centerR: Rational,
  centerI: Rational,
  radius: Rational
) extends MandelbrotSet {

  def updated(newCenterR: Rational, newCenterI: Rational, newRadius: Rational): MandelbrotSet = MandelbrotSetDouble(newCenterR, newCenterI, newRadius)

  import MandelbrotSet.{WIDTH, HEIGHT, MAX_ITERATIONS}

  private[this] val factor: Rational = 2*radius/(WIDTH - 1)
  private[this] val factorD: Double = factor.toDouble
  private[this] val minR: Double = (centerR - WIDTH*factor/2).toDouble
  private[this] val maxR: Double = (centerR + WIDTH*factor/2).toDouble
  private[this] val minI: Double = (centerI - HEIGHT*factor/2).toDouble
  private[this] val maxI: Double = (centerI + HEIGHT*factor/2).toDouble

  private[this] def real(x: Int): Double = minR + x*factorD
  private[this] def imag(y: Int): Double = maxI - y*factorD

  private[this] def iterate(cR: Double, cI: Double): Int = {
    var zR = cR
    var zI = cI
    var n = 0
    while (n < MAX_ITERATIONS) {
      val zR2 = zR * zR
      val zI2 = zI * zI
      if (zR2 + zI2 > 4.0) return n
      zI = 2 * zR * zI + cI
      zR = zR2 - zI2 + cR
      n += 1
    }
    -1
  }

  def plot(writer: PixelWriter): Unit = {
    cforRange(0 until HEIGHT) { y =>
      val cI = maxI - y * factorD
      cforRange(0 until WIDTH) { x =>
        val cR = minR + x * factorD
        val n = iterate(cR, cI)
        val color = if (n == -1) 0xFF000010 else (n << 12) + 0xEF0000FF
        writer.setArgb(x, y, color)
      }
    }
  }

  def zoomOnPixel(x: Int, y: Int): MandelbrotSet =
    updated(real(x), imag(y), radius/2)

  def translate(x: Int, y: Int): MandelbrotSet = updated(centerR - x * factor, centerI + y * factor, radius)

}

object Mandelbrot extends JFXApp {

  import MandelbrotSet.{WIDTH, HEIGHT}

  stage = new PrimaryStage {
    title = "Mandelbrot"
    resizable = false
    initStyle(StageStyle.UTILITY)
    scene = new Scene(WIDTH, HEIGHT) {
      fill = LinearGradient(
        startX = 0.0,
        startY = 0.0,
        endX = 0.0,
        endY = 1.0,
        proportional = true,
        cycleMethod = CycleMethod.NO_CYCLE,
        stops = List(Stop(0.0, Color.BLACK), Stop(1.0, Color.GRAY))
      )
      val fpsLabel = new Text {
        text = "0 fps"
        translateX = 10
        translateY = 20
        fill = Color.WHITE
      }
      var mandel = MandelbrotSet.default
      val canvas = new Canvas(WIDTH, HEIGHT)
      val gc = canvas.graphicsContext2D
      val group = new Group {
        var sx = 0
        var sy = 0
        var smandel = mandel
        focusTraversable = true
        onMousePressed = (e: MouseEvent) => {
          sx = e.x.toInt
          sy = e.y.toInt
          smandel = mandel
        }
        onMouseClicked = (e: MouseEvent) => {
          if (e.clickCount > 1) mandel = mandel.zoomOnPixel(e.x.toInt, e.y.toInt)
        }
        onMouseDragged = (e: MouseEvent) => {
            mandel = smandel.translate(e.x.toInt - sx, e.y.toInt - sy)
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
          fps = s"${Math.round(1/delta)} fps"
          fpsLabel.text = fps
          mandel.plot(writer)
        }
        lastTime = ns
      } start()
      content = group
    }
  }

}
