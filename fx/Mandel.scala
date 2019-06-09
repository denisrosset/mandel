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

object Mandelbrot extends JFXApp {

  val w = 640
  val h = 480
  val minR = -2.0
  val maxR = 1.0
  val minI = -1.1
  val maxI = minI + (maxR - minR) * h / w
  val factorR = (maxR - minR) / (w - 1)
  val factorI = (maxI - minI) / (h - 1)
  val iterations = 60
  var phase = 12.0

  stage = new PrimaryStage {
    title = "Mandelbrot"
    resizable = false
    initStyle(StageStyle.UTILITY)
    scene = new Scene(w, h) {
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
      val canvas = new Canvas(w, h)
      val gc = canvas.graphicsContext2D
      val group = new Group {
        focusTraversable = true
        onKeyPressed = (ke: KeyEvent) => println(ke.code)
        onKeyReleased = (ke: KeyEvent) => println(ke.code)
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
          (0 until h).foreach { y =>
            val cI = maxI - y * factorI
            (0 until w).foreach { x =>
              val cR = minR + x * factorR
              plot(writer, x, y, cR -> cI)
            }
          }
        }
        lastTime = ns
      } start()
      content = group
    }
  }

  def plot(writer: PixelWriter, x: Int, y: Int, c: (Double, Double)) = {
    val (cR, cI) = c
    var ZR = cR
    var ZI = cI
    var isInside = true
    var n = 0
    breakable { while (n < iterations) {
        val ZIsq = ZI * ZI
        val ZRsq = ZR * ZR
        if (ZRsq + ZIsq > 4) { isInside = false; break() }
        ZI = 2 * ZR * ZI + cI
        ZR = ZRsq - ZIsq + cR
        n += 1
    }}
    val color = if (isInside) 0xFF000010 else (n << phase.toByte) + 0xEF0000FF
    writer.setArgb(x, y, color)
  }
}
