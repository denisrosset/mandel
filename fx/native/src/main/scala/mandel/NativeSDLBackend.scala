package mandel

import scalanative.unsafe._
import scalanative.annotation._
import scalanative.unsigned._
import SDL._
import SDLExtra._
import spire.implicits._

@extern
@link("SDL2")
object SDL {
  type Window   = CStruct0
  type Renderer = CStruct0

  def SDL_Init(flags: UInt): Unit = extern
  def SDL_CreateWindow(title: CString,
                       x: CInt,
                       y: CInt,
                       w: Int,
                       h: Int,
                       flags: UInt): Ptr[Window] = extern
  def SDL_Delay(ms: UInt): Unit                  = extern
  def SDL_CreateRenderer(win: Ptr[Window],
                         index: CInt,
                         flags: UInt): Ptr[Renderer] = extern

  type _56   = Nat.Digit2[Nat._5, Nat._6]
  type Event = CStruct2[UInt, CArray[Byte, _56]]

  def SDL_PollEvent(event: Ptr[Event]): CInt = extern

  type Rect = CStruct4[CInt, CInt, CInt, CInt]

  def SDL_RenderClear(renderer: Ptr[Renderer]): Unit = extern
  def SDL_SetRenderDrawColor(renderer: Ptr[Renderer],
                             r: UByte,
                             g: UByte,
                             b: UByte,
                             a: UByte): Unit = extern
  def SDL_RenderFillRect(renderer: Ptr[Renderer], rect: Ptr[Rect]): Unit =
    extern
  def SDL_RenderPresent(renderer: Ptr[Renderer]): Unit = extern
  def SDL_RenderDrawPoint(renderer: Ptr[Renderer], x: Int, y: Int): Unit = extern

  type KeyboardEvent =
    CStruct8[UInt, UInt, UInt, UByte, UByte, UByte, UByte, Keysym]
  type MouseButtonEvent =
    CStruct9[UInt, UInt, UInt, UInt, UByte, UByte, UByte, Int, Int]
  type MouseMotionEvent =
    CStruct9[UInt, UInt, UInt, UInt, UInt, Int, Int, Int, Int]
  type Keysym   = CStruct4[Scancode, Keycode, UShort, UInt]
  type Scancode = Int
  type Keycode  = Int

  def SDL_GetMouseState(x: Ptr[Int], y: Ptr[Int]): UInt = extern

}

object SDLExtra {
  val INIT_VIDEO   = 0x00000020.toUInt
  val WINDOW_SHOWN = 0x00000004.toUInt
  val VSYNC        = 0x00000004.toUInt

  implicit class EventOps(val self: Ptr[Event]) extends AnyVal {
    def type_ = self._1
  }

  val QUIT_EVENT = 0x100.toUInt

  val MOUSE_MOTION = 0x400.toUInt
  val MOUSE_BUTTON_DOWN = 0x401.toUInt
  val MOUSE_BUTTON_UP = 0x402.toUInt

  implicit class RectOps(val self: Ptr[Rect]) extends AnyVal {
    def init(x: Int, y: Int, w: Int, h: Int): Ptr[Rect] = {
      self._1 = x
      self._2 = y
      self._3 = w
      self._4 = h
      self
    }
  }

  val KEY_DOWN  = 0x300.toUInt
  val KEY_UP    = (0x300 + 1).toUInt
  val RIGHT_KEY = 1073741903
  val LEFT_KEY  = 1073741904
  val DOWN_KEY  = 1073741905
  val UP_KEY    = 1073741906
  val DIGIT0_KEY = '0'.toInt
  val DIGIT1_KEY = '1'.toInt
  val DIGIT2_KEY = '2'.toInt
  val DIGIT3_KEY = '3'.toInt
  val DIGIT4_KEY = '4'.toInt
  val DIGIT5_KEY = '5'.toInt

  val BUTTON_LEFT = 1.toUByte
  val BUTTON_MIDDLE = 2.toUByte
  val BUTTON_RIGHT = 3.toUByte

  implicit class KeyboardEventOps(val self: Ptr[KeyboardEvent])
      extends AnyVal {
    def keycode: Keycode = self._8._2
  }

  implicit class MouseButtonEventOps(val self: Ptr[MouseButtonEvent]) extends AnyVal {
    def button: UByte = self._5
    def clicks: UByte = self._7
    def x: Int = self._8
    def y: Int = self._9
  }

  implicit class MouseMotionEventOps(val self: Ptr[MouseMotionEvent]) extends AnyVal {
    def x: Int = self._6
    def y: Int = self._7
  }
}

object NativeSDLBackend extends App {
  val title  = c"Mandelbrot"
  val WIDTH  = 640
  val HEIGHT = 480

  var window: Ptr[Window]     = _
  var renderer: Ptr[Renderer] = _
  var mandelbrot: Mandelbrot = new MandelbrotDouble(500)
  var viewpoint: Viewpoint = Viewpoint.default
  var viewpoint0: Viewpoint = viewpoint
  var x0 = 0
  var y0 = 0
  var down = false

  object image extends ImageWriter {
    def width: Int = WIDTH
    def height: Int = HEIGHT
    def setPixel(x: Int, y: Int, r: Int, g: Int, b: Int): Unit = {
      SDL_SetRenderDrawColor(renderer, r.toUByte, g.toUByte, b.toUByte, 0.toUByte)
      SDL_RenderDrawPoint(renderer, x, y)
    }
  }

  def onDraw(): Unit = {
    val t0 = System.nanoTime
    mandelbrot.plot(image, viewpoint)
    val t1 = System.nanoTime
    val fps = (10*1000*1000*1000.0/(t1.toDouble-t0.toDouble)).toInt / 10.0
    println(s"$fps fps")
    SDL_RenderPresent(renderer)
  }

  def init(): Unit = {
    SDL_Init(INIT_VIDEO)
    window = SDL_CreateWindow(title, 0, 0, WIDTH, HEIGHT, WINDOW_SHOWN)
    renderer = SDL_CreateRenderer(window, -1, VSYNC)
  }

  def delay(ms: UInt): Unit =
    SDL_Delay(ms)

  def loop(): Unit = {
    val event = stackalloc[Event]
    while (true) {
      while (SDL_PollEvent(event) != 0) {
        event.type_ match {
          case MOUSE_BUTTON_DOWN =>
            val e = event.asInstanceOf[Ptr[MouseButtonEvent]]
            if (e.clicks.toInt == 2)
              viewpoint = viewpoint.zoomOnPixel(e.x, e.y, WIDTH, HEIGHT)
            viewpoint0 = viewpoint
            x0 = e.x
            y0 = e.y
            down = true
          case MOUSE_MOTION =>
            val e = event.asInstanceOf[Ptr[MouseMotionEvent]]
            if (down)
              viewpoint = viewpoint0.translate(x0 - e.x, y0 - e.y, WIDTH, HEIGHT)
          case MOUSE_BUTTON_UP =>
            val e = event.asInstanceOf[Ptr[MouseButtonEvent]]
            if (e.button == BUTTON_LEFT)
              viewpoint = viewpoint0.translate(x0 - e.x, y0 - e.y, WIDTH, HEIGHT)
            down = false
          case KEY_DOWN =>
            val e = event.asInstanceOf[Ptr[KeyboardEvent]]
            e.keycode match {
              case DIGIT1_KEY =>
                println("Switch to Float")
                mandelbrot = new MandelbrotFloat(500)
              case DIGIT2_KEY =>
                println("Switch to Double")
                mandelbrot = new MandelbrotDouble(500)
              case DIGIT3_KEY =>
                println("Switch to generic Float")
                mandelbrot = new MandelbrotGeneric[Float](500)
              case DIGIT4_KEY =>
                println("Switch to generic Double")
                mandelbrot = new MandelbrotGeneric[Double](500)
              case DIGIT5_KEY =>
                println("Switch to generic DoubleDouble")
                mandelbrot = new MandelbrotGeneric[DoubleDouble](500)
              case _ =>
            }
          case QUIT_EVENT =>
            return
          case _ =>
            ()
        }
      }
      onDraw()
    }
  }

  init()
  loop()
}
