package mandel

import spire.algebra.{Eq, Field, Order}

import scala.annotation.{strictfp, tailrec}
import spire.syntax.cfor._

/** Immutable, extended-precision floating-point numbers which maintain 106 bits
  * (approximately 30 decimal digits) of precision.
  * <p>
  *
  *   Original code by Martin Davis, see http://www.tsusiatsoftware.net/
  *   adapated to Scala programming conventions by Denis Rosset
  *
  * <p>
  * A DoubleDouble uses a representation containing two double-precision values.
  * A number x is represented as a pair of doubles, x.hi and x.lo, such that the
  * number represented by x is x.hi + x.lo, where
  *
  * <pre>
  * |x.lo| <= 0.5*ulp(x.hi)
  * </pre>
  *
  * and ulp(y) means "unit in the last place of y". The basic arithmetic
  * operations are implemented using convenient properties of IEEE-754
  * floating-point arithmetic.
  * <p>
  * The range of values which can be represented is the same as in IEEE-754. The
  * precision of the representable numbers is twice as great as IEEE-754 double
  * precision.
  * <p>
  * The correctness of the arithmetic algorithms relies on operations being
  * performed with standard IEEE-754 double precision and rounding. This is the
  * Java standard arithmetic model, but for performance reasons Java
  * implementations are not constrained to using this standard by default. Some
  * processors (notably the Intel Pentium architecure) perform floating point
  * operations in (non-IEEE-754-standard) extended-precision. A JVM
  * implementation may choose to use the non-standard extended-precision as its
  * default arithmetic mode. To prevent this from happening, this code uses the
  * Java <tt>strictfp</tt> modifier, which forces all operations to take place in
  * the standard IEEE-754 rounding model.
  * <p>
  * The API provides a value-oriented interface. DoubleDouble values are
  * immutable; operations on them return new objects carrying the result of the
  * operation. This provides a much simpler semantics for writing DoubleDouble
  * expressions, and Java memory management is efficient enough that this imposes
  * very little performance penalty.
  * <p>
  * This implementation uses algorithms originally designed variously by Knuth,
  * Kahan, Dekker, and Linnainmaa. Douglas Priest developed the first C
  * implementation of these techniques. Other more recent C++ implementation are
  * due to Keith M. Briggs and David Bailey et al.
  *
  * <h3>References</h3>
  * <ul>
  * <li>Priest, D., <i>Algorithms for Arbitrary Precision Floating Point
  * Arithmetic</i>, in P. Kornerup and D. Matula, Eds., Proc. 10th Symposium on
  * Computer Arithmetic, IEEE Computer Society Press, Los Alamitos, Calif., 1991.
  * <li>Yozo Hida, Xiaoye S. Li and David H. Bailey, <i>Quad-Double Arithmetic:
  * Algorithms, Implementation, and Application</i>, manuscript, Oct 2000;
  * Lawrence Berkeley National Laboratory Report BNL-46996.
  * <li>David Bailey, <i>High Precision Software Directory</i>;
  * <tt>http://crd.lbl.gov/~dhbailey/mpdist/index.html</tt>
  * </ul>
  *
  * @author Martin Davis
  *
  * Source: https://github.com/ChiralBehaviors/Utils/tree/master/src/main/java/com/hellblazer/utils/math
  * under Apache 2.0 License
  *
  */
@strictfp object DoubleDouble {

  private[this] val instance: DoubleDoubleAlgebra = new DoubleDoubleAlgebra
  implicit def field: Field[DoubleDouble] = instance
  implicit def order: Order[DoubleDouble] = instance

  /** The value nearest to the constant Pi. */
  val pi = DoubleDouble(3.141592653589793116e+00, 1.224646799147353207e-16)
  /** The value nearest to the constant 2 * Pi. */
  val twopi = DoubleDouble(6.283185307179586232e+00, 2.449293598294706414e-16)
  /** The value nearest to the constant Pi / 2. */
  val halfpi = DoubleDouble(1.570796326794896558e+00, 6.123233995736766036e-17)
  /** The value nearest to the constant e (the natural logarithm base). */
  val e = DoubleDouble(2.718281828459045091e+00, 1.445646891729250158e-16)
  /** A value representing the result of an operation which does not return a
    * valid number. */
  val NaN = DoubleDouble(Double.NaN, Double.NaN)
  /** The smallest representable relative difference between two {link @DoubleDouble} values */
  val eps = 1.23259516440783e-32 /* = 2^-106 */
  /** The value to split a double-precision value on during multiplication */
  val split = 134217729.0D // 2^27+1, for IEEE double

  val maxPrintDigits = 32
  val ten = DoubleDouble(10.0)
  val one = DoubleDouble(1.0)
  val half = DoubleDouble(0.5)
  val zero = DoubleDouble(0.0)
  val sciNotExponentChar = "E"
  val sciNotZero = "0.0E0"

  /**
    * Converts a string representation of a real number into a DoubleDouble
    * value. The format accepted is similar to the standard Java real number
    * syntax. It is defined by the following regular expression:
    *
    * <pre>
    * [<tt>+</tt>|<tt>-</tt>] {<i>digit</i>} [ <tt>.</tt> {<i>digit</i>} ] [ ( <tt>e</tt> | <tt>E</tt> ) [<tt>+</tt>|<tt>-</tt>
    * ] {<i>digit</i>}+
    *
    * <pre>
    *
    * @param str the string to parse
    * @return the value of the parsed number
    * @throws NumberFormatException if <tt>str</tt>
    *                               is not a valid representation of a number
    */
  def parse(str: String): DoubleDouble = {
    var i = 0
    val strlen = str.length
    // skip leading whitespace
    while (Character.isWhitespace(str.charAt(i))) i += 1
    // check for sign
    var isNegative = false
    if (i < strlen) {
      val signCh = str.charAt(i)
      if (signCh == '-' || signCh == '+') {
        i += 1
        if (signCh == '-') isNegative = true
      }
    }
    // scan all digits and accumulate into an integral value
    // Keep track of the location of the decimal point (if any) to allow
    // scaling later
    var value: DoubleDouble = DoubleDouble.zero
    var numDigits = 0
    var numBeforeDec = 0
    var exp = 0
    @tailrec def rec(): Unit =
      if (i >= strlen) value
      else {
        val ch = str.charAt(i)
        i += 1
        if (Character.isDigit(ch)) {
          val d = ch - '0'
          value = value.*(ten).+(DoubleDouble(d))
          numDigits += 1
          rec()
        } else if (ch == '.') {
          numBeforeDec = numDigits
          rec()
        } else if (ch == 'e' || ch == 'E') {
          val expStr = str.substring(i)
          // this should catch any format problems with the exponent
          try
            exp = expStr.toInt
          catch {
            case ex: NumberFormatException =>
              throw new NumberFormatException("Invalid exponent " + expStr + " in string " + str)
          }
        }
        else throw new NumberFormatException("Unexpected character '" + ch + "' at position " + i + " in string " + str)
    }
    var value2 = value
    // scale the number correctly
    val numDecPlaces = numDigits - numBeforeDec - exp
    if (numDecPlaces == 0) value2 = value
    else if (numDecPlaces > 0) {
      val scale = ten.pow(numDecPlaces)
      value2 = value./(scale)
    }
    else if (numDecPlaces < 0) {
      val scale = ten.pow(-numDecPlaces)
      value2 = value.*(scale)
    }
    // apply leading sign, if any
    if (isNegative) -value2 else value2
  }

  /**
    * Converts the string argument to a DoubleDouble number.
    *
    * @param str
    * a string containing a representation of a numeric value
    * @return the extended precision version of the value
    * @throws NumberFormatException
    * if <tt>s</tt> is not a valid representation of a number
    */
  def apply(str: String): DoubleDouble = parse(str)

  /**
    * Determines the decimal magnitude of a number. The magnitude is the
    * exponent of the greatest power of 10 which is less than or equal to the
    * number.
    *
    * @param x the number to find the magnitude of
    * @return the decimal magnitude of x
    */
  def computeMagnitude(x: Double): Int = {
    val xAbs = x.abs
    val xLog10 = java.lang.Math.log10(xAbs)
    var xMag = xLog10.floor.toInt
    // Since log computation is inexact, there may be an off-by-one error in
    // the computed magnitude. Following tests that magnitude is correct,
    // and adjusts it if not
    val xApprox = java.lang.Math.pow(10, xMag)
    if (xApprox * 10 <= xAbs) xMag += 1
    xMag
  }

  /**
    * Creates a string of a given length containing the given character
    *
    * @param ch the character to be repeated
    * @param len the len of the desired string
    * @return the string
    */
  def stringOfChar(ch: Char, len: Int) = {
    val buf = new StringBuffer
    cforRange(0 until len) { i =>
      buf.append(ch)
    }
    buf.toString
  }

  /** Creates a new DoubleDouble with value x. */
  def apply(x: Double): DoubleDouble = MutableDoubleDouble(x).result()

  /** Creates a new DoubleDouble with value (hi, lo). */
  def apply(hi: Double, lo: Double): DoubleDouble = MutableDoubleDouble(hi, lo).result()

}

abstract class GenDoubleDouble {

  def toDoubleDouble: DoubleDouble

  def hi: Double

  def lo: Double

  /** Returns an integer indicating the sign of this value.
    * <ul>
    * <li>if this value is > 0, returns 1
    * <li>if this value is < 0, returns -1
    * <li>if this value is = 0, returns 0
    * <li>if this value is NaN, returns 0
    * </ul>
    *
    * @return an integer indicating the sign of this value
    */
  def signum: Int =
    if (isPositive) 1
    else if (isNegative) -1
    else 0

  /** Dumps the components of this number to a string.
    *
    * @return a string showing the components of the number
    */
  def dump: String = s"DD($hi, $lo)"


  /** Tests whether this value is equal to another <tt>DoubleDouble</tt> value.
    *
    * @param y a DoubleDouble value
    * @return true if this value = y
    */
  override def equals(that: Any): Boolean = that match {
    case y: GenDoubleDouble => this.defaultEquals(y)
    case _ => false // TODO: compatibility with primitive types ?
  }

  def defaultEquals(y: GenDoubleDouble): Boolean = hi == y.hi && lo == y.lo

  /** Compares two DoubleDouble objects numerically.
    *
    * @return -1,0 or 1 depending on whether this value is less than, equal to
    *         or greater than the value of <tt>o</tt>
    */
  def compareTo(other: GenDoubleDouble): Int =
    if (hi < other.hi) -1
    else if (hi > other.hi) 1
    else if (lo < other.lo) -1
    else if (lo > other.lo) 1
    else 0

  /**
    * Converts this value to the nearest double-precision number.
    *
    * @return the nearest double-precision number to this value
    */
  def toDouble: Double = hi + lo

  /**
    * Tests whether this value is greater than or equals to another
    * <tt>DoubleDouble</tt> value.
    *
    * @param y a DoubleDouble value
    * @return true if this value >= y
    */
  def >=(y: GenDoubleDouble): Boolean = hi > y.hi || hi == y.hi && lo >= y.lo

  /**
    * Tests whether this value is greater than another <tt>DoubleDouble</tt>
    * value.
    *
    * @param y a DoubleDouble value
    * @return true if this value > y
    */
  def >(y: GenDoubleDouble): Boolean = hi > y.hi || hi == y.hi && lo > y.lo

  /**
    * Converts this value to the nearest integer.
    *
    * @return the nearest integer to this value
    */
  def toInt: Int = hi.toInt

  /**
    * Tests whether this value is NaN.
    *
    * @return true if this value is NaN
    */
  def isNaN: Boolean = hi.isNaN

  /**
    * Tests whether this value is less than 0.
    *
    * @return true if this value is less than 0
    */
  def isNegative: Boolean = hi < 0.0 || hi == 0.0 && lo < 0.0

  /**
    * Tests whether this value is greater than 0.
    *
    * @return true if this value is greater than 0
    */
  def isPositive: Boolean = hi > 0.0 || hi == 0.0 && lo > 0.0

  /**
    * Tests whether this value is equal to 0.
    *
    * @return true if this value is equal to 0
    */
  def isZero: Boolean = hi == 0.0 && lo == 0.0

  /**
    * Tests whether this value is less than or equal to another
    * <tt>DoubleDouble</tt> value.
    *
    * @param y a DoubleDouble value
    * @return true if this value <= y
    */
  def <=(y: GenDoubleDouble): Boolean = hi < y.hi || hi == y.hi && lo <= y.lo

  /**
    * Tests whether this value is less than another <tt>DoubleDouble</tt>
    * value.
    *
    * @param y a DoubleDouble value
    * @return true if this value < y
    */
  def <(y: GenDoubleDouble): Boolean = hi < y.hi || hi == y.hi && lo < y.lo

  /** Returns the absolute value of this value. Special cases:
    * <ul>
    * <li>If this value is NaN, it is returned.
    * </ul>
    *
    * @return the absolute value of this value
    */
  def abs: DoubleDouble =
    if (isNaN) DoubleDouble.NaN
    else if (isNegative) (-this)
    else toDoubleDouble

  /** Returns a DoubleDouble whose value is <tt>(this + y)</tt>.
    *
    * @param y the addend
    * @return <tt>(this + y)</tt>
    */
  def +(y: GenDoubleDouble): DoubleDouble =
    if (isNaN || y.isNaN) DoubleDouble.NaN else (mutableCopy += y).result()

  /** Returns the smallest (closest to negative infinity) value that is not
    * less than the argument and is equal to a mathematical integer. Special
    * cases:
    * <ul>
    * <li>If this value is NaN, returns NaN.
    * </ul>
    *
    * @return the smallest (closest to negative infinity) value that is not
    *         less than the argument and is equal to a mathematical integer.
    */
  def ceil: DoubleDouble = if (isNaN) DoubleDouble.NaN else mutableCopy.selfCeil().result()

  /** Returns a DoubleDouble whose value is <tt>(this / y)</tt>.
    *
    * @param y the divisor
    * @return <tt>(this / y)</tt>
    */
  def /(y: GenDoubleDouble): DoubleDouble =
    if (isNaN || y.isNaN) DoubleDouble.NaN else (mutableCopy /= y).result()

  /** Returns the largest (closest to positive infinity) value that is not
    * greater than the argument and is equal to a mathematical integer. Special
    * cases:
    * <ul>
    * <li>If this value is NaN, returns NaN.
    * </ul>
    *
    * @return the largest (closest to positive infinity) value that is not
    *         greater than the argument and is equal to a mathematical integer.
    */
  def floor: DoubleDouble =
    if (isNaN) DoubleDouble.NaN else mutableCopy.selfFloor().result()

  /** Returns a mutable copy of <tt>this</tt>. */
  def mutableCopy: MutableDoubleDouble = MutableDoubleDouble(hi, lo)

  /** Returns a DoubleDouble whose value is <tt>(this * y)</tt>. */
  def *(y: GenDoubleDouble): DoubleDouble =
    if (isNaN || y.isNaN) DoubleDouble.NaN else (mutableCopy *= y).result()

  /** Returns a DoubleDouble whose value is <tt>-this</tt>. */
  def unary_- : DoubleDouble = if (isNaN) DoubleDouble.NaN else DoubleDouble(-hi, -lo)

  /** Computes the value of this number raised to an integral power. Follows
    * semantics of Java Math.pow as closely as possible.
    *
    * @param exp the integer exponent
    * @return x raised to the integral power exp
    */
  def pow(exp: Int): DoubleDouble = if (isNaN) DoubleDouble.NaN else mutableCopy.selfPow(exp).result()

  /** Returns a DoubleDouble whose value is <tt>1 / this</tt>.
    *
    * @return the reciprocal of this value
    */
  def reciprocal: DoubleDouble = mutableCopy.selfReciprocal().result()

  /** Rounds this value to the nearest integer. The value is rounded to an
    * integer by adding 1/2 and taking the floor of the result. Special cases:
    * <ul>
    * <li>If this value is NaN, returns NaN.
    * </ul>
    *
    * @return this value rounded to the nearest integer
    */
  def rint: DoubleDouble =
    if (isNaN) DoubleDouble.NaN
    // may not be 100% correct
    else (mutableCopy += DoubleDouble.half).selfFloor().result()

  /**
    * Computes the square of this value.
    *
    * @return the square of this value.
    */
  def square: DoubleDouble = this * this

  /** Returns a DoubleDouble whose value is <tt>(this - y)</tt>.
    *
    * @param y
    * the subtrahend
    * @return <tt>(this - y)</tt>
    */
  def -(y: DoubleDouble): DoubleDouble =
    if (isNaN || y.isNaN) DoubleDouble.NaN else (mutableCopy -= y).result()

  /** Returns the string representation of this value in scientific notation.
    *
    * @return the string representation in scientific notation
    */
  def toSciNotation: String =
    if (isZero) DoubleDouble.sciNotZero // special case zero, to allow as
    else getSpecialNumberString.getOrElse {
      val magnitude = new Array[Int](1)
      val digits = extractSignificantDigits(false, magnitude)
      val expStr = DoubleDouble.sciNotExponentChar + magnitude(0)
      // should never have leading zeroes
      // MD - is this correct? Or should we simply strip them if they are
      // present?
      if (digits.charAt(0) == '0') throw new IllegalStateException("Found leading zero: " + digits)
      // add decimal point
      var trailingDigits = ""
      if (digits.length > 1) trailingDigits = digits.substring(1)
      val digitsWithDecimal = digits.charAt(0) + "." + trailingDigits
      if (isNegative)
        "-" + digitsWithDecimal + expStr
      else
        digitsWithDecimal + expStr
    }

  /**
    * Returns the string representation of this value in standard notation.
    *
    * @return the string representation in standard notation
    */
  def toStandardNotation: String =
    getSpecialNumberString.getOrElse {
      val magnitude = new Array[Int](1)
      val sigDigits = extractSignificantDigits(true, magnitude)
      val decimalPointPos = magnitude(0) + 1
      var num = sigDigits
      // add a leading 0 if the decimal point is the first char
      if (sigDigits.charAt(0) == '.') num = "0" + sigDigits
      else if (decimalPointPos < 0) num = "0." + DoubleDouble.stringOfChar('0', -decimalPointPos) + sigDigits
      else if (sigDigits.indexOf('.') == -1) { // no point inserted - sig digits must be smaller than magnitude of
        // number
        // add zeroes to end to make number the correct size
        val numZeroes = decimalPointPos - sigDigits.length
        val zeroes = DoubleDouble.stringOfChar('0', numZeroes)
        num = sigDigits + zeroes + ".0"
      }
      if (isNegative) "-" + num else num
    }

  /**
    * Returns a string representation of this number, in either standard or
    * scientific notation. If the magnitude of the number is in the range [
    * 10<sup>-3</sup>, 10<sup>8</sup> ] standard notation will be used.
    * Otherwise, scientific notation will be used.
    *
    * @return a string representation of this number
    */
  override def toString: String = {
    val mag = DoubleDouble.computeMagnitude(hi)
    if (mag >= -3 && mag <= 20) toStandardNotation else toSciNotation
  }

  /**
    * Returns the integer which is largest in absolute value and not further
    * from zero than this value. Special cases:
    * <ul>
    * <li>If this value is NaN, returns NaN.
    * </ul>
    *
    * @return the integer which is largest in absolute value and not further
    *         from zero than this value
    */
  def trunc: DoubleDouble =
    if (isNaN) DoubleDouble.NaN
    else if (isPositive) floor
    else ceil

  /**
    * Extracts the significant digits in the decimal representation of the
    * argument. A decimal point may be optionally inserted in the string of
    * digits (as long as its position lies within the extracted digits - if
    * not, the caller must prepend or append the appropriate zeroes and decimal
    * point).
    *
    * @param y
    * the number to extract ( >= 0)
    * @param decimalPointPos
    * the position in which to insert a decimal point
    * @return the string containing the significant digits and possibly a
    *         decimal point
    */
  @strictfp def extractSignificantDigits(insertDecimalPoint: Boolean, magnitude: Array[Int]) = {
    import DoubleDouble.{one, ten, computeMagnitude}
    val y = mutableCopy
    if (y.isNegative) y.selfNegate()
    // compute *correct* magnitude of y
    var mag = computeMagnitude(y.hi)
    val scale = ten.pow(mag)
    y /= scale
    // fix magnitude if off by one
    if (y > ten) {
      y /= ten
      mag += 1
    }
    else if (y < one) {
      y *= ten
      mag -= 1
    }
    val decimalPointPos = mag + 1
    val buf = new StringBuffer
    val numDigits = DoubleDouble.maxPrintDigits - 1
    var i = 0
    var cont = true
    while (cont && i <= numDigits) {
      if (insertDecimalPoint && i == decimalPointPos) buf.append('.')
      val digit = y.hi.toInt

      /**
        * If a negative remainder is encountered, simply terminate the
        * extraction. This is robust, but maybe slightly inaccurate. My
        * current hypothesis is that negative remainders only occur for
        * very small lo components, so the inaccuracy is tolerable
        */
      if (digit < 0) cont = false else {
        var rebiasBy10 = false
        var digitChar: Char = 0
        if (digit > 9) { // set flag to re-bias after next 10-shift
          rebiasBy10 = true
          // output digit will end up being '9'
          digitChar = '9'
        }
        else digitChar = ('0' + digit).toChar
        buf.append(digitChar)
        y -= digit.toDouble
        y *= ten
        if (rebiasBy10) y += ten

        /**
          * Check if remaining digits will be 0, and if so don't output them.
          * Do this by comparing the magnitude of the remainder with the
          * expected precision.
          */
        val remMag = computeMagnitude(y.hi)
        if (remMag < 0 && remMag.abs >= numDigits - i) cont = false
        i += 1
      }
    }
    magnitude(0) = mag
    buf.toString
  }

  /**
    * Returns the string for this value if it has a known representation. (E.g.
    * NaN or 0.0)
    *
    * @return the string for this special number
    * @return null if the number is not a special number
    */
  private def getSpecialNumberString: Option[String] =
    if (isZero) Some("0.0")
    else if (isNaN) Some("NaN")
    else None
}

/** Immutable double-double precision number */
abstract class DoubleDouble extends GenDoubleDouble

final class DoubleDoubleAlgebra extends Field[DoubleDouble] with Order[DoubleDouble] {
  def plus(x: DoubleDouble, y: DoubleDouble): DoubleDouble = x + y
  def div(x: DoubleDouble, y: DoubleDouble): DoubleDouble = x / y
  def one: DoubleDouble = DoubleDouble.one
  def times(x: DoubleDouble, y: DoubleDouble): DoubleDouble = x * y
  def negate(x: DoubleDouble): DoubleDouble = -x
  def zero: DoubleDouble = DoubleDouble.zero
  def compare(x: DoubleDouble, y: DoubleDouble): Int = x.compareTo(y)

  override def reciprocal(x: DoubleDouble): DoubleDouble = x.reciprocal

  override def fromInt(n: Int): DoubleDouble = DoubleDouble(n)

  override def minus(x: DoubleDouble, y: DoubleDouble): DoubleDouble = x.-(y)

  override def isZero(a: DoubleDouble)(implicit ev: Eq[DoubleDouble]): Boolean = a.isZero

  override def eqv(x: DoubleDouble, y: DoubleDouble): Boolean = x.equals(y)

  override def neqv(x: DoubleDouble, y: DoubleDouble): Boolean = !x.equals(y)

  override def lteqv(x: DoubleDouble, y: DoubleDouble): Boolean = x <= y

  override def lt(x: DoubleDouble, y: DoubleDouble): Boolean = x < y

  override def gteqv(x: DoubleDouble, y: DoubleDouble): Boolean = x.>=(y)

  override def gt(x: DoubleDouble, y: DoubleDouble): Boolean = x.>(y)
}
