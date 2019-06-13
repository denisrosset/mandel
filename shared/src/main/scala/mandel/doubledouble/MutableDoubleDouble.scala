package mandel

import scala.annotation.strictfp

@strictfp object MutableDoubleDouble {

  def one: MutableDoubleDouble = apply(1.0)

  def apply(fp: Double): MutableDoubleDouble =
    new MutableDoubleDoubleImpl(fp, 0.0, true)

  def apply(dd: DoubleDouble): MutableDoubleDouble =
    new MutableDoubleDoubleImpl(dd.hi, dd.lo, true)

  def apply(hi: Double, lo: Double): MutableDoubleDouble =
    new MutableDoubleDoubleImpl(hi, lo, true)

}

trait MutableDoubleDouble extends GenDoubleDouble {

  /** Returns an immutable version of this, which
    * cannot then be modified any further.
    */
  def result(): DoubleDouble

  /** Sets this to its floor. */
  def selfFloor(): MutableDoubleDouble

  /** Sets this to its ceil. */
  def selfCeil(): MutableDoubleDouble

  /** Sets this to its opposite. */
  def selfNegate(): MutableDoubleDouble

  /** Sets this to its square. */
  def selfSquare(): MutableDoubleDouble

  /** Sets the value of this to the given argument. */
  def setValueTo(dd: GenDoubleDouble): MutableDoubleDouble

  /** Sets the value of this to the given argument. */
  def setValueTo(d: Double): MutableDoubleDouble

  /** Sets this to its exp-th power. */
  def selfPow(exp: Int): MutableDoubleDouble

  /** Adds the argument to the value of <tt>this</tt>. */
  def +=(yhi: Double): MutableDoubleDouble

  /** Adds the argument to the value of <tt>this</tt>. */
  def +=(y: GenDoubleDouble): MutableDoubleDouble

  /** Subtracts the argument to the value of <tt>this</tt>. */
  def -=(yhi: Double): MutableDoubleDouble

  /** Subtracts the argument to the value of <tt>this</tt>. */
  def -=(y: GenDoubleDouble): MutableDoubleDouble

  /** Multiplies this by the argument, returning this. */
  def *=(y: Double): MutableDoubleDouble

  /** Multiplies this by the argument, returning this. */
  def *=(y: GenDoubleDouble): MutableDoubleDouble

  /** Divides this by the argument, returning this. */
  def /=(y: Double): MutableDoubleDouble

  /** Divides this by the argument, returning this. */
  def /=(y: GenDoubleDouble): MutableDoubleDouble

  /** Sets this to its reciprocal. */
  def selfReciprocal(): MutableDoubleDouble
}

/** Trickery: this implementation class extends both the mutable and immutable
  * types, so that the result() operation is zero-copy.
  */
@strictfp class MutableDoubleDoubleImpl(var hi: Double, var lo: Double,
                                                  var isMutable: Boolean) extends DoubleDouble with MutableDoubleDouble {

  def toDoubleDouble: DoubleDouble =
    if (isMutable) new MutableDoubleDoubleImpl(hi, lo, false) else this

  def result(): DoubleDouble = {
    isMutable = false
    this
  }

  def selfNegate(): MutableDoubleDouble = {
    require(isMutable)
    hi = -hi
    lo = -lo
    this
  }

  def selfPow(exp: Int): MutableDoubleDouble =
    if (exp == 0) { hi = 1.0; lo = 0.0; this }
    else if (exp == 1) this
    else if (exp == -1) selfReciprocal()
    else {
      val r = mutableCopy
      this.setValueTo(1.0)
      var n = exp.abs
      // n > 1 by construction
      /* Use binary exponentiation */
      while (n > 0) {
        if (n % 2 == 1) this *= r
        n /= 2
        if (n > 0) r.selfSquare()
      }
      /* Compute the reciprocal if n is negative. */
      if (exp < 0) this.selfReciprocal()
      this
    }

  def selfSquare(): MutableDoubleDouble = {
    this *= this
  }

  def setValueTo(d: Double): MutableDoubleDouble = {
  hi = d
  lo = 0.0
  this
}

  def setValueTo(dd: GenDoubleDouble): MutableDoubleDouble = {
    hi = dd.hi
    lo = dd.lo
    this
  }

  @strictfp def +=(yhi: Double): MutableDoubleDouble = {
    require(isMutable)
    val S = hi + yhi
    var e = S - hi
    var s = S - e
    s = yhi - e + (hi - s)
    e = s + lo
    val H = S + e
    e = e + (S - H)
    val zhi = H + e
    val zlo = e + (H - zhi)
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def +=(y: GenDoubleDouble): MutableDoubleDouble = {
    require(isMutable)
    val S = hi + y.hi
    val T = lo + y.lo
    var e = S - hi
    val f = T - lo
    var s = S - e
    var t = T - f
    s = y.hi - e + (hi - s)
    t = y.lo - f + (lo - t)
    e = s + T
    val H = S + e
    val h = e + (S - H)
    e = t + h
    val zhi = H + e
    val zlo = e + (H - zhi)
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def -=(yhi: Double): MutableDoubleDouble = {
    require(isMutable)
    val S = hi - yhi
    var e = S - hi
    var s = S - e
    s = -yhi - e + (hi - s)
    e = s + lo
    val H = S + e
    e = e + (S - H)
    val zhi = H + e
    val zlo = e + (H - zhi)
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def -=(y: GenDoubleDouble): MutableDoubleDouble = {
    require(isMutable)
    val S = hi - y.hi
    val T = lo - y.lo
    var e = S - hi
    val f = T - lo
    var s = S - e
    var t = T - f
    s = -y.hi - e + (hi - s)
    t = -y.lo - f + (lo - t)
    e = s + T
    val H = S + e
    val h = e + (S - H)
    e = t + h
    val zhi = H + e
    val zlo = e + (H - zhi)
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def *=(yhi: Double): MutableDoubleDouble = {
    var hx = 0.0
    var tx = 0.0
    var hy = 0.0
    var ty = 0.0
    var C = 0.0
    var c = 0.0
    C = DoubleDouble.split * hi
    hx = C - hi
    c = DoubleDouble.split * yhi
    hx = C - hx
    tx = hi - hx
    hy = c - yhi
    C = hi * yhi
    hy = c - hy
    ty = yhi - hy
    c = hx * hy - C + hx * ty + tx * hy + tx * ty + lo * yhi
    val zhi = C + c
    hx = C - zhi
    val zlo = c + hx
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def *=(y: GenDoubleDouble): MutableDoubleDouble = {
    var hx = 0.0
    var tx = 0.0
    var hy = 0.0
    var ty = 0.0
    var C = 0.0
    var c = 0.0
    C = DoubleDouble.split * hi
    hx = C - hi
    c = DoubleDouble.split * y.hi
    hx = C - hx
    tx = hi - hx
    hy = c - y.hi
    C = hi * y.hi
    hy = c - hy
    ty = y.hi - hy
    c = hx * hy - C + hx * ty + tx * hy + tx * ty + (hi * y.lo + lo * y.hi)
    val zhi = C + c
    hx = C - zhi
    val zlo = c + hx
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def /=(yhi: Double): MutableDoubleDouble = {
    var hc = 0.0
    var tc = 0.0
    var hy = 0.0
    var ty = 0.0
    var C = 0.0
    var c = 0.0
    var U = 0.0
    var u = 0.0
    C = hi / yhi
    c = DoubleDouble.split * C
    hc = c - C
    u = DoubleDouble.split * yhi
    hc = c - hc
    tc = C - hc
    hy = u - yhi
    U = C * yhi
    hy = u - hy
    ty = yhi - hy
    u = hc * hy - U + hc * ty + tc * hy + tc * ty
    c = (hi - U - u + lo) / yhi
    u = C + c
    val zhi = u
    val zlo = C - u + c
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def /=(y: GenDoubleDouble): MutableDoubleDouble = {
    var hc = 0.0
    var tc = 0.0
    var hy = 0.0
    var ty = 0.0
    var C = 0.0
    var c = 0.0
    var U = 0.0
    var u = 0.0
    C = hi / y.hi
    c = DoubleDouble.split * C
    hc = c - C
    u = DoubleDouble.split * y.hi
    hc = c - hc
    tc = C - hc
    hy = u - y.hi
    U = C * y.hi
    hy = u - hy
    ty = y.hi - hy
    u = hc * hy - U + hc * ty + tc * hy + tc * ty
    c = (hi - U - u + lo - C * y.lo) / y.hi
    u = C + c
    val zhi = u
    val zlo = C - u + c
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def selfReciprocal(): MutableDoubleDouble = {
    var hc = 0.0
    var tc = 0.0
    var hy = 0.0
    var ty = 0.0
    var C = 0.0
    var c = 0.0
    var U = 0.0
    var u = 0.0
    C = 1.0 / hi
    c = DoubleDouble.split * C
    hc = c - C
    u = DoubleDouble.split * hi
    hc = c - hc
    tc = C - hc
    hy = u - hi
    U = C * hi
    hy = u - hy
    ty = hi - hy
    u = hc * hy - U + hc * ty + tc * hy + tc * ty
    c = (1.0 - U - u - C * lo) / hi
    val zhi = C + c
    val zlo = C - zhi + c
    hi = zhi
    lo = zlo
    this
  }

  @strictfp def selfFloor(): MutableDoubleDouble =
    if (isNaN) this else {
      val fhi = java.lang.Math.floor(hi)
      var flo = 0.0
      // Hi is already integral. Floor the low word
      if (fhi == hi) flo = java.lang.Math.floor(lo)
      // TODO: normalize here?
      hi = fhi
      lo = flo
      this
    }

  @strictfp def selfCeil(): MutableDoubleDouble =
    if (isNaN) this else {
      val fhi = java.lang.Math.ceil(hi)
      var flo = 0.0
      // Hi is already integral. Ceil the low word
      if (fhi == hi) flo = java.lang.Math.ceil(lo)
      // TODO: do we need to renormalize here?
      hi = fhi
      lo = flo
      this
    }
}
