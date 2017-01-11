package com.github.agetakoyaki29.scala.geometry


object Delta {

  // object Default {
    implicit val delta: Double = MinNormal
  // }

  def relativeDelta(d: Double): Double = d.ulp.scalb(4)

  def eq0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = {
    d1.abs <= (delta +: deltas).max
  }
  def eq(d1: Double, d2: Double): Boolean = eq0(d1-d2, relativeDelta(d1), relativeDelta(d2))
  // def sumeq0(ds: Seq[Double]): Boolean = eq0(ds.sum, ds.map{relativeDelta(_)}: _*)

  def lt0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = d1 <= (delta +: deltas).max
  def lt(d1: Double, d2: Double): Boolean = lt0(d1-d2, relativeDelta(d1), relativeDelta(d2))

  def gt0(d1: Double, deltas: Double*)(implicit delta: Double): Boolean = d1 <= (delta +: deltas).max
  def gt(d1: Double, d2: Double): Boolean = gt0(d1-d2, relativeDelta(d1), relativeDelta(d2))

  // ---- Math ----

  val MaxExponent = java.lang.Double.MAX_EXPONENT
  val MinExponent = java.lang.Double.MIN_EXPONENT

  def sign(boolean: Boolean): Int = if(boolean) 1 else -1

  def minus(double: Double): Double = -double

  def isPositive(double: Double): Boolean = ??? // PositiveZero, NegativeZero
  def isNegative(double: Double): Boolean = ! isPositive(double)

  // def isPositiveInfinite(double: Double): Boolean

  def log2(double: Double): Double = log(double, 2)   // FIXME fast, rounded, check special double

  def log(d1: Double, d2: Double): Double = Math.log(d1) / Math.log(d2)   // FIXME fast, rounded, check special double

  // ---- Zero ----

  val Zero = 0d
  val PositiveZero = +0d
  val NegativeZero = -0d

  def isZero(double: Double): Boolean = double == 0d
  def isPositiveZero(double: Double): Boolean = double.isZero && double.isPositive
  def isNegativeZero(double: Double): Boolean = double.isZero && double.isNegative

  // ---- Normal ----

  lazy val MinNormal = java.lang.Double.MIN_NORMAL

  def isNormal(double: Double): Boolean = double match {
    case _ if double.isNaN => true
    case _ if double.isInfinite => true
    case _ if double.isZero => true
    case _ => double.abs >= MinNormal
  }

  // ---- radian degree ----

  // def chompRadian(double: Double): Double = ???
  // def chompDegree(double: Double): Double = ???

  // ---- for validation ----

  val NonNaN: PartialFunction[Double, Double] = {
    case double if double.isNaN => throw new IllegalArgumentException("required non NaN")
  }
  val NonInfinite: PartialFunction[Double, Double] = {
    case double if double.isInfinite => throw new IllegalArgumentException("required non Infinite")
  }
  val NonZero: PartialFunction[Double, Double] = {
    case double if double.isZero => throw new IllegalArgumentException("required non Zero")
  }
  val NonPlus: PartialFunction[Double, Double] = {
    case double if double > 0 => throw new IllegalArgumentException("required non Plus")
  }
  val NonMinus: PartialFunction[Double, Double] = {
    case double if double < 0 => throw new IllegalArgumentException("required non Minus")
  }
  val All: PartialFunction[Double, Double] = {
    case double => double
  }

  // ---- RichBoolean ----

  implicit class RichBoolean(val that: Boolean) {
    def sign: Int = Delta.sign(that)
  }

  // ---- RichDouble ----

  implicit class RichDouble(val that: Double) {
    def sqr: Double = that.scalb(1)

    // ---- copy from Delta ----
    def minus: Double = Delta.minus(that)
    def isPositive: Boolean = Delta.isPositive(that)
    def isNegative: Boolean = Delta.isNegative(that)
    def isPositiveZero: Boolean = Delta.isPositiveZero(that)
    def isNegativeZero: Boolean = Delta.isNegativeZero(that)
    def isZero: Boolean = Delta.isZero(that)
    def isNormal: Boolean = Delta.isNormal(that)
    def log2: Double = Delta.log2(that)
    def log(d2: Double): Double = Delta.log(that, d2)

    // ---- copy from java.lang.Double ----
    def isFinite: Boolean = java.lang.Double.isFinite(that)
    def toLongBits: Long = java.lang.Double.doubleToLongBits(that)
    def toRawLongBits: Long = java.lang.Double.doubleToRawLongBits(that)
    def toHexString: String = java.lang.Double.toHexString(that)

    // ---- copy from Math ----
    // def acos: Double = Math.acos(that)
    // def asin: Double = Math.asin(that)
    // def atan: Double = Math.atan(that)
    // def atan2(x: Double): Double = Math.atan2(that, x)
    // def cos: Double = Math.cos(that)
    // def cosh: Double = Math.cosh(that)
    // def exp: Double = Math.exp(that)
    // def expm1: Double = Math.expm1(that)
    // def hypot(y: Double): Double = Math.hypot(that, y)
    // def sin: Double = Math.sin(that)
    // def sinh: Double = Math.sinh(that)
    // def tan: Double = Math.tan(that)
    // def tanh: Double = Math.tanh(that)
    // ---- ulp ----
    def ulp: Double = Math.ulp(that)
    def nextUp: Double = Math.nextUp(that)
    def nextDown: Double = Math.nextDown(that)
    def nextAfter(direction: Double): Double = Math.nextAfter(that, direction)
    // ----
    def cbrt: Double = Math.cbrt(that)
    def sqrt: Double = Math.sqrt(that)
    // ----
    def copySign(sign: Double): Double = Math.copySign(that, sign)
    def getExponent: Double = Math.getExponent(that)
    def IEEEremainder(divisor: Double): Double = Math.IEEEremainder(that, divisor)
    def log: Double = Math.log(that)
    def log10: Double = Math.log10(that)
    def log1p: Double = Math.log1p(that)
    def pow(exponent: Double): Double = Math.pow(that, exponent)
    def rint: Double = Math.rint(that)
    def scalb(scaleFactor: Int): Double = Math.scalb(that, scaleFactor)
  }

  // for long bits
  implicit class LongBits(val that: Long) {
    def bitsToDouble: Double = java.lang.Double.longBitsToDouble(that)
  }

  // ---- for ____ ----

  implicit class WithDelta(that: Double) {
    def lt(d2: Double): WantDelta = WantDelta(Delta.lt(that, _))
    def gt(d2: Double): WantDelta = WantDelta(Delta.lt(that, _))
  }

  case class WantDelta(f: Double => Boolean) {
    def +-(delta: Double): Boolean = f(delta)
  }

}
