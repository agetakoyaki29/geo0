package com.github.agetakoyaki29.scala.geometry.dim2

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.{Dim, DimFactory}


object Dim2 extends Dim2Factory[Dim2] {
  def apply(x: Double, y: Double): Dim2 = new Dim2(x, y)

  // ---- for validation ----

  val NonNaN: PartialFunction[Dim2, Dim2] = {
    case dim2 if dim2.isNaN => throw new IllegalArgumentException("required non NaN")
  }
  val NonInfinite: PartialFunction[Dim2, Dim2] = {
    case dim2 if dim2.isInfinite => throw new IllegalArgumentException("required non Infinite")
  }
  val NonZero: PartialFunction[Dim2, Dim2] = {
    case dim2 if dim2.isZero => throw new IllegalArgumentException("required non Zero")
  }
  // val NonPlus: PartialFunction[Dim2, Dim2] = {
  //   case dim2 if dim2 > 0 => throw new IllegalArgumentException("required non Plus")
  // }
  // val NonMinus: PartialFunction[Dim2, Dim2] = {
  //   case dim2 if dim2 < 0 => throw new IllegalArgumentException("required non Minus")
  // }
  val All: PartialFunction[Dim2, Dim2] = {
    case dim2 => dim2
  }
}


class Dim2(_x: Double, val _y: Double) extends IndexedSeq[Double] with Dim {
  val (x, y) = (_x, _y)

  val factory: Dim2Factory[_ <: Dim2] = Dim2

  def toDim2: Dim2 = this

  // -- validate --

  validate apply this
  this foreach {validateEach apply _}

  protected def validate: PartialFunction[Dim2, Dim2] = Dim2.All
  protected def validateEach: PartialFunction[Double, Double] = All orElse NonNaN orElse NonInfinite

  // ---- for IndexedSeq ----

  override final def foreach[U](f: Double => U): Unit = ???
  final def apply(idx: Int): Double = ???
  final def length: Int = 2

  // ---- from IndexedSeq ----

  final def zipmap[B](op: Dim2)(f: (Double, Double) => B): IndexedSeq[B] = this zip op map { (tup: (Double, Double)) => f(tup._1, tup._2) }

  @UpRet def reverseD2: Dim2 = factory(super.reverse)

  @UpRet def updatedD2(idx: Int, elem: Double): Dim2 = factory(super.updated(idx, elem))

  @UpRet def mapD2(f: Double => Double): Dim2 = factory(super.map(f))

  @UpRet def zipmapD2(op: Dim2)(f: (Double, Double) => Double): Dim2 = factory(this.zipmap(op)(f))

  // ---- basic operators ----

  @UpRet def abs: Dim2 = mapD2{_.abs}

  @UpRet def unary_+(): Dim2 = factory(this)
  @UpRet def unary_-(): Dim2 = mapD2{-_}

  @UpRet def +(op: Dim2): Dim2 = zipmapD2(op) {_+_}

  @UpRet def -(op: Dim2): Dim2 = zipmapD2(op) {_-_}

  /**
   * @param d NonInfinite(zero * inf = NaN)
   */
  @UpRet def *(d: Double): Dim2 = d match {
    case _ if d.isInfinite => throw new IllegalArgumentException("required non infinite")
    case _ => mapD2{_*d}
  }

  /**
   * @param d NonZero(zero / zero = NaN, any / zero = inf)
   */
  @UpRet def /(d: Double): Dim2 = d match {
    case _ if d.isZero => throw new IllegalArgumentException("required non zero")
    case _ => mapD2{_/d}
  }

  @UpRet def minus: Dim2 = -this

  /**
   * return 1 norm, same direction Dim2
   * isZero => throw IllegalStateException
   */
  @UpRet def normalized: Dim2 = {
    if(this.isZero) throw new IllegalStateException("Zero can't normalize")
    this / norm
  }

  // ---- std ----

  override def equals(op: Any) = op match {
    case dim2: Dim2 => this.zipmap(dim2) {_==_} reduce {_||_}
    case _ => false
  }
}


abstract class Dim2Factory[+T <: Dim2 : ClassTag] extends DimFactory[T] {
  final val Length: Int = 2

  def apply(x: Double, y: Double): T
  def apply(op: Dim2): T = clone(op)
  def apply(seq: Seq[Double]): T = {
    if(seq.length != Length) throw new IllegalArgumentException(s"wrong length seq; found: ${seq.length}, required: ${Length}")
    this(seq(0), seq(1))
  }

  def clone(dim2: Dim2): T = ???
}
