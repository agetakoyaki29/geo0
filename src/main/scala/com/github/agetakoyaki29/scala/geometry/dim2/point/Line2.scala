package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.dim2.{Dim2Factory, Dim2, Vector2}


object Dir2 extends Dim2Factory[Dir2] {
  def apply(x: Double, y: Double): Dir2 = new Dir2(x, y)

  def AtAngle(deg: Double): Dir2 = this(Math.cos(deg), Math.sin(deg))
}


@SameRet
class Dir2(_x: Double, _y: Double) extends Point2(_x, _y) {

  override val factory: Dim2Factory[_ <: Dir2] = Dir2

  def toDir2: Dir2 = this

  override protected def validate = Dim2.NonZero orElse Dim2.NonInfinite orElse super.validate

  // ----

  @UpRet def reflect: Dir2 = minus

  // ---- about angle ----

  def align(idx: Int) = this parallel Dir2.E(idx)

  @UpRet def normalDir: Dir2 = factory(-y, x)

  def normal(op: Dir2): Boolean = this dotEq0 op

  def parallel(op: Dir2): Boolean = this crossEq0 op

  /**
   * -pi ~ pi
   */
  def angle: Double = Math.atan2(y, x)

  def angleTo(op: Dir2): Double = op.angle - this.angle

  def cosTo(op: Dir2): Double = this dot op / this.norm / op.norm

  def sinTo(op: Dir2): Double = this cross op / this.norm / op.norm

  // ---- figure to point ----

  // def inRegion1(pt: Point): Boolean = pt match {
  //   case _ if pt.isInfinite => true
  //   case _ => this dotGt0 pt
  // }
  // def inRegion2(pt: Point): Boolean = (this reflect) inRegion1 (this to pt)

  def through(pt: Point2): Boolean = pt match {
    case _ if pt.isInfinite => true
    case _ => this crossEq0 pt
  }

  /**
   * 0 <= this angle pt <= pi
   * (this sinTo pt) < 0
   */
  def containPoint2(pt: Point2): Boolean = pt match {
    case _ if pt.isInfinite => true
    case _ => this crossLt0 pt
  }

  /**
   * this sinTo pt * pt.norm
   * this distance isInfinite => PositiveInfinite
   */
  override def distance(pt: Point2): Double = pt match {
    case _ if pt.isInfinite => Double.PositiveInfinity
    case _ => (this cross pt / this.norm).abs
  }
  override def distanceSqr(pt: Point2): Double = pt match {
    case _ if pt.isInfinite => Double.PositiveInfinity
    case _ => (this cross pt).sqr / this.normSqr
  }

  /**
   * this.normalized * this cosTo pt * pt.norm
   * pt + this.normal.normalized * -distance
   * this * (this dot pt) / (this dot this)
   * this distance isInfinite => PositiveInfinite
   */
  def nearest(pt: Point2): Point2 = pt match {
    case _ if pt.isInfinite => Point2.Infinity
    case _ => Point2(this) * (this dot pt / this.normSqr)
  }

  // ---- figure to other figure ----

  def same(op: Dir2): Boolean = this parallel op

  // def same(line: Line): Boolean = (this passThrough line.sp) && (this parallel line.dir)

  // def aabb: AABB = AABB.WHOLE

  // def intersect(line: Line): Set[Point2] = ???
  // def isIntersect(line: Line): Boolean = ???

  // ---- UpRet ----

  override def reverseD2 = factory(super.reverseD2)
  override def updatedD2(idx: Int, elem: Double) = factory(super.updatedD2(idx, elem))
  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+())
  override def unary_-() = factory(super.unary_-())
  override def +(op: Dim2) = factory(super.+(op))
  override def -(op: Dim2) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))
  override def minus = factory(super.minus)
  override def normalized = factory(super.normalized)
}


object Line2 {
}


class Line2 {
}
