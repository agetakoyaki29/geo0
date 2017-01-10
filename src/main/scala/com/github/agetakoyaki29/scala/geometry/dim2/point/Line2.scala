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
class Dir2(_x: Double, _y: Double) extends Vector2(_x, _y) {

  override val factory: Dim2Factory[_ <: Dir2] = Dir2

  def toDir2: Dir2 = this

  override protected def validate = Dim2.NonZero orElse super.validate

  // ----

  @UpRet def reflect: Dir2 = minus

  // ---- about angle ----

  def align(idx: Int): Boolean = this parallel Dir2.E(idx)

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

  def inRegion1(pt: Point2): Boolean = this dotGt0 pt
  def inRegion2(pt: Point2): Boolean = (this reflect) inRegion1 (pt - this)

  def through(pt: Point2): Boolean = this crossEq0 pt

  /**
   * 0 <= this angle pt <= pi
   * (this sinTo pt) < 0
   */
  def containPoint2(pt: Point2): Boolean = this crossLt0 pt

  /**
   * this sinTo pt * pt.norm
   */
  def distance(pt: Point2): Double = (this cross pt / this.norm).abs
  def distanceSqr(pt: Point2): Double = (this cross pt).sqr / this.normSqr

  /**
   * this.normalized * this cosTo pt * pt.norm
   * pt + this.normal.normalized * -distance
   * this * (this dot pt) / (this dot this)
   */
  def nearest(pt: Point2): Point2 = Point2(this * (this dot pt / this.normSqr))

  // ---- figure to other figure ----

  def same(op: Dir2): Boolean = this parallel op

  def same(line: Line2): Boolean = (this through line.sp) && (this parallel line.dir)

  // def aabb: AABB = AABB.WHOLE

  def intersectLine2(line: Line2): Set[Point2] = ???
  def isIntersectLine2(line: Line2): Boolean = ???

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
  def apply(sp: Point2, dir: Dir2) = new Line2(sp, dir)
  def apply(sp: Point2, ep: Point2) = new Line2(sp, Dir2(sp to ep))
}


class Line2(val sp: Point2, val dir: Dir2) extends Trans2[Line2] with Figure2 {
  val ep = sp unto Point2(dir)

  def updated(sp: Point2, dir: Dir2): Line2 = Line2(sp, dir)
  def updatedSP(sp: Point2): Line2 = updated(sp, dir)
  def updatedDir(dir: Dir2): Line2 = updated(sp, dir)
  def updatedEP(ep: Point2): Line2 = updatedDir(Dir2(sp to ep))

  def reflect: Line2 = Line2(sp unto Point2(dir), dir.reflect)

  // ---- for Trans2 ----

  def +(op: Point2): Line2 = updatedSP(sp+op)
  def -(op: Point2): Line2 = updatedSP(sp-op)

  // ---- for Figure2 ----

  def isIntersect(op: Figure2): Boolean = op match {
    case line: Line2 => this isIntersectLine2 line
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Set[Point2] = op match {
    case line: Line2 => this intersectLine2 line
    case _ => op intersect this
  }

  // ---- copy from Dir2 ----

  def align(idx: Int): Boolean = ???
  def normalDir: Line2 = ???
  def normal(op: Line2): Boolean = ???
  def parallel(op: Line2): Boolean = ???

  def angle: Double = ???
  def angleTo(op: Line2): Double = ???
  def cosTo(op: Line2): Double = ???
  def sinTo(op: Line2): Double = ???

  def inRegion1(pt: Point2): Boolean = ???
  def inRegion2(pt: Point2): Boolean = ???
  def through(pt: Point2): Boolean = ???
  def containPoint2(pt: Point2): Boolean = ???
  def distance(pt: Point2): Double = ???
  def distanceSqr(pt: Point2): Double = ???
  def nearest(pt: Point2): Point2 = ???

  def same(line: Line2): Boolean = ???
  def intersectLine2(line: Line2): Set[Point2] = ???
  def isIntersectLine2(line: Line2): Boolean = ???
}
