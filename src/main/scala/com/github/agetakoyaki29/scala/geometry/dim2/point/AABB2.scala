package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.dim2.{Dim2Factory, Dim2, Vector2}


object Corner2 extends Dim2Factory[Corner2] {
  def apply(x: Double, y: Double): Corner2 = new Corner2(x, y)
}


@SameRet
class Corner2(_x: Double, _y: Double) extends Vector2(_x.abs, _y.abs) {

  override val factory: Dim2Factory[_ <: Corner2] = Corner2

  def toCorner2: Corner2 = this

  // ----

  def center: Point2 = O
  def minmin: Point2 = Point2(-x, -y)
  def minmax: Point2 = Point2(-x,  y)
  def maxmin: Point2 = Point2( x, -y)
  def maxmax: Point2 = Point2( x,  y)

  def isConcentric(aabb: AABB2): Boolean = center same aabb.center

  // ---- figure to point ----

  def through(pt: Point2): Boolean = ???

  def containPoint2(pt: Point2): Boolean = ???

  def distance(pt: Point2): Double = ???
  def distanceSqr(pt: Point2): Double = ???

  def nearest(pt: Point2): Point2 = ???

  // ---- figure to other figure ----

  def same(op: Corner2): Boolean = Point2(this) same Point2(op)

  def same(aabb: AABB2): Boolean = (aabb.center same O) && (this same aabb.corner)

  // def aabb: AABB = AABB(O, this)

  def isIntersectCircle2(circle: Circle2): Boolean = ???
  def intersectCircle2(circle: Circle2): Set[Point2] = ???

  def isIntersectAABB2(aabb: AABB2): Boolean = ???
  def intersectAABB2(aabb: AABB2): Set[Point2] = ???

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


object AABB2 {
  def apply(sp: Point2, corner: Corner2) = new AABB2(sp, corner)
  def apply(sp: Point2, ep: Point2) = new AABB2(sp, Corner2(sp to ep))
}


class AABB2(val sp: Point2, val corner: Corner2) extends Trans2[AABB2] with Figure2 {
  val ep = sp unto Point2(corner)

  def updated(sp: Point2, corner: Corner2): AABB2 = AABB2(sp, corner)
  def updatedSP(sp: Point2): AABB2 = updated(sp, corner)
  def updatedRange(corner: Corner2): AABB2 = updated(sp, corner)
  def updatedEP(ep: Point2): AABB2 = updatedRange(Corner2(sp to ep))

  // ---- for Trans2 ----

  def +(op: Point2): AABB2 = updatedSP(sp+op)
  def -(op: Point2): AABB2 = updatedSP(sp-op)

  // ---- for Figure2 ----

  def isIntersect(op: Figure2): Boolean = op match {
    case circle: Circle2 => this isIntersectCircle2 circle
    case aabb: AABB2     => this isIntersectAABB2 aabb
    case _ => op isIntersect this
  }

  def intersect(op: Figure2): Set[Point2] = op match {
    case circle: Circle2 => this intersectCircle2 circle
    case aabb: AABB2     => this intersectAABB2 aabb
    case _ => op intersect this
  }

  // ---- copy from Corner2 ----

  def center: Point2 = ???
  def minmin: Point2 = ???
  def minmax: Point2 = ???
  def maxmin: Point2 = ???
  def maxmax: Point2 = ???
  def isConcentric(aabb: AABB2): Boolean = ???

  def through(pt: Point2): Boolean = ???
  def containPoint2(pt: Point2): Boolean = ???
  def distance(pt: Point2): Double = ???
  def distanceSqr(pt: Point2): Double = ???
  def nearest(pt: Point2): Point2 = ???

  def same(aabb: AABB2): Boolean = ???
  def isIntersectCircle2(circle: Circle2): Boolean = ???
  def isIntersectAABB2(aabb: AABB2): Boolean = ???
  def intersectCircle2(circle: Circle2): Set[Point2] = ???
  def intersectAABB2(aabb: AABB2): Set[Point2] = ???
}
