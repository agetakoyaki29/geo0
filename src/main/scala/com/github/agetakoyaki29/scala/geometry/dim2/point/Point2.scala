package com.github.agetakoyaki29.scala.geometry.dim2.point

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.dim2.{Dim2Factory, Dim2, Vector2}


object Point2 extends Dim2Factory[Point2] {
  def apply(x: Double, y: Double): Point2 = new Point2(x, y)

  val O: Point2 = Zero
}


@SameRet
class Point2(_x: Double, _y: Double) extends Vector2(_x, _y) with Trans2[Point2] {

  override val factory: Dim2Factory[_ <: Point2] = Point2

  def toPoint2: Point2 = this

  // ----

  /**
   * return distance between two point >= 0
   */
  def distance(op: Point2): Double = (this to op).norm
  def distanceSqr(op: Point2): Double = (this to op).normSqr

  def same(op: Point2) = this.zipmap(op) {eq(_, _)} reduce {_&&_}

  // ---- for Trans2 ----

  def +(op: Point2): Point2 = this.+(op.toDim2)
  def -(op: Point2): Point2 = this.-(op.toDim2)

  // ---- use Trans2 ----

  def to[A <: Trans2[A]](trans: A): A = trans from this
  // def to[A : ClassTag](any: A): A = any match {
  //   case trans2: Trans2[A] => trans2 from this
  //   case any => any
  // }

  def unto[A <: Trans2[A]](trans: A): A = trans unfrom this
  // def unto[A : ClassTag](any: A): A = any match {
  //   case trans2: Trans2[A] => trans2 unfrom this
  //   case any => any
  // }

  // def conjugate[A : ClassTag, B : ClassTag](f: A => B)(arg: A): B = unto(f(to(arg)))

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
