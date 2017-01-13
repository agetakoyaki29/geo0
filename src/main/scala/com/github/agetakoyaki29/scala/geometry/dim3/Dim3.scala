package com.github.agetakoyaki29.scala.geometry.dim3

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._
import geometry.{Dim, DimFactory}
import geometry.dim2.Dim2


object Dim3 extends Dim3Factory[Dim3] {
  def apply(x: Double, y: Double, z: Double): Dim3 = new Dim3(x, y, z)
}


class Dim3(val x: Double, val y: Double, val z: Double) extends IndexedSeq[Double] with Dim {

  val factory: Dim3Factory[_ <: Dim3] = Dim3

  // ---- for IndexedSeq ----

  override final def foreach[U](f: Double => U): Unit = ???
  final def apply(idx: Int): Double = ???
  final def length: Int = 3

  // ---- from IndexedSeq ----

  final def zipmap[B](op: Dim3)(f: (Double, Double) => B): IndexedSeq[B] = this zip op map { (tup: (Double, Double)) => f(tup._1, tup._2) }

  @UpRet def updatedD3(idx: Int, elem: Double): Dim3 = factory(super.updated(idx, elem))

  @UpRet def mapD3(f: Double => Double): Dim3 = factory(this map f)

  @UpRet def zipmapD3(op: Dim3)(f: (Double, Double) => Double): Dim3 = factory(this.zipmap(op)(f))

  // ---- basic operators ----

  @UpRet def abs: Dim3 = mapD3{_.abs}

  @UpRet def unary_+(): Dim3 = factory(this)
  @UpRet def unary_-(): Dim3 = mapD3{-_}

  @UpRet def +(op: Dim3): Dim3 = zipmapD3(op) {_+_}
  @UpRet def -(op: Dim3): Dim3 = zipmapD3(op) {_-_}

  @UpRet def *(d: Double): Dim3 = mapD3{_*d}
  @UpRet def /(d: Double): Dim3 = mapD3{_/d}

  // ---- std ----

  override def equals(op: Any) = op match {
    case dim3: Dim3 => this.zipmap(dim3) {_==_} reduce {_||_}
    case _ => false
  }
}


abstract class Dim3Factory[T <: Dim3 : ClassTag] extends DimFactory[T] {
  final val Length: Int = 3

  def apply(x: Double, y: Double, z: Double): T
  def apply(op: Dim3): T = op match {
    case t: T => t
    case dim3 => clone(dim3)
  }
  def apply(seq: Seq[Double]): T = {
    if(seq.length != Length) throw new IllegalArgumentException(s"wrong length seq; found: ${seq.length}, required: ${Length}")
    this(seq(0), seq(1), seq(2))
  }

  def apply(dim2: Dim2): T = ???

  def clone(dim3: Dim3): T = ???
}
