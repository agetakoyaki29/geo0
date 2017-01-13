package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._


trait DimFactory[T <: Dim] {
  def Length: Int
  val Indices: Range = 0 until Length
  def Other(idx: Int): IndexedSeq[Int] = Indices filter {_ != idx}

  def apply(seq: Seq[Double]): T
  def apply(elem: Double): T = this.apply(Indices map {_ => elem})

  val Zero: T = this.apply(0d)
  val Infinity: T = this.apply(Double.PositiveInfinity)
  val NaN: T = this.apply(Double.NaN)

  def E(idx: Int): T = this.apply(Zero.updated(idx, 1d))
  def F(idx: Int): T = this.apply(this.apply(1d).updated(idx, 0d))

  // ---- for validation ----

  val NotNaN:      T => T = dim => { require(! dim.isNaN,      "required not NaN " + dim.getClass.getSimpleName);      dim }
  val NotInfinite: T => T = dim => { require(! dim.isInfinite, "required not Infinite " + dim.getClass.getSimpleName); dim }
  val NotZero:     T => T = dim => { require(! dim.isZero,     "required not Zero " + dim.getClass.getSimpleName);     dim }
  val Identity:    T => T = identity

}
