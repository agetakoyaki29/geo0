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

}
