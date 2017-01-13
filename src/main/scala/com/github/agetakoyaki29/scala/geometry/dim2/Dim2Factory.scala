package com.github.agetakoyaki29.scala.geometry.dim2

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.geometry
import geometry.DimFactory


abstract class Dim2Factory[T <: Dim2 : ClassTag] extends DimFactory[T] {
  final val Length: Int = 2

  def apply(x: Double, y: Double): T
  def apply(op: Dim2): T = clone(op)
  def apply(seq: Seq[Double]): T = {
    require(seq.length == Length, s"wrong length seq; found: ${seq.length}, required: ${Length}")
    this(seq(0), seq(1))
  }

  def clone(dim2: Dim2): T = ???

}
