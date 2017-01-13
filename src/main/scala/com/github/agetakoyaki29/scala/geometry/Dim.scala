package com.github.agetakoyaki29.scala.geometry

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._


trait Dim extends IndexedSeq[Double] {

  // val factory: DimFactory[_ <: Dim]

  final def isZero: Boolean = map{_.isZero}.reduce{_&&_}
  final def isInfinite: Boolean = !isNaN && map{_.isInfinite}.reduce{_||_}
  final def isNaN: Boolean = map{_.isNaN}.reduce{_||_}

  def norm: Double = normSqr.sqrt
  /**
   * this map {d => d*d} reduce {_+_}
   * this dot this
   */
  def normSqr: Double = this map {d => d*d} sum

  // -- std --

  override def toString = this.getClass.getSimpleName + "(" + mkString(", ") + ")"  // for convenience

}
