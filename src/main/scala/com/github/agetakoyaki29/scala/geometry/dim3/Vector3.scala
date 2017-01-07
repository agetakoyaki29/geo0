package com.github.agetakoyaki29.scala.geometry.dim3

import com.github.agetakoyaki29.scala.sameret.{SameRet, UpRet}
import com.github.agetakoyaki29.scala.geometry
import geometry.Delta._


object Vector3 extends Dim3Factory[Vector3] {
  def apply(x: Double, y: Double, z: Double) = new Vector3(x, y, z)
}


@SameRet
class Vector3(_x: Double, _y: Double, _z: Double) extends Dim3(_x, _y, _z) {

  override val factory: Dim3Factory[_ <: Vector3] = Vector3

  final def dot(op: Vector3): Double = zipmapD3(op) {_*_} sum
  final def dotEq0(op: Vector3): Boolean = ???
  final def dotGt0(op: Vector3): Boolean = ???
  final def dotLt0(op: Vector3): Boolean = ???

  final def cross(op: Vector3): Vector3 = ???

}
