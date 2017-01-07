package com.github.agetakoyaki29.scala.geometry.dim2.point


trait Trans2[+Repr] {
  def +(pt: Point2): Repr
  def -(pt: Point2): Repr

  def from(pt: Point2): Repr = this-pt
  def unfrom(pt: Point2): Repr = this+pt
}
