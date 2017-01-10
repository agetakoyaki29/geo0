package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.geometry
import geometry.dim2.point.Point2


trait Figure2 {
  // def same
  // def aabb
  // def contain(op: Figure2): Boolean
  def intersect(op: Figure2): Set[Point2]
  def isIntersect(op: Figure2): Boolean
}
