package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.geometry
import geometry.dim2.point.Point2


trait Figure2 {
// def aabb: Rect
  // def distance(pt: Point2): Double
  // def through(pt: Point2): Boolean
  // def nearest(pt: Point2): Point2

  // def same(op: Figure2): Boolean
  // def contain(op: Figure2): Boolean
  def isIntersect(op: Figure2): Boolean
  def intersect(op: Figure2): Set[Point2]
}
