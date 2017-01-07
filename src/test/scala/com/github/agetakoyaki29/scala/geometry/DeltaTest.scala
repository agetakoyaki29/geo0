package com.github.agetakoyaki29.scala.geometry

import org.scalatest.WordSpec
import org.scalatest.Matchers
import com.github.agetakoyaki29.scala.geometry.Delta.delta
import com.github.agetakoyaki29.scala.geometry.Delta.RichDouble


class RichDoubleTest extends WordSpec with Matchers {
  val NaN = Double.NaN
  val PositiveInfinity = Double.PositiveInfinity
  val NegativeInfinity = Double.NegativeInfinity
  val MinPositiveValue = Double.MinPositiveValue
  val MinNormal = Delta.MinNormal  // java.lang.Double.MIN_NORMAL
  val MaxVal = Double.MaxValue
  val MinVal = Double.MinValue
  val Zero = 0d
  val PosZero = +0d   // TODO rename or delete
  val NegZero = -0d
  // val One = 1d

  val double1 = 2d
  val double2 = Math.PI
  val double3 = -0.524d
  val minP1 = Double.MinPositiveValue.scalb(4)
  val NonNormal1 = MinNormal.nextDown

  "about +,-,*,/,%(5 arithmetic operations)" when {
    "positive any / PosZero === Double.PositiveInfinity" in {
      assert(double2 / PosZero === Double.PositiveInfinity)
    }
    "positive any / NegZero === Double.NegativeInfinity" in {
      assert(double2 / NegZero === Double.NegativeInfinity)
    }
  }

  "about ==,!=,<,<=,>,>=(6 comparison operations)" when {
    "NaN !== Nan" in {
      assert(NaN !== NaN)
    }
  }

  "RichDouble" when {
    "NaN" should {
      "isNaN" in {
        assert(NaN.isNaN)
      }
      behave like testUlp(NaN)
      behave like testNormal(NaN)
    }
    "isInfs" when {
      "PositiveInfinity" should {
        "isInfinite" in {
          assert(PositiveInfinity.isInfinite)
        }
        behave like testUlp(PositiveInfinity)
        behave like testNormal(PositiveInfinity)
      }
      "NegativeInfinity" should {
        "isInfinite" in {
          assert(NegativeInfinity.isInfinite)
        }
        behave like testUlp(NegativeInfinity)
        behave like testNormal(NegativeInfinity)
      }
    }
    "isZeros" when {
      "Zero" should {
        "isZero" in {
          assert(Zero.isZero)
        }
        "ulp === Double.MinPositiveValue" in {
          assert(Zero.ulp === Double.MinPositiveValue)
        }
        behave like testUlp(Zero)
        behave like testNormal(Zero)
      }
      "NegZero" should {
        "isZero" in {
          assert(NegZero.isZero)
        }
        "Math.signum: Double === -0d" in {
          assert(Math.signum(NegZero) === -0d)
        }
        "signum: Int === 0" in {
          assert(NegZero.signum === 0)
        }
        "NegZero === -PosZero" in {
          assert(NegZero === -PosZero)
        }
        behave like testUlp(NegZero)
        behave like testNormal(NegZero)
      }
    }
    "MinPositiveValue" should {
      "ulp === Double.MinPositiveValue" in {
        assert(MinPositiveValue.ulp === Double.MinPositiveValue)
      }
      behave like testUlp(MinPositiveValue)
      behave like testNormal(MinPositiveValue)
    }
    "MinNormal" should {
      "ulp === Double.MinPositiveValue" in {
        assert(MinNormal.ulp === Double.MinPositiveValue)
      }
      behave like testUlp(MinNormal)
      behave like testNormal(MinNormal)
    }
    "MaxVal" should {
      "MaxVal === -MinVal" in {
        assert(MaxVal === -MinVal)
      }
      behave like testUlp(MaxVal)
      behave like testNormal(MaxVal)
    }
    "MinVal" should {
      "MinVal === -MaxVal" in {
        assert(MinVal === -MaxVal)
      }
      behave like testUlp(MinVal)
      behave like testNormal(MinVal)
    }

    "double1" should {
      behave like testUlp(double1)
      behave like testNormal(double1)
    }
    "double2" should {
      behave like testUlp(double2)
      behave like testNormal(double2)
    }
    "double3" should {
      behave like testUlp(double3)
      behave like testNormal(double3)
    }
    "minP1" should {
      behave like testUlp(minP1)
      behave like testNormal(minP1)
    }
    "NonNormal1" should {
      behave like testUlp(NonNormal1)
      behave like testNormal(NonNormal1)
    }

    def testNormal(double: Double) = double match {
      case _ if !double.isNormal => {
        "non normal double is !NaN && !Inf && !Zero" in {
          assert(!(double.isNaN || double.isInfinite || double.isZero))
        }
        "non normal double abs < java.lang.Double.MIN_NORMAL" in {
          assert(double.abs < java.lang.Double.MIN_NORMAL)
        }
      }
      case _ if double.isNormal => double match {
        case _ if double.isNaN =>
          "it getExponent === java.lang.Double.MAX_EXPONENT + 1" in {
            assert(double.getExponent === (java.lang.Double.MAX_EXPONENT+1))
          }
        case _ if double.isInfinite =>
          "it getExponent === java.lang.Double.MAX_EXPONENT + 1" in {
            assert(double.getExponent === (java.lang.Double.MAX_EXPONENT+1))
          }
        case _ if double.isZero =>
          "it getExponent === java.lang.Double.MIN_EXPONENT - 1" in {
            assert(double.getExponent === (java.lang.Double.MIN_EXPONENT-1))
          }
        case _=>
          "normal double abs >= java.lang.Double.MIN_NORMAL" in {
            assert(double.abs >= java.lang.Double.MIN_NORMAL)
          }
          "normal double.getExponent === double.abs.log2.rint" ignore {
            assert(double.getExponent === double.abs.log2.rint)
          }
      }
    }

    def testUlp(it: Double) = it match {
      case _ if it.isNaN =>
        "it.ulp isNaN" in {
          assert(it.ulp.isNaN)
        }
      case _ if it.isInfinite =>
        "it.ulp === Double.PositiveInfinity" in {
          assert(it.ulp === Double.PositiveInfinity)
        }
      case _ =>
        "it + it.ulp !== it" in {
          assert(it + it.ulp !== it)
        }
        "it + it.ulp.scalb(-1) === it" in {
          assert(it + it.ulp.scalb(-1) === it)
        }
        "it + it.ulp === it.nextUp" in {
          assert(it + it.ulp === it.nextUp)
        }
        "it - it.ulp === it.nextDown" in {
          assert(it - it.ulp === it.nextDown)
        }
        "it.ulp > 0" in {
          assert(it.ulp > 0)
        }
        "it.ulp === (-it).ulp" in {
          assert(it.ulp === (-it).ulp)
        }
    }

  }
}
