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
  val PositiveZero = +0d   // TODO rename or delete
  val NegativeZero = -0d
  // val One = 1d
  val PositiveDouble = +313.523
  val NegativeDouble = -62.66

  val double1 = 2d
  val double2 = Math.PI
  val double3 = -0.524d
  val minP1 = Double.MinPositiveValue.scalb(4)
  val NonNormal1 = MinNormal.nextDown

  "about ==,!=,<,<=,>,>=(6 comparison operations)" when {
    "NaN !== Nan" in {
      assert(NaN !== NaN)
    }
    "+Zero === -Zero" in {
      assert(PositiveZero === NegativeZero)
    }
  }

  "about +,-,*,/,%(5 arithmetic operations)" when {
    "Inf + Inf === Inf (same sign)" in {
      assert(PositiveInfinity + PositiveInfinity === Double.PositiveInfinity)
      assert(NegativeInfinity + NegativeInfinity === Double.NegativeInfinity)
    }
    "Inf + Inf is NaN (different sign)" in {
      assert((NegativeInfinity + PositiveInfinity).isNaN)
      assert((PositiveInfinity + NegativeInfinity).isNaN)
    }

    "Inf * Zero is NaN (and vise versa)" in {
      assert((PositiveInfinity * PositiveZero).isNaN)
      assert((NegativeInfinity * NegativeZero).isNaN)
      assert((PositiveInfinity * NegativeZero).isNaN)
      assert((NegativeInfinity * PositiveZero).isNaN)
      assert((PositiveZero * PositiveInfinity).isNaN)
      assert((NegativeZero * NegativeInfinity).isNaN)
      assert((NegativeZero * PositiveInfinity).isNaN)
      assert((PositiveZero * NegativeInfinity).isNaN)
    }

    "Inf / Inf isNaN" in {
      assert((PositiveInfinity / PositiveInfinity).isNaN)
      assert((NegativeInfinity / NegativeInfinity).isNaN)
      assert((PositiveInfinity / NegativeInfinity).isNaN)
      assert((NegativeInfinity / PositiveInfinity).isNaN)
    }
    "any(non Inf) / Inf === Zero" in {
      assert((PositiveDouble / PositiveInfinity).isZero)
      assert((NegativeDouble / NegativeInfinity).isZero)
      assert((PositiveDouble / NegativeInfinity).isZero)
      assert((NegativeDouble / PositiveInfinity).isZero)
      // assert((PositiveDouble / PositiveInfinity).isPositiveZero)
      // assert((NegativeDouble / NegativeInfinity).isPositiveZero)
      // assert((PositiveDouble / NegativeInfinity).isNegativeZero)
      // assert((NegativeDouble / PositiveInfinity).isNegativeZero)
      assert((PositiveZero / PositiveInfinity).isZero)
      assert((NegativeZero / NegativeInfinity).isZero)
      assert((PositiveZero / NegativeInfinity).isZero)
      assert((NegativeZero / PositiveInfinity).isZero)
      // assert((PositiveZero / PositiveInfinity).isPositiveZero)
      // assert((NegativeZero / NegativeInfinity).isPositiveZero)
      // assert((PositiveZero / NegativeInfinity).isNegativeZero)
      // assert((NegativeZero / PositiveInfinity).isNegativeZero)
    }
    "Zero / Zero isNaN" in {
      assert((PositiveZero / PositiveZero).isNaN)
      assert((NegativeZero / NegativeZero).isNaN)
      assert((PositiveZero / NegativeZero).isNaN)
      assert((NegativeZero / PositiveZero).isNaN)
    }
    "any(non Zero) / Zero === Double.PositiveInfinity (same sign)" in {
      assert(PositiveDouble / PositiveZero === Double.PositiveInfinity)
      assert(NegativeDouble / NegativeZero === Double.PositiveInfinity)
      assert(PositiveInfinity / PositiveZero === Double.PositiveInfinity)
      assert(NegativeInfinity / NegativeZero === Double.PositiveInfinity)
    }
    "any(non Zero) / Zero === Double.NegativeInfinity (different sign)" in {
      assert(PositiveDouble / NegativeZero === Double.NegativeInfinity)
      assert(NegativeDouble / PositiveZero === Double.NegativeInfinity)
      assert(PositiveInfinity / NegativeZero === Double.NegativeInfinity)
      assert(NegativeInfinity / PositiveZero === Double.NegativeInfinity)
    }
  }

  "RichDouble" when {
    s"NaN($NaN)" should {
      "isNaN" in {
        assert(NaN.isNaN)
      }
      behave like testUlp(NaN)
      behave like testNormal(NaN)
    }
    "isInfs" when {
      s"PositiveInfinity($PositiveInfinity)" should {
        "isInfinite" in {
          assert(PositiveInfinity.isInfinite)
        }
        behave like testUlp(PositiveInfinity)
        behave like testNormal(PositiveInfinity)
      }
      s"NegativeInfinity($NegativeInfinity)" should {
        "isInfinite" in {
          assert(NegativeInfinity.isInfinite)
        }
        behave like testUlp(NegativeInfinity)
        behave like testNormal(NegativeInfinity)
      }
    }
    "isZeros" when {
      s"PositiveZero($PositiveZero)" should {
        "isZero" in {
          assert(PositiveZero.isZero)
        }
        "ulp === Double.MinPositiveValue" in {
          assert(PositiveZero.ulp === Double.MinPositiveValue)
        }
        behave like testUlp(PositiveZero)
        behave like testNormal(PositiveZero)
      }
      s"NegativeZero($NegativeZero)" should {
        "isZero" in {
          assert(NegativeZero.isZero)
        }
        "Math.signum: Double === -0d" in {
          assert(Math.signum(NegativeZero) === -0d)
        }
        "signum: Int === 0" in {
          assert(NegativeZero.signum === 0)
        }
        "NegativeZero === -PositiveZero" in {
          assert(NegativeZero === -PositiveZero)
        }
        behave like testUlp(NegativeZero)
        behave like testNormal(NegativeZero)
      }
    }
    s"MinPositiveValue($MinPositiveValue)" should {
      "ulp === Double.MinPositiveValue" in {
        assert(MinPositiveValue.ulp === Double.MinPositiveValue)
      }
      behave like testUlp(MinPositiveValue)
      behave like testNormal(MinPositiveValue)
    }
    s"MinNormal($MinNormal)" should {
      "ulp === Double.MinPositiveValue" in {
        assert(MinNormal.ulp === Double.MinPositiveValue)
      }
      behave like testUlp(MinNormal)
      behave like testNormal(MinNormal)
    }
    s"MaxVal($MaxVal)" should {
      "MaxVal === -MinVal" in {
        assert(MaxVal === -MinVal)
      }
      behave like testUlp(MaxVal)
      behave like testNormal(MaxVal)
    }
    s"MinVal($MinVal)" should {
      "MinVal === -MaxVal" in {
        assert(MinVal === -MaxVal)
      }
      behave like testUlp(MinVal)
      behave like testNormal(MinVal)
    }

    s"double1($double1)" should {
      behave like testUlp(double1)
      behave like testNormal(double1)
    }
    s"double2($double2)" should {
      behave like testUlp(double2)
      behave like testNormal(double2)
    }
    s"double3($double3)" should {
      behave like testUlp(double3)
      behave like testNormal(double3)
    }
    s"minP1($minP1)" should {
      behave like testUlp(minP1)
      behave like testNormal(minP1)
    }
    s"NonNormal1($NonNormal1)" should {
      behave like testUlp(NonNormal1)
      behave like testNormal(NonNormal1)
    }

    def testNormal(double: Double) = double match {
      case _ if !double.isNormal => {
        "non normal double !isNaN && !isInf && !isZero" in {
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
