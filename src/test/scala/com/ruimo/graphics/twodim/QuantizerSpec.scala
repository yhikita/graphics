package com.ruimo.graphics.twodim

import org.specs2.mutable.Specification
import scala.collection.immutable

class QuantizerSpec extends Specification {
  "Quantizer" should {
    "Can find index" in {
      val q = new Quantizer(100, Range(0.1, 0.4), Range(0.4, 0.9), Range(2.0, 3.0))
      q.totalWidth === (0.4 - 0.1) + (0.9 - 0.4) + (3.0 - 2.0)
      q.min === 0.1
      q.max === 3.0
      q.minTable === immutable.Vector(0.1, 0.4, 2.0)
      q.widthSumTable.size === 3
      q.widthSumTable(0) must beCloseTo(0, 0.01)
      q.widthSumTable(1) must beCloseTo(0.3, 0.01)
      q.widthSumTable(2) must beCloseTo(0.8, 0.01)

      q.toIndex(0) === 0
      q.toIndex(0.2) === 6
      q.toIndex(0.7) === 33
      q.toIndex(0.9) === 44
      q.toIndex(1.5) === 44
      q.toIndex(2.9) === 94
      q.toIndex(3.0) === 99
    }

    "Can convert index to value" in {
      val q = new Quantizer(100, Range(0.1, 0.4), Range(0.4, 0.9), Range(2.0, 3.0))

      q.fromIndex(0) === 0.1
      q.fromIndex(6) must beCloseTo(0.208, 0.01)
      q.fromIndex(33) must beCloseTo(0.694, 0.01)
      q.fromIndex(44) must beCloseTo(0.892, 0.01)
      q.fromIndex(94) must beCloseTo(2.892, 0.01)
      q.fromIndex(100) must beCloseTo(3.0, 0.01)
    }
  }
}

