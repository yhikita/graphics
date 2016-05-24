package com.ruimo.graphics.twodim

// resolution = 100
// range0: [0.1, 0.4) width = 0.4 - 0.1 = 0.3
// range1: [0.4, 0.9) width = 0.9 - 0.4 = 0.5
// range2: [2.0, 3.0) width = 3.0 - 2.0 = 1.0
// Range min is inclusive, max is exclusive.
// Each range is exclusive. (Does not permit range0[0.1, 0.9), range1[0.8, 1.0))
// --
// totalWidth = 0.3 + 0.5 + 1.0 = 1.8
// minTable = [0.1, 0.4, 2.0]
// widthSumTable = [0, 0.3, 0.3 + 0.5] = [0, 0.3, 0.8]
// --
// Finding index.
// Assume value = 0
// As value <= range0.min index = 0
// --
// Finding index.
// Assume value = 0.2
// Seach 0.2 into minTable => range0 => sumWidth in widthSumTable = 0
// int((0(=sum width) + min(0.2 - 0.1, 0.3)) * 100 / 1.8(=total width) + 0.5) = 5
// --
// Finding index.
// Assume value = 0.7
// Seach 0.7 into minTable => range1 => sumWidth in widthSumTable = 0.3
// int((0.3(=sum width) + min(0.7 - 0.4, 0.5)) * 100 / 1.8(=total width) + 0.5) = 33
// --
// Finding index.
// Assume value = 0.9
// Search 1.5 into minTable => range1 => sumWidth in widthSumTable = 0.3
// int((0.3 + min(0.9 - 0.4, 0.5)) * 100 / 1.8 + 0.5) = 44
// --
// Finding index.
// Assume value = 1.5
// Search 1.5 into minTable => range1 => sumWidth in widthSumTable = 0.3
// int((0.3 + min(1.5 - 0.4, 0.5)) * 100 / 1.8 + 0.5) = 44
// --
// Finding index.
// Assume value = 2.9
// Search 2.9 into minTable => range2 => sumWidth in widthSumTable = 0.8
// int((0.8 + min(2.9 - 2.0, 1.0)) * 100 / 1.8 + 0.5) = 94
// --
// Finding index.
// Assume value = 3.0
// Search 3.0 into minTable => range2 => sumWidth in widthSumTable = 0.8
// int((0.8 + min(3.0 - 2.0, 1.0)) * 100 / 1.8 + 0.5) = 100 (100 >= resolution answer is 99(=resolution - 1)).
class Quantizer(resolution: Int, range: Range*) {
  require(! range.isEmpty)

  val (
    totalWidth: Double,
    minTable: immutable.Vector[Double],
    widthSumTable: immutable.Vector[Double]
  ) = range.foldLeft((0, immutable.Vector[Double](), immutable.Vector(0.0f))) { (t, e) =>
    (t._1 + e.width, t._2 + e.min, t._3 + t._1)
  }
  val min: Double = range.head.min

  def toIndex(value: Double): Int = {
    if (value <= min) 0

    val idx = ((value - min) * resolution / (max - min) + 0.5).toInt
    if (idx < 0) 0
    else if (idx >= resolution) resolution - 1
    else idx
  }

  def fromIndex(idx: Int): Double = min + (max - min) * idx / resolution
}
