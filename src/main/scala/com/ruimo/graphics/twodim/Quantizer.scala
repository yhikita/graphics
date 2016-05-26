package com.ruimo.graphics.twodim

import scala.collection.immutable
import scala.collection.Searching._

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
// int((0(=sum width) + min(0.2 - 0.1, 0.3)) * 100 / 1.8(=total width) + 0.5) = 6
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
// --
// From index.
// Assume idx = 0
// As idx <= 0, value = range0.min
// --
// From index.
// Assume idx = 6
// width = 1.8 * 6 / 100 = 0.108
// Find 0.108 in widthSumTable => widthSumTableIdx = 0
// range0.min + 0.108 = 0.208
// --
// From index.
// Assume idx = 33
// width = 1.8 * 33 / 100 = 0.594
// Find 0.594 in widthSumTable => widthSumTableIdx = 1
// range1.min + (0.594 - 0.3) = 0.4 + 0.294 = 0.694
// --
// From index.
// Assume idx = 44
// width = 1.8 * 44 / 100 = 0.792
// Find 0.792 in widthSumTable => widthSumTableIdx = 1
// range1.min + (0.792 - 0.3) = 0.4 + 0.492 = 0.892
// --
// From index.
// Assume idx = 94
// width = 1.8 * 94 / 100 = 1.692
// Find 1.692 in widthSumTable => widthSumTableIdx = 2
// range2.min + (1.692 - 0.8) = 2.0 + 0.892 = 2.892
// --
// From index.
// Assume idx = 99
// width = 1.8 * 99 / 100 = 1.782
// Find 1.782 in widthSumTable => widthSumTableIdx = 2
// range2.min + (1.782 - 0.8) = 2.0 + 0.982 = 2.982
// --
// From index.
// Assume idx = 100
// As idx >= resolution, value = range2.max = 3
class Quantizer(val resolution: Int, range: Range*) {
  require(! range.isEmpty)

  val (
    totalWidth: Double,
    minTable: immutable.Vector[Double],
    widthSumTable: immutable.Vector[Double]
  ) = range.foldLeft((0d, immutable.Vector[Double](), immutable.Vector[Double]())) { (t, e) =>
    (t._1 + e.width, t._2 :+ e.min, t._3 :+ t._1)
  }
  val min: Double = range.head.min
  val max: Double = range.last.max

  def toIndex(value: Double): Int = {
    if (value <= min) 0
    else {
      val rangeIdx = {
        val i = minTable.search(value) match {
          case Found(ix) => ix
          case InsertionPoint(ix) => ix - 1
        }
        if (i < 0) 0 else i
      }
      val r = range(rangeIdx)
      val idx = ((widthSumTable(rangeIdx) + (value - r.min).min(r.width)) * resolution / totalWidth + 0.5).toInt
      if (idx >= resolution) resolution - 1 else idx
    }
  }

  def fromIndex(idx: Int): Double =
    if (idx <= 0) min
    else if (idx >= resolution) max
    else {
      val width = totalWidth * idx / resolution
      val wsIdx = widthSumTable.search(width) match {
        case Found(ix) => ix
        case InsertionPoint(ix) => ix - 1
      }
      range(wsIdx).min + width - widthSumTable(wsIdx)
    }
}
