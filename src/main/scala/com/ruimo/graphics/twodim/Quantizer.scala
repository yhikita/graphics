package com.ruimo.graphics.twodim

class Quantizer(min: Double = 0, max: Double, resolution: Int) {
  def toIndex(value: Double): Int = {
    val idx = ((value - min) * resolution / (max - min) + 0.5).toInt
    if (idx < 0) 0
    else if (idx >= resolution) resolution - 1
    else idx
  }

  def fromIndex(idx: Int): Double = min + (max - min) * idx / resolution
}
