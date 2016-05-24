package com.ruimo.graphics.twodim

case class Range(min: Double = 0, max: Double) {
  require(min <= max, "min(=" + min + ") should be less than or equal to max(=" + max + ")")

  val width: Double = max - min
}

