package com.ruimo.graphics.twodim

import Math.{PI => Pi}

object Degree {
  def toRadian(degree: Double): Double = degree * Pi / 180
}
