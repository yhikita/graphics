package com.ruimo.graphics.twodim

case class Line(x: Int, y: Int, w: Int, h: Int) {
  require(0 <= x, "x (=" + x + ") should be zero or greater than zero")
  require(0 <= y, "y (=" + x + ") should be zero or greater than zero")
  require(0 <= w, "w (=" + w + ") should be zero or greater than zero")
  require(0 <= h, "h (=" + h + ") should be zero or greater than zero")
}
