package com.ruimo.graphics.twodim

case class Rgb(rgb: Int) extends AnyVal {
  def blue = rgb & 0xff
  def green = (rgb >> 8) & 0xff
  def red = (rgb >> 16) & 0xff
  def brightness: Int = {
    val r = red
    val g = green
    val b = blue
    (r + r + r + b + g + g + g + g) / 8 // Y = 0.375 R + 0.5 G + 0.125 B
  }
}
