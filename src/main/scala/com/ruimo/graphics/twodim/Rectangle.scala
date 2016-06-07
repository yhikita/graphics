package com.ruimo.graphics.twodim

case class Rectangle(
  x: Int, y: Int, width: Int, height: Int
) {
  def contains(xx: Int, yy: Int): Boolean =
    x <= xx && xx < x + width &&
    y <= yy && yy < y + height

  def +(offset: Offset): Rectangle = copy(
    x = x + offset.x,
    y = y + offset.y
  )
}

