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

  def clip(maxX: Int, maxY: Int): Rectangle = {
    val newX = if (maxX < x) maxX else x
    val newWidth = if (maxX < newX + width) maxX - newX else width
    val newY = if (maxY < y) maxY else y
    val newHeight = if (maxY < newY + height) maxY - newY else height
    Rectangle(newX, newY, newWidth, newHeight)
  }

  val area: Int = width * height
}

