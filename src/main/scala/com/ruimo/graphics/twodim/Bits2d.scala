package com.ruimo.graphics.twodim


import java.awt.image.BufferedImage

import scala.collection.immutable

case class Bits2d(
  offsetX: Int, offsetY: Int, width: Int, height: Int,
  rect: Rectangle, bits: immutable.BitSet
) {
  def apply(x: Int, y: Int): Boolean = {
    if (! rect.contains(x, y))
      throw new IllegalArgumentException("(" + x + ", " + y + ") is out of bounds " + rect)
    bits(x - offsetX + (y - offsetY) * width)
  }
}

object Bits2d {
  def apply(
    img: BufferedImage, threshold: Int = 128,
    area: Option[Rectangle] = None
  ): Bits2d = {
    val rect: Rectangle = area.getOrElse(Rectangle(0, 0, img.getWidth, img.getHeight))
    val builder = immutable.BitSet.newBuilder
    builder.sizeHint(rect.width * rect.height)
    builder.clear()
    var idx = 0

    for {
      y <- rect.y until rect.y + rect.height
      x <- rect.x until rect.x + rect.width
    } {
      if (Rgb(img.getRGB(x, y)).brightness >= threshold) {
        builder += idx
      }
      idx += 1
    }

    Bits2d(rect.x, rect.y, rect.width, rect.height, rect, builder.result())
  }
}

