package com.ruimo.graphics.twodim


import java.awt.image.BufferedImage

import scala.collection.immutable

case class Bits2d(width: Int, height: Int, bits: immutable.BitSet) {
  def apply(x: Int, y: Int): Boolean = bits(x + y * width)
}

object Bits2d {
  def apply(
    img: BufferedImage, threshold: Int,
    area: Option[Rectangle] = None
  ): Bits2d = {
    val builder = immutable.BitSet.newBuilder
    val rect: Rectangle = area.getOrElse(Rectangle(0, 0, img.getWidth, img.getHeight))
    var idx = 0

    for {
      x <- rect.x until rect.x + rect.width
      y <- rect.y until rect.y + rect.height
    } {
      if (Rgb(img.getRGB(x, y)).brightness >= threshold) builder += idx
      idx += 1
    }

    Bits2d(rect.width, rect.height, builder.result())
  }
}

