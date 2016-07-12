package com.ruimo.graphics.twodim

import java.awt.image.BufferedImage

import scala.collection.immutable

case class Bits2d(
  width: Int, height: Int,
  visibleRect: Rectangle, bits: immutable.BitSet
) {
  val offsetX: Int = visibleRect.x
  val offsetY: Int = visibleRect.y

  def apply(x: Int, y: Int): Boolean = {
    if (! visibleRect.contains(x, y))
      throw new IllegalArgumentException("(" + x + ", " + y + ") is out of bounds " + visibleRect)
    bits(x - offsetX + (y - offsetY) * visibleRect.width)
  }
}

object Bits2d {
  def errorInRaster(
    src: Bits2d, dest: Bits2d, dstOffsetX: Int, dstOffsetY: Int
  ): Int = {
    0
  }

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

    Bits2d(img.getWidth, img.getHeight, rect, builder.result())
  }
}

