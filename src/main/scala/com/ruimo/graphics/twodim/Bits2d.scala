package com.ruimo.graphics.twodim

import scala.annotation.tailrec
import java.awt.Color
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.nio.file.Path

import scala.collection.immutable

class Bits2d(
  val width: Int, val height: Int,
  val visibleRect: Rectangle, bits: immutable.BitSet
) {
  val offsetX: Int = visibleRect.x
  val offsetY: Int = visibleRect.y

  def apply(x: Int, y: Int): Boolean = {
    if (! visibleRect.contains(x, y))
      throw new IllegalArgumentException("(" + x + ", " + y + ") is out of bounds " + visibleRect)
    bits(x - offsetX + (y - offsetY) * visibleRect.width)
  }

  def save(path: Path, imageType: String = "png") {
    val buf = new BufferedImage(visibleRect.width, visibleRect.height, BufferedImage.TYPE_INT_BGR)
    val g = buf.createGraphics()
    for {
      x <- 0 until visibleRect.width
      y <- 0 until visibleRect.height
    } {
      val vx = x + visibleRect.x
      val vy = y + visibleRect.y
      if (apply(vx, vy)) {
        g.setColor(Color.BLACK)
      } else {
        g.setColor(Color.WHITE)
      }
      g.drawLine(x, y, x, y)
    }
    ImageIO.write(buf, imageType, path.toFile)
  }

  def find(
    tofind: Bits2d, maxError: Int,
    xstart: Int, ystart: Int, xend: Int, yend: Int
  ): Option[Offset] = {
    for {
      yoffset <- ystart to yend
      xoffset <- xstart to xend
    } {
      @tailrec def finder(x: Int, y: Int, sumError: Int): Option[Offset] =
        if (sumError > maxError) {
          None
        }
        else {
          val error: Int = if (this(x + xoffset, y + yoffset) != tofind(x, y)) 1 else 0
          val newx = x + 1
          if (newx >= tofind.width) {
            val newy = y + 1
            if (newy >= tofind.height) {
              Some(Offset(xoffset, yoffset))
            }
            else {
              finder(0, newy, error + sumError)
            }
          }
          else {
            finder(newx, y, error + sumError)
          }
        }

      finder(0, 0, 0) match {
        case s: Some[Offset] => return s
        case None => {}
      }
    }

    None
  }
}

object Bits2d {
  def errorInRaster(
    src: Bits2d, dest: Bits2d, dstOffsetX: Int, dstOffsetY: Int
  ): Int = {
    0
  }

  def apply(
    img: BufferedImage, threshold: Int = 200,
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

    new Bits2d(img.getWidth, img.getHeight, rect, builder.result())
  }
}

