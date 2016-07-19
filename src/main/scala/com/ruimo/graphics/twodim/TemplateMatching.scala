package com.ruimo.graphics.twodim

import scala.annotation.tailrec
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

object TemplateMatching {
  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    limit: Option[Rectangle] = None
  ): Option[Offset] = {
    if (canvas.width < template.width) return None
    if (canvas.height < template.height) return None

    val (xstart: Int, ystart: Int, xend: Int, yend: Int) = limit match {
      case None => (
        0, 0,
        canvas.width - template.width,
        canvas.height - template.height
      )
      case Some(Rectangle(x, y, w, h)) => (
        x, y,
        x + w - template.width,
        y + h - template.height
      )
    }

    if (xend < 0) return None
    if (yend < 0) return None
    if (xend > canvas.width) return None
    if (yend > canvas.height) return None

    find(canvas, template, maxError, xstart, ystart, xend, yend)
  }

var idx = 0

def save(bits: Bits2d, idx: Int, name: String) {
  val buf = new BufferedImage(bits.visibleRect.width, bits.visibleRect.height, BufferedImage.TYPE_INT_BGR)
  val g = buf.createGraphics()
  g.setColor(Color.BLACK)
  for {
    x <- 0 until bits.visibleRect.width
    y <- 0 until bits.visibleRect.height
  } {
    val vx = x + bits.visibleRect.x
    val vy = y + bits.visibleRect.y
    if (bits(vx, vy))
      g.drawLine(x, y, x, y)
  }
  ImageIO.write(buf, "png", new File("/tmp/bits" + name + idx))
}

  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    xstart: Int, ystart: Int, xend: Int, yend: Int
  ): Option[Offset] = {
println("idx = " + idx)
println("xstart = " + xstart)
println("ystart = " + ystart)
println("xend = " + xend)
println("yend = " + yend)
save(canvas, idx, "canvas")
save(template, idx, "template")
idx += 1

    for {
      yoffset <- ystart to yend
      xoffset <- xstart to xend
    } {
      @tailrec def finder(x: Int, y: Int, sumError: Int): Option[Offset] =
        if (sumError > maxError) {
          None
        }
        else {
          val error: Int = if (canvas(x + xoffset, y + yoffset) != template(x, y)) 1 else 0
          val newx = x + 1
          if (newx >= template.width) {
            val newy = y + 1
            if (newy >= template.height) {
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
