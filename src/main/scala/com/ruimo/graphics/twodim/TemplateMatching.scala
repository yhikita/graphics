package com.ruimo.graphics.twodim

import scala.annotation.tailrec

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

  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    xstart: Int, ystart: Int, xend: Int, yend: Int
  ): Option[Offset] = {
    for {
      yoffset <- ystart to yend
      xoffset <- xstart to xend
    } {
      @tailrec def finder(x: Int, y: Int, sumError: Int): Option[Offset] =
        if (sumError > maxError) None
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
