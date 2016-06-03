package com.ruimo.graphics.twodim

import scala.annotation.tailrec

object TemplateMatching {
  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    threshold: Int = 127, limit: Option[Rectangle] = None
  ): Option[Offset] = {
    if (canvas.width < template.width) return None
    if (canvas.height < template.height) return None

    val (xstart: Int, ystart: Int, xend: Int, yend: Int) = limit match {
      case None => (
        0, 0,
        canvas.width - template.width, // exclusive
        canvas.height - template.height // exclusive
      )
      case Some(Rectangle(x, y, w, h)) => (
        x, y,
        x + w - template.width, // exclusive
        y + h - template.height // exclusive
      )
    }

    if (xend <= 0) return None
    if (yend <= 0) return None
    if (xend - xstart > template.width) return None
    if (yend - ystart > template.height) return None
    if (xend > canvas.width) return None
    if (yend > canvas.height) return None

    find(canvas, template, maxError, xstart, ystart, xend, yend)
  }

  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    xstart: Int, ystart: Int, xend: Int, yend: Int
  ): Option[Offset] = {
    @tailrec def finder(x: Int, y: Int, sumError: Int): Option[Offset] =
      if (sumError > maxError) None
      else {
        val error: Int = if (canvas(x, y) != template(x, y)) 1 else 0
        val newx = x + 1
        if (newx >= x + template.width) {
          val newy = y + 1
          if (newy >= y + template.height) {
            Some(Offset(x, y))
          }
          else {
            finder(0, newy, error + sumError)
          }
        }
        else {
          finder(newx, y, error + sumError)
        }
      }

    for {
      x <- xstart until xend
      y <- ystart until yend
    } {
      finder(x, y, 0) match {
        case s: Some[Offset] => return s
        case None => {}
      }
    }

    None
  }
}
