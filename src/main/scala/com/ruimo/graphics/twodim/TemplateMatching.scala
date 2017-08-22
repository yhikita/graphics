package com.ruimo.graphics.twodim

import scala.annotation.tailrec
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import java.nio.file.Paths

object TemplateMatching {
  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    limit: Option[Rectangle] = None
  ): Option[Offset] = {
    if (System.getProperty("DEBUG_TEMPLATE_MATCHING") != null) {
      println("canvas.width = " + canvas.width)
      println("canvas.height = " + canvas.height)
      println("template.width = " + template.width)
      println("template.height = " + template.height)
      println("limit = " + limit)
    }

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

    if (System.getProperty("DEBUG_TEMPLATE_MATCHING") != null) {
      println("xend = " + xend)
      println("yend = " + yend)
    }

    if (xend < 0) return None
    if (yend < 0) return None
    if (xend > canvas.width) return None
    if (yend > canvas.height) return None

    find(canvas, template, maxError, xstart, ystart, xend, yend)
  }

  private def save(bits: Bits2d, tstamp: Long, name: String) {
    bits.save(Paths.get("/tmp/bits" + name + tstamp))
  }

  def find(
    canvas: Bits2d, template: Bits2d, maxError: Int,
    xstart: Int, ystart: Int, xend: Int, yend: Int
  ): Option[Offset] = {
    if (System.getProperty("DEBUG_TEMPLATE_MATCHING") != null) {
      val tstamp = System.currentTimeMillis
      println("tstamp = " + tstamp)
      println("xstart = " + xstart)
      println("ystart = " + ystart)
      println("xend = " + xend)
      println("yend = " + yend)
      println("canvas.width = " + canvas.width)
      println("canvas.height = " + canvas.height)
      println("canvas.visibleRect = " + canvas.visibleRect)
      println("template.width = " + template.width)
      println("template.height = " + template.height)
      println("template.visibleRect = " + template.visibleRect)
      save(canvas, tstamp, "canvas")
      save(template, tstamp, "template")
    }

    canvas.find(template, maxError, xstart, ystart, xend, yend)
  }
}
