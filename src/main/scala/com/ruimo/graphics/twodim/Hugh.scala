package com.ruimo.graphics.twodim

import Math.{PI => Pi}
import Math.sqrt
import java.nio.file.Path
import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.immutable

case class Point(x: Int, y: Int)

object Hugh {
  case class FoundLine(ro: Double, th: Double, count: Int)

  // Brightness 0-255
  def rgbToBrightness(rgb: Int): Int = rgbToBrightness(new Color(rgb))

  // Brightness 0-255
  def rgbToBrightness(color: Color): Int = {
    val r = color.getRed
    val g = color.getGreen
    val b = color.getBlue
    (r + r + r + b + g + g + g + g) / 8 // Y = 0.375 R + 0.5 G + 0.125 B
  }

  def defaultIsDot(rgb: Int): Boolean = rgbToBrightness(rgb) <= 127

  private def sinCosTable(resolution: Int): (Array[Double], Array[Double]) = {
    val sin = new Array[Double](resolution)
    val cos = new Array[Double](resolution)
    for (idx <- 0 until resolution) {
      val th: Double = Pi * idx / resolution
      sin(idx) = Math.sin(th)
      cos(idx) = Math.cos(th)
    }
    (sin, cos)
  }

  private def rotate90(radian: Double): Double = radian - Pi / 2

  private def sort(
    vote: Array[Array[Int]], roQuantizer: Quantizer, thQuantizer: Quantizer
  ): immutable.IndexedSeq[FoundLine] = {
    val size = vote.size * vote(0).size
    val buf = new Array[FoundLine](size)
    var idx = 0
    for {
      roIdx <- 0 until vote.size
      thIdx <- 0 until vote(0).size
    } {
      buf(idx) = FoundLine(
        roQuantizer.fromIndex(roIdx),
        rotate90(thQuantizer.fromIndex(thIdx)),
        vote(roIdx)(thIdx)
      )
      idx += 1
    }

    buf.filter(_.count > 0).sortBy(- _.count).toIndexedSeq
  }

  def perform(
    path: Path,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot
  ): immutable.IndexedSeq[FoundLine] = {
    val img: BufferedImage = ImageIO.read(path.toFile)
    val width: Int = img.getWidth
    val height: Int = img.getHeight
    def findDots(f: Point => Unit) {
      val line = new Array[Int](width)
      @tailrec def scan(x: Int, y: Int) {
        if (y >= height) return
        if (x == 0) img.getRGB(0, y, width, 1, line, 0, width)
        if (isDot(line(x))) f(Point(x, y))
        val nextX = x + 1
        if (nextX >= line.length) scan(0, y + 1)
        else scan(nextX, y)
      }

      scan(0, 0)
    }

    val diagonal = sqrt(width * width + height * height)
    val roQuantizer: Quantizer = new Quantizer(min = -diagonal, max = diagonal, resolution = roResolution)
    val thQuantizer: Quantizer = new Quantizer(max = Pi, resolution = thResolution)
    val (sin: Array[Double], cos: Array[Double]) = sinCosTable(thResolution)
    val vote: Array[Array[Int]] = Array.ofDim[Int](roResolution, thResolution)

    findDots { p =>
      for (thIdx <- 0 until thResolution) {
        val ro: Double = p.x * cos(thIdx) + p.y * sin(thIdx)
        vote(roQuantizer.toIndex(ro))(thIdx) += 1
      }
    }
    sort(vote, roQuantizer, thQuantizer)
  }
}
