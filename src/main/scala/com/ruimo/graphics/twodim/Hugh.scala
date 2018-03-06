package com.ruimo.graphics.twodim

import Math.{PI => Pi}
import Math.{sqrt, pow}
import java.nio.file.Path
import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.collection.{immutable => imm, mutable => mut}

case class Point(x: Int, y: Int)

object Hugh {
  case class FoundLine(ro: Double, th: Double, count: Int)
  case class FoundLineWithDots(ro: Double, th: Double, dots: imm.Set[(Int, Int)]) {
    val count = dots.size
  }

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

  private def sinCosTable(thQuantizer: Quantizer): (Array[Double], Array[Double]) = {
    val sin = new Array[Double](thQuantizer.resolution)
    val cos = new Array[Double](thQuantizer.resolution)
    for (idx <- 0 until thQuantizer.resolution) {
      val th: Double = thQuantizer.fromIndex(idx)
      sin(idx) = Math.sin(th)
      cos(idx) = Math.cos(th)
    }
    (sin, cos)
  }

  private def rotate90(radian: Double): Double = radian - Pi / 2

  private def sort(
    vote: Array[Array[Int]], roQuantizer: Quantizer, thQuantizer: Quantizer
  ): imm.IndexedSeq[FoundLine] = {
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

  private def sortWithDots(
    vote: Array[Array[mut.ArrayBuffer[(Int, Int)]]], roQuantizer: Quantizer, thQuantizer: Quantizer
  ): imm.IndexedSeq[FoundLineWithDots] = {
    val size = vote.size * vote(0).size
    val buf = new Array[FoundLineWithDots](size)
    var idx = 0
    for {
      roIdx <- 0 until vote.size
      thIdx <- 0 until vote(0).size
    } {
      buf(idx) = FoundLineWithDots(
        roQuantizer.fromIndex(roIdx),
        rotate90(thQuantizer.fromIndex(thIdx)),
        vote(roIdx)(thIdx).toSet
      )
      idx += 1
    }

    buf.filter(_.count > 0).sortBy(- _.count).toIndexedSeq
  }

  def performHighPrecision(
    img: BufferedImage,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot,
    thRange: imm.Seq[Range] = imm.Seq(),
    errorAllowance: Int
  ): imm.IndexedSeq[FoundLineWithDots] = performHighPrecisionWithCombine(
    img, roResolution, thResolution, isDot, thRange,
    errorAllowance,
    img.getWidth.toDouble / 1000, img.getWidth.toDouble / 1000 / 100
  )

  def performHighPrecisionWithCombine(
    img: BufferedImage,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot,
    thRange: imm.Seq[Range] = imm.Seq(),
    errorAllowance: Int,
    roCombineLimit: Double, thetaCombineLimit: Double
  ): imm.IndexedSeq[FoundLineWithDots] = {
    def distance(p0: (Int, Int), p1: (Int, Int)): Double = sqrt(
      pow(p0._1 - p1._1, 2) + pow(p0._2 - p1._2, 2)
    )

    val lines = performImageWithDots(img, roResolution, thResolution, isDot, thRange)
    lines.flatMap { l =>
      if (l.dots.size <= 1) List(l)
      else {
        val sortedDots = l.dots.toIndexedSeq.sortBy(p => pow(p._1, 2) + pow(p._2, 2))

        @tailrec def loop(
          from: (Int, Int), idx: Int, result: imm.Seq[imm.Set[(Int, Int)]]
        ): imm.IndexedSeq[FoundLineWithDots] =
          if (idx >= sortedDots.size) {
            result.map(dots => FoundLineWithDots(l.ro, l.th, dots)).toIndexedSeq
          } else {
            if (distance(from, sortedDots(idx)) > errorAllowance) {
              loop(sortedDots(idx), idx + 1, result :+ sortedDots.slice(0, idx).toSet)
            } else {
              loop(from, idx + 1, result)
            }
          }

        loop(sortedDots.head, 1, imm.Seq())
      }
    }
  }

  def perform(
    path: Path,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot,
    thRange: imm.Seq[Range] = imm.Seq()
  ): imm.IndexedSeq[FoundLine] = {
    performImage(
      ImageIO.read(path.toFile),
      roResolution, thResolution,
      isDot, thRange
    )
  }

  def performImage(
    img: BufferedImage,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot,
    thRange: imm.Seq[Range] = imm.Seq()
  ): imm.IndexedSeq[FoundLine] = {
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
    val roQuantizer: Quantizer = new Quantizer(roResolution, Range(-diagonal, diagonal))
    val thQuantizer: Quantizer = new Quantizer(thResolution, (if (thRange.isEmpty) Seq(Range(max = Pi)) else thRange): _*)
    val (sin: Array[Double], cos: Array[Double]) = sinCosTable(thQuantizer)
    val vote: Array[Array[Int]] = Array.ofDim[Int](roResolution, thResolution)

    findDots { p =>
      for (thIdx <- 0 until thResolution) {
        val ro: Double = p.x * cos(thIdx) + p.y * sin(thIdx)
        vote(roQuantizer.toIndex(ro))(thIdx) += 1
      }
    }
    sort(vote, roQuantizer, thQuantizer)
  }

  def performImageWithDots(
    img: BufferedImage,
    roResolution: Int, thResolution: Int,
    isDot: Int => Boolean = defaultIsDot,
    thRange: imm.Seq[Range] = imm.Seq()
  ): imm.IndexedSeq[FoundLineWithDots] = {
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
    val roQuantizer: Quantizer = new Quantizer(roResolution, Range(-diagonal, diagonal))
    val thQuantizer: Quantizer = new Quantizer(thResolution, (if (thRange.isEmpty) Seq(Range(max = Pi)) else thRange): _*)
    val (sin: Array[Double], cos: Array[Double]) = sinCosTable(thQuantizer)
    val vote: Array[Array[mut.ArrayBuffer[(Int, Int)]]] = Array.ofDim[mut.ArrayBuffer[(Int, Int)]](roResolution, thResolution)
    for {
      ro <- 0 until roResolution
      th <- 0 until thResolution
    } vote(ro)(th) = mut.ArrayBuffer()

    findDots { p =>
      for (thIdx <- 0 until thResolution) {
        val ro: Double = p.x * cos(thIdx) + p.y * sin(thIdx)
        vote(roQuantizer.toIndex(ro))(thIdx) += (p.x -> p.y)
      }
    }
    sortWithDots(vote, roQuantizer, thQuantizer)
  }
}
