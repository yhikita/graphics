package com.ruimo.graphics.twodim

import scala.collection.{immutable => imm, mutable => mut}
import java.awt.image.BufferedImage

import com.ruimo.graphics.twodim.Hugh.FoundLineWithDots
import Degree.toRadian
import com.ruimo.scoins.Percent

import scala.math.Pi
import scala.math.{abs, max, min, pow, sqrt}
import scala.annotation.tailrec
import scala.util.control.TailCalls._

// Detect rectangles. The rectangles should be constracted with horizontal and vertical lines.
// This is so much fast than DetectRectangle. However, this only detects exact vertical and horizontal lines.
object DetectRectangle2 {
  case class VLine(p0: Point, p1: Point) {
    val x: Int = (p0.x + p1.x) / 2
  }
  case class HLine(p0: Point, p1: Point) {
    val y: Int = (p0.y + p1.y) / 2
  }

  def findLargest(
    image: BufferedImage, errorAllowance: Int, lengthLimit: Percent = Percent(50),
    isDot: Int => Boolean = Hugh.brightnessIsDot(200),
    slantAllowance: Int = 2
  ): Option[Rectangle] = {
    val width = image.getWidth
    val height = image.getHeight
    val vLineLengthLimit = lengthLimit.of(height)
    val hLineLengthLimit = lengthLimit.of(width)

    def findVerticaLine(p0: Point, p1: Point): imm.Seq[VLine] = {
      def calcX(y: Int): Int = p0.x + (p1.x - p0.x) * (y - p0.y) / (p1.y - p0.y)
      def getVLine(p0: Point, p1: Point): Option[VLine] =
        if (p1.y - p0.y >= vLineLengthLimit) Some(VLine(p0, p1)) else None

      def init(y: Int, result: imm.Seq[VLine]): TailRec[imm.Seq[VLine]] =
        if (y > p1.y) done(result)
        else {
          val x = calcX(y)
          if (isDot(image.getRGB(x, y))) {
            tailcall(onDot(y + 1, Point(x, y), result))
          } else {
            tailcall(init(y + 1, result))
          }
        }

      def onDot(y: Int, start: Point, result: imm.Seq[VLine]): TailRec[imm.Seq[VLine]] =
        if (y > p1.y) done(result ++ getVLine(start, p1))
        else {
          if (isDot(image.getRGB(calcX(y), y))) {
            tailcall(onDot(y + 1, start, result))
          } else {
            tailcall(dotMissing(y + 1, start, y - 1, result))
          }
        }

      def dotMissing(
        y: Int, start: Point, lastDotY: Int, result: imm.Seq[VLine]
      ): TailRec[imm.Seq[VLine]] =
        if (y > p1.y)
          done(result ++ getVLine(start, Point(calcX(lastDotY), lastDotY)))
        else {
          if (isDot(image.getRGB(calcX(y), y))) {
            tailcall(onDot(y + 1, start, result))
          } else {
            if ((y - lastDotY) >= errorAllowance) {
              tailcall(
                init(y + 1, result ++ getVLine(start, Point(calcX(lastDotY), lastDotY)))
              )
            } else {
              tailcall(dotMissing(y + 1, start, lastDotY, result))
            }
          }
        }

      init(0, imm.Seq()).result
    }

    def findHorizontalLine(p0: Point, p1: Point): imm.Seq[HLine] = {
      def calcY(x: Int): Int = p0.y + (p1.y - p0.y) * (x - p0.x) / (p1.x - p0.x)
      def getHLine(p0: Point, p1: Point): Option[HLine] =
        if (p1.x - p0.x >= hLineLengthLimit) Some(HLine(p0, p1)) else None

      def init(x: Int, result: imm.Seq[HLine]): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) done(result)
        else {
          val y = calcY(x)
          if (isDot(image.getRGB(x, y))) {
            tailcall(onDot(x + 1, Point(x, y), result))
          } else {
            tailcall(init(x + 1, result))
          }
        }

      def onDot(x: Int, start: Point, result: imm.Seq[HLine]): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) done(result ++ getHLine(start, p1))
        else {
          if (isDot(image.getRGB(x, calcY(x)))) {
            tailcall(onDot(x + 1, start, result))
          } else {
            tailcall(dotMissing(x + 1, start, x - 1, result))
          }
        }

      def dotMissing(
        x: Int, start: Point, lastDotX: Int, result: imm.Seq[HLine]
      ): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) {
          done(result ++ getHLine(start, Point(lastDotX, calcY(lastDotX))))
        }
        else {
          if (isDot(image.getRGB(x, calcY(x)))) {
            tailcall(onDot(x + 1, start, result))
          } else {
            if ((x - lastDotX) >= errorAllowance) {
              tailcall(init(x + 1, result ++ getHLine(start, Point(lastDotX, calcY(lastDotX)))))
            } else {
              tailcall(dotMissing(x + 1, start, lastDotX, result))
            }
          }
        }

      init(0, imm.Seq()).result
    }

    val vlines: imm.Set[VLine] =
      (0 until width).foldLeft(imm.Set[VLine]()) { (sum, x) =>
        sum ++ (
          (1 to slantAllowance).foreach { s =>
            if (x >= s) findVerticaLine(Point(x, 0), Point(x - s, height - 1)) else imm.Seq()
          }
        ) ++ (
          findVerticaLine(Point(x, 0), Point(x, height - 1))
        ) ++ (
          (1 to slantAllowance).foreach { s =>
            if (x < width - s) findVerticaLine(Point(x, 0), Point(x + s, height - 1)) else imm.Seq()
          }
        )
      }

    val hlines: imm.Set[HLine] =
      (0 until height).foldLeft(imm.HashSet[HLine]()) { (sum, y) =>
        sum ++ (
          (1 to slantAllowance).foreach { s =>
            if (y >= s) findHorizontalLine(Point(0, y), Point(width - 1, y - s)) else imm.Seq()
          }
        ) ++ (
          findHorizontalLine(Point(0, y), Point(width - 1, y))
        ) ++ (
          (1 to slantAllowance).foreach { s =>
            if (y < height - s) findHorizontalLine(Point(0, y), Point(width - 1, y + s)) else imm.Seq()
          }
        )
      }

    def findRectangle(vlines: imm.Set[VLine], hlines: imm.Set[HLine])(onFound: Rectangle => Unit) {
      def distance(p0: Point, p1: Point): Double = sqrt(pow(p1.x - p0.x, 2) + pow(p1.y - p0.y, 2))

      def rectangle(vl0: VLine, hl0: HLine, vl1: VLine, hl1: HLine): Rectangle = {
        val x0 = min(vl0.x, vl1.x)
        val x1 = max(vl0.x, vl1.x)
        val y0 = min(hl0.y, hl1.y)
        val y1 = max(hl0.y, hl1.y)

        Rectangle(x0, y0, x1 - x0, y1 - y0)
      }

      def init(vls: imm.Set[VLine], hls: imm.Set[HLine]) {
        vls.foreach { vl =>
          findTopLeft(vls, hls, vl)
        }
      }

      def findTopLeft(
        vls: imm.Set[VLine], hls: imm.Set[HLine], line0: VLine
      ) {
        hls.foreach { hl =>
          if (distance(hl.p0, line0.p0) <= errorAllowance) {
            findTopRight(vls, hls, line0, hl)
          }
        }
      }

      def findTopRight(
        vls: imm.Set[VLine], hls: imm.Set[HLine], line0: VLine, line1: HLine
      ) {
        vls.foreach { vl =>
          if (
            vl.x != line0.x &&
            distance(vl.p0, line1.p1) <= errorAllowance
          ) {
            findBottomRight(vls, hls, line0, line1, vl)
          }
        }
      }

      def findBottomRight(
        vls: imm.Set[VLine], hls: imm.Set[HLine], line0: VLine, line1: HLine, line2: VLine
      ) {
        hls.foreach { hl =>
          if (
            hl.y != line1.y
              && distance(hl.p1, line2.p1) <= errorAllowance
              && distance(hl.p0, line0.p1) <= errorAllowance
          ) {
            onFound(rectangle(line0, line1, line2, hl))
          }
        }
      }

      init(vlines, hlines)
    }

    var currentRect: Option[Rectangle] = None

    findRectangle(vlines, hlines) { foundRect =>
      currentRect match {
        case None => currentRect = Some(foundRect)
        case Some(cr) =>
          if (cr.area < foundRect.area) {
            currentRect = Some(foundRect)
          }
      }
    }

    currentRect
  }
}
