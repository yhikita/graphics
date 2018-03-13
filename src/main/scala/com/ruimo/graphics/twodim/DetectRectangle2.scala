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
    isDot: Int => Boolean = Hugh.brightnessIsDot(200), lineThreshold: Percent = Percent(70),
    slantAllowance: Int = 2
  ): Option[Rectangle] = {
    val width = image.getWidth
    val height = image.getHeight
    val vLineLengthLimit = lengthLimit.of(height)
    val hLineLengthLimit = lengthLimit.of(width)

    def findVerticaLine(p0: Point, p1: Point): imm.Seq[VLine] = {
      def calcX(y: Int): Int = p0.x + (p1.x - p0.x) * (y - p0.y) / (p1.y - p0.y)
      def getVLine(start: Point, lastDotY: Int, dotCount: Int): Option[VLine] =
        if (
          dotCount >= lineThreshold.of(lastDotY - start.y + 1) &&
            lastDotY - start.y + 1 >= vLineLengthLimit
        ) Some(VLine(start, Point(calcX(lastDotY), lastDotY))) else None

      def init(y: Int, result: imm.Seq[VLine], dotCount: Int): TailRec[imm.Seq[VLine]] =
        if (y > p1.y) done(result)
        else {
          val x = calcX(y)
          if (isDot(image.getRGB(x, y))) {
            tailcall(onDot(y + 1, Point(x, y), result, dotCount + 1))
          } else {
            tailcall(init(y + 1, result, dotCount))
          }
        }

      def onDot(y: Int, start: Point, result: imm.Seq[VLine], dotCount: Int): TailRec[imm.Seq[VLine]] =
        if (y > p1.y) done(result ++ getVLine(start, y, dotCount))
        else {
          if (isDot(image.getRGB(calcX(y), y))) {
            tailcall(onDot(y + 1, start, result, dotCount + 1))
          } else {
            tailcall(dotMissing(y + 1, start, y - 1, result, dotCount))
          }
        }

      def dotMissing(
        y: Int, start: Point, lastDotY: Int, result: imm.Seq[VLine], dotCount: Int
      ): TailRec[imm.Seq[VLine]] =
        if (y > p1.y)
          done(result ++ getVLine(start, lastDotY, dotCount))
        else {
          if (isDot(image.getRGB(calcX(y), y))) {
            tailcall(onDot(y + 1, start, result, dotCount + 1))
          } else {
            if ((y - lastDotY) >= errorAllowance) {
              tailcall(
                init(y + 1, result ++ getVLine(start, lastDotY, dotCount), 0)
              )
            } else {
              tailcall(dotMissing(y + 1, start, lastDotY, result, dotCount))
            }
          }
        }

      init(0, imm.Seq(), 0).result
    }

    def findHorizontalLine(p0: Point, p1: Point): imm.Seq[HLine] = {
      def calcY(x: Int): Int = p0.y + (p1.y - p0.y) * (x - p0.x) / (p1.x - p0.x)
      def getHLine(start: Point, lastDotX: Int, dotCount: Int): Option[HLine] =
        if (
          dotCount >= lineThreshold.of(lastDotX - start.x + 1) &&
            lastDotX - start.x + 1 >= hLineLengthLimit
        ) Some(HLine(start, Point(lastDotX, calcY(lastDotX)))) else None

      def init(x: Int, result: imm.Seq[HLine], dotCount: Int): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) done(result)
        else {
          val y = calcY(x)
          if (isDot(image.getRGB(x, y))) {
            tailcall(onDot(x + 1, Point(x, y), result, dotCount + 1))
          } else {
            tailcall(init(x + 1, result, dotCount))
          }
        }

      def onDot(x: Int, start: Point, result: imm.Seq[HLine], dotCount: Int): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) done(result ++ getHLine(start, x, dotCount))
        else {
          if (isDot(image.getRGB(x, calcY(x)))) {
            tailcall(onDot(x + 1, start, result, dotCount + 1))
          } else {
            tailcall(dotMissing(x + 1, start, x - 1, result, dotCount))
          }
        }
 
      def dotMissing(
        x: Int, start: Point, lastDotX: Int, result: imm.Seq[HLine], dotCount: Int
      ): TailRec[imm.Seq[HLine]] =
        if (x > p1.x) {
          done(result ++ getHLine(start, lastDotX, dotCount))
        }
        else {
          if (isDot(image.getRGB(x, calcY(x)))) {
            tailcall(onDot(x + 1, start, result, dotCount + 1))
          } else {
            if ((x - lastDotX) >= errorAllowance) {
              tailcall(init(x + 1, result ++ getHLine(start, lastDotX, dotCount), 0))
            } else {
              tailcall(dotMissing(x + 1, start, lastDotX, result, dotCount))
            }
          }
        }

      init(0, imm.Seq(), 0).result
    }

    val vlines: imm.Set[VLine] =
      (0 until width).foldLeft(imm.Set[VLine]()) { (sum, x) =>
        sum ++ (
          (1 to slantAllowance).foldLeft(imm.Set[VLine]()) { (sum, s) =>
            if (x >= s) sum ++ findVerticaLine(Point(x, 0), Point(x - s, height - 1)) else sum
          }
        ) ++ (
          findVerticaLine(Point(x, 0), Point(x, height - 1))
        ) ++ (
          (1 to slantAllowance).foldLeft(imm.Set[VLine]()) { (sum, s) =>
            if (x < width - s) sum ++ findVerticaLine(Point(x, 0), Point(x + s, height - 1)) else sum
          }
        )
      }

    val hlines: imm.Set[HLine] =
      (0 until height).foldLeft(imm.HashSet[HLine]()) { (sum, y) =>
        sum ++ (
          (1 to slantAllowance).foldLeft(imm.HashSet[HLine]()) { (sum, s) =>
            if (y >= s) sum ++ findHorizontalLine(Point(0, y), Point(width - 1, y - s)) else sum
          }
        ) ++ (
          findHorizontalLine(Point(0, y), Point(width - 1, y))
        ) ++ (
          (1 to slantAllowance).foldLeft(imm.HashSet[HLine]()) { (sym, s) =>
            if (y < height - s) sum ++ findHorizontalLine(Point(0, y), Point(width - 1, y + s)) else sum
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
