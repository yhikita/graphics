package com.ruimo.graphics.twodim

import scala.collection.{immutable => imm, mutable => mut}
import java.awt.image.BufferedImage

import com.ruimo.graphics.twodim.Hugh.FoundLineWithDots
import Degree.toRadian
import com.ruimo.scoins.Percent

import scala.math.Pi
import scala.math.{abs, max, min, pow, sqrt}
import scala.annotation.tailrec

import org.slf4j.Logger
import org.slf4j.LoggerFactory

// Detect rectangles. The rectangles should be constracted with horizontal and vertical lines.
object DetectRectangle {
  val logger = LoggerFactory.getLogger(getClass)

  // maxAngleToDetect, roResolution, thetaResolution are used for hugh conversion.
  // By Hugh conversion, horizontal and vertical lines are found. Take top lineCount lines.
  // Detect largest rectangle from these taken lines.
  // When looking for lines, errorAllowance is used to allow non consecutive dots in a line.
  def findLargest(
    image: BufferedImage,
    maxAngleToDetect: Double = 1.0, roResolution: Int = 3000, thetaResolution: Int = 200,
    lineCount: Int = 100, errorAllowance: Int = 10, lengthLimit: Percent = Percent(50),
    isDot: Int => Boolean = Hugh.brightnessIsDot(200)
  ): Option[Rectangle] = {
    val rangeVertical = imm.Seq(
      Range(min = toRadian(-maxAngleToDetect), max = toRadian(maxAngleToDetect)),
      Range(min = toRadian(180 - maxAngleToDetect), max = Pi)
    )

    val rangeHorizontal = imm.Seq(
      Range(min = toRadian(90 - maxAngleToDetect / 2), max = toRadian(90 + maxAngleToDetect / 2))
    )

    val foundVertical: imm.IndexedSeq[FoundLineWithDots] = Hugh.performImageWithDots(
      image, roResolution, thetaResolution, thRange = rangeVertical, isDot = isDot
    )

    val foundHorizontal: imm.IndexedSeq[FoundLineWithDots] = Hugh.performImageWithDots(
      image, roResolution, thetaResolution, thRange = rangeHorizontal, isDot = isDot
    )
    def distinct[T <: DetectedLineWithDots](lines: imm.Seq[T]): imm.Seq[T] = {
      val set = new mut.HashSet[T]() ++= lines

      def includes(l0: DetectedLineWithDots, l1: DetectedLineWithDots): Boolean = {
        l0 match {
          case HorizontalLineWithDots(seq0, _) => l1 match {
            case HorizontalLineWithDots(seq1, _) => seq1.find((e1: (Int, Int)) => ! seq0(e1)) == None
            case _ => false
          }
          case VerticalLineWithDots(seq0, _) => l1 match {
            case VerticalLineWithDots(seq1, _) => seq1.find((e1: (Int, Int)) => ! seq0(e1)) == None
            case _ => false
          }
        }
      }

      var ret = imm.Seq[T]()
      set.toSeq.sortBy(e => -e.dots.size).foreach { e =>
        ret.find(rete => includes(rete, e)) match {
          case Some(_) =>
          case None =>
            ret = ret :+ e
        }
      }
      ret
    }

    def splitNonConsecutiveVLine(lines: imm.Seq[VerticalLineWithDots]): imm.Seq[VerticalLineWithDots] = lines.flatMap(_.split)
    def splitNonConsecutiveHLine(lines: imm.Seq[HorizontalLineWithDots]): imm.Seq[HorizontalLineWithDots] = lines.flatMap(_.split)

    val vLineLengthLimit = lengthLimit.of(image.getHeight)
    val hLineLengthLimit = lengthLimit.of(image.getWidth)
    val resultVertical: imm.Seq[VerticalLineWithDots] = distinct(foundVertical.take(lineCount).map(l => VerticalLineWithDots(l.dots, errorAllowance))).filter { l =>
      l.length >= vLineLengthLimit
    }
    val resultHorizontal: imm.Seq[HorizontalLineWithDots] = distinct(foundHorizontal.take(lineCount).map(l => HorizontalLineWithDots(l.dots, errorAllowance))).filter { l =>
      l.length >= hLineLengthLimit
    }

    val splitVertical: imm.Seq[VerticalLineWithDots] = splitNonConsecutiveVLine(resultVertical).filter { l =>
      l.length >= vLineLengthLimit
    }
    val splitHorizontal: imm.Seq[HorizontalLineWithDots] = splitNonConsecutiveHLine(resultHorizontal).filter { l =>
      l.length >= hLineLengthLimit
    }
    logger.info("splitHorizontal: size = " + splitHorizontal.size)

    def findRectangle(splitVertical: imm.Seq[VerticalLineWithDots], splitHorizontal: imm.Seq[HorizontalLineWithDots]): imm.Seq[Rectangle] = {
      def distance(p0: (Int, Int), p1: (Int, Int)): Double = sqrt(
        pow(p1._1 - p0._1, 2) + pow(p1._2 - p0._2, 2)
      )

      def rectangle(vl0: VerticalLineWithDots, hl0: HorizontalLineWithDots, vl1: VerticalLineWithDots, hl1: HorizontalLineWithDots): Rectangle = {
        val x0 = min(vl0.x, vl1.x)
        val x1 = max(vl0.x, vl1.x)
        val y0 = min(hl0.y, hl1.y)
        val y1 = max(hl0.y, hl1.y)

        Rectangle(x0, y0, x1 - x0, y1 - y0)
      }

      var found: List[Rectangle] = List()

      def init(vls: imm.Seq[VerticalLineWithDots], hls: imm.Seq[HorizontalLineWithDots]) {
        vls.foreach { vl =>
          findTopLeft(vls, hls, vl)
        }
      }

      def findTopLeft(
        vls: imm.Seq[VerticalLineWithDots], hls: imm.Seq[HorizontalLineWithDots], line0: VerticalLineWithDots
      ) {
        hls.foreach { hl =>
          if (distance(hl.left, line0.top) <= errorAllowance) {
            findTopRight(vls, hls, line0, hl)
          }
        }
      }

      def findTopRight(
        vls: imm.Seq[VerticalLineWithDots], hls: imm.Seq[HorizontalLineWithDots],
        line0: VerticalLineWithDots, line1: HorizontalLineWithDots
      ) {
        vls.foreach { vl =>
          if (
            vl.x != line0.x &&
            distance(vl.top, line1.right) <= errorAllowance
          ) {
            findBottomRight(vls, hls, line0, line1, vl)
          }
        }
      }

      def findBottomRight(
        vls: imm.Seq[VerticalLineWithDots], hls: imm.Seq[HorizontalLineWithDots],
        line0: VerticalLineWithDots, line1: HorizontalLineWithDots, line2: VerticalLineWithDots
      ) {
        hls.foreach { hl =>
          if (
            hl.y != line1.y
              && distance(hl.right, line2.bottom) <= errorAllowance
              && distance(hl.left, line0.bottom) <= errorAllowance
          ) {
            found = rectangle(line0, line1, line2, hl)::found
          }
        }
      }

      init(splitVertical, splitHorizontal)
      found
    }

    def findLargest(rects: imm.Seq[Rectangle]): Option[Rectangle] = {
      if (rects.size == 0) None
      else if (rects.size == 1) Some(rects(0))
      else {
        var ret = rects(0)
        rects.tail.foreach { r =>
          if (ret.area < r.area) {
            ret = r
          }
        }
        Some(ret)
      }
    }

    findLargest(findRectangle(splitVertical.toList, splitHorizontal.toList))
  }
}
