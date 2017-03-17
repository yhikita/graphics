package com.ruimo.graphics.twodim

import scala.util.Try
import java.awt.image.BufferedImage

import scala.collection.mutable
import scala.collection.immutable
import scala.annotation.tailrec

import org.slf4j.Logger
import org.slf4j.LoggerFactory

object Crop {
  val logger = LoggerFactory.getLogger(getClass)

  def extractArea(image: BufferedImage, x: Int, y: Int, width: Int, height: Int): Seq[Rgb] = {
    val buf = new Array[Int](width * height)
    image.getRGB(x, y, width, height, buf, 0, width)
    buf.map(rgba => Rgb(rgba)).toSeq
  }

  def simpleCrop(rect: Rectangle, image: BufferedImage): BufferedImage =
    image.getSubimage(rect.x, rect.y, rect.width, rect.height)

  def edgeCrop(
    leftEdgeSearchArea: Area, topEdgeSearchArea: Area,
    rightEdgeSearchArea: Area, bottomEdgeSearchArea: Area,
    image: BufferedImage, initialStrategy: EdgeSearchStrategy
  ): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val leftArea = leftEdgeSearchArea.toPixelArea(width, height)
    logger.info("edgeCrop. left: " + leftArea)
    @tailrec def findLeft(x: Int, endX: Int, strategy: EdgeSearchStrategy): Int =
      if (x >= endX) throw new IllegalStateException("Cannot find left edge.")
      else {
        val (found: Boolean, nextStrategy) = strategy.find(extractArea(image, x, leftArea.y, 1, leftArea.height))
        if (found) x else findLeft(x + 1, endX, nextStrategy)
      }
    val topArea = topEdgeSearchArea.toPixelArea(width, height)
    logger.info("edgeCrop. top: " + topArea)
    @tailrec def findTop(y: Int, endY: Int, strategy: EdgeSearchStrategy): Int =
      if (y >= endY) throw new IllegalStateException("Cannot find top edge.")
      else {
        val (found: Boolean, nextStrategy) = strategy.find(extractArea(image, topArea.x, y, topArea.width, 1))
        if (found) y else findTop(y + 1, endY, nextStrategy)
      }
    val rightArea = rightEdgeSearchArea.toPixelArea(width, height)
    logger.info("edgeCrop. right: " + rightArea)
    @tailrec def findRight(x: Int, endX: Int, strategy: EdgeSearchStrategy): Int =
      if (x <= endX) throw new IllegalStateException("Cannot find right edge.")
      else {
        val (found: Boolean, nextStrategy) = strategy.find(extractArea(image, x, rightArea.y, 1, rightArea.height))
        if (found) x else findRight(x - 1, endX, nextStrategy)
      }
    val bottomArea = bottomEdgeSearchArea.toPixelArea(width, height)
    logger.info("edgeCrop. bottom: " + bottomArea)
    @tailrec def findBottom(y: Int, endY: Int, strategy: EdgeSearchStrategy): Int =
      if (y <= endY) throw new IllegalStateException("Cannot find bottom edge.")
      else {
        val (found: Boolean, nextStrategy) = strategy.find(extractArea(image, bottomArea.x, y, bottomArea.width, 1))
        if (found) y else findBottom(y - 1, endY, nextStrategy)
      }

    val leftEdge: Int = findLeft(leftArea.x, leftArea.x + leftArea.width, initialStrategy)
    val topEdge: Int = findTop(topArea.y, topArea.y + topArea.height, initialStrategy)
    val rightEdge: Int = findRight(rightArea.x + rightArea.width - 1, rightArea.x - 1, initialStrategy)
    val bottomEdge: Int = findBottom(bottomArea.y + bottomArea.height - 1, bottomArea.y - 1, initialStrategy)
    image.getSubimage(leftEdge, topEdge, rightEdge - leftEdge, bottomEdge - topEdge)
  }
}
