package com.ruimo.graphics.twodim

import java.awt.Color
import java.awt.image.BufferedImage

class RuledLinesRemover(deltaX: Int = 1, deltaY: Int = 1, dotRatio: Int = 40,
  correctOverlapping: Boolean = true, correctDelta: Int = 2, correctDotRatio: Int = 40,
  isDebug: Boolean = false) {

  val BLACK: Int = new Color(0, 0, 0).getRGB()
  val RED: Int = new Color(255, 0, 0).getRGB()
  val GREEN: Int = new Color(0, 255, 0).getRGB()
  val BLUE: Int = new Color(0, 0, 255).getRGB()
  val WHITE: Int = new Color(255, 255, 255).getRGB()

  def removeRuledLines(inputImage: BufferedImage): BufferedImage = {
    val width = inputImage.getWidth()
    val height = inputImage.getHeight()

    // color without alpha
    val outputImage: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val rgb = inputImage.getRGB(x, y)
        outputImage.setRGB(x, y, rgb)
      }
    }

    // remove ruled lines
    // 1) vertical ruled lines
    removeVerticalRuledLines(inputImage, outputImage, correctOverlapping)
    // 2) horizontal ruled lines
    removeHorizontalRuledLines(inputImage, outputImage, correctOverlapping)

    return outputImage
  }

  private def removeVerticalRuledLines(inputImage: BufferedImage, outputImage: BufferedImage, correctOverlapping: Boolean) = {
    val width = inputImage.getWidth()
    val height = inputImage.getHeight()
    for (lineX <- deltaX until width - deltaX) {
      var count = 0
      for (x <- lineX - deltaX until lineX + deltaX) {
        for (y <- 0 until height) {
          if (inputImage.getRGB(x, y) != WHITE) {
            count += 1
          }
        }
      }
      if (count >= (dotRatio/100.0f) * deltaX*2 * height) {
        for (x <- lineX - deltaX until lineX + deltaX) {
          for (y <- correctDelta until height - correctDelta) {
            val color = getCorrectionColor()
            if (correctOverlapping) {
              if (isVerticalRuledLine(x, y, outputImage)) {
                outputImage.setRGB(x, y, color)
              }
            } else {
              outputImage.setRGB(x, y, color)
            }
          }
        }
      }
    }
  }

  private def removeHorizontalRuledLines(inputImage: BufferedImage, outputImage: BufferedImage, correctOverlapping: Boolean) = {
    val width = inputImage.getWidth()
    val height = inputImage.getHeight()
    for (lineY <- deltaY until height - deltaY) {
      var count = 0
      for (y <- lineY - deltaY until lineY + deltaY) {
        for (x <- 0 until width) {
          if (inputImage.getRGB(x, y) != WHITE) {
            count += 1
          }
        }
      }
      if (count >= (dotRatio/100.0f) * deltaY*2 * width) {
        for (y <- lineY - deltaY until lineY + deltaY) {
          for (x <- correctDelta until width - correctDelta) {
            val color = getCorrectionColor()
            if (correctOverlapping) {
              if (isHorizontalRuledLine(x, y, outputImage)) {
                outputImage.setRGB(x, y, color)
              }
            } else {
              outputImage.setRGB(x, y, color)
            }
          }
        }
      }
    }
  }

  private def isVerticalRuledLine(x: Int, y: Int, image: BufferedImage): Boolean = {
    isRuledLine(x, y, image, true)
  }

  private def isHorizontalRuledLine(x: Int, y: Int, image: BufferedImage): Boolean = {
    isRuledLine(y, x, image, false)
  }

  private def isRuledLine(a: Int, b: Int, image: BufferedImage, isVertical: Boolean): Boolean = {
    var count = 0
    for (i <- a - correctDelta until a + correctDelta) {
      for (j <- b - correctDelta until b) {
        if (isVertical && image.getRGB(i, j) == BLACK ) {
          count += 1
        } else if (!isVertical && image.getRGB(j, i) == BLACK ) {
          count += 1
        }
      }
    }
    if (count > 2 * correctDelta * correctDelta * (correctDotRatio/100.0f)) {
      return false
    }
    return true
  }

  private def getCorrectionColor(): Int = {
    if (isDebug) {
      return GREEN
    } else {
      return WHITE
    }
  }

}
