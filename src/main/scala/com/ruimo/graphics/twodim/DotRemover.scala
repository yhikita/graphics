package com.ruimo.graphics.twodim

import java.awt.image.BufferedImage

import scala.collection.{mutable => mut}
import scala.annotation.tailrec

/**
Dot remover. Remove rectanglular dots if they are smaller than the specified size.
If the speicifed size is 1, the following dots will not be removed at all.
If the speicifed size is equal to or more than 2, the following dots will be removed entirely.

■: black dot
{{{
  ■■
    ■
}}}

Logic:

From top left to right bottom, scan black rectangles and remove them.
In scanning, search for black dot(s) surrounded by white dots.

■: black dot
□: white dot
{{{
□□□□
□■■□
□□■□
□□□□
}}}

Suppose the specified size is N, check from 1 to N.
For example, If the size is 3, check from 1 to 3.

size = 1: Cannot detect black dots.
{{{
□□□□
□■■□
□□■□
□□□□
}}}

size = 2: Can detect black dots.
{{{
□□□□
□■■□
□□■□
□□□□
}}}

Once the black dots are found, remove them and advance x by size + 1 (=3 in the above case).
If no black dots are found, advance x by 1.
Record the size until one horizontal line are scanned. Then advance y by minimum of size + 1.
If no black dots are found, advance y by 1.

Corner case:

x = 0 and y = 0: The following dots are size 2.
{{{
■■□
□■□
□□□
}}}

x = width - (size - 1) and y = 0: The following dots are size 2.
{{{
□■■
□□■
□□□
}}}

To handle these corner case safely, create 2 dots larger copy (top, left, borrom, right 1 dot larger each) of original image and process the copied image.
In addition, binarize the image thru this copy process for performance.
  */
object DotRemover {
  def removeDots(
    dotSize: Int, blackBrightness: Int, img: BufferedImage,
    scanArea: Option[Rectangle] = None
  ) {
    val area = scanArea.getOrElse(Rectangle(0, 0, img.getWidth(), img.getHeight()))
    val width = area.width + 2
    val height = area.height + 2
    class VirtualScreen {
      val dots: Vector[mut.BitSet] = {
        val buf = new Array[mut.BitSet](height)
        for (i <- 0 until height) {
          buf(i) = new mut.BitSet
        }

        for (y <- 0 until img.getHeight) {
          for (x <- 0 until img.getWidth) {
            if (Rgb(img.getRGB(x, y)).brightness <= blackBrightness) {
              buf(y + 1) += x + 1
            }
          }
        }
        buf.toVector
      }

      def dotsFound(x: Int, y: Int, size: Int): Boolean = {
        for (xx <- x to (x + size + 1)) {
          if (dots(y)(xx)) return false
          if (dots(y + size + 1)(xx)) return false
        }

        for (yy <- y to (y + size + 1)) {
          if (dots(yy)(x)) return false
          if (dots(yy)(x + size + 1)) return false
        }

        true
      }

      def removeDots(x: Int, y: Int, size: Int) {
        for (yy <- y until (y + size)) {
          for (xx <- x until (x + size)) {
            if (dots(yy)(xx)) {
              dots(yy) -= xx
              img.setRGB(xx - 1, yy - 1, 0xffffff)
            }
          }
        }
      }
    }

    val screen = new VirtualScreen

    @tailrec def scan(x: Int, y: Int, currentSearchSize: Int, foundSize: Int = -1): Unit = {
      if (y >= height - 1) return
      if (x >= width - 1) {
        scan(0, y + (if (foundSize == -1) 1 else foundSize), dotSize)
      }
      else if (currentSearchSize < 1) scan(x + 1, y, dotSize, foundSize)
      else {
        if (x + currentSearchSize >= width - 1 || y + currentSearchSize >= height - 1) {
          scan(x, y, currentSearchSize - 1, foundSize)
        }
        else {
          if (screen.dotsFound(x, y, currentSearchSize)) {
            screen.removeDots(x + 1, y + 1, currentSearchSize)
            scan(x + currentSearchSize + 1, y, dotSize, foundSize)
          }
          else {
            scan(x, y, currentSearchSize - 1, foundSize)
          }
        }
      }
    }

    scan(0, 0, dotSize)
  }
}
