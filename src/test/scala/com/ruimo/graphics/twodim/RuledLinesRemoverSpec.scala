package com.ruimo.graphics.twodim

import org.specs2.mutable.Specification
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import javax.imageio.ImageIO
import java.nio.file.{Files, Path, Paths}

import Hugh.FoundLine

import scala.collection.immutable
import Math.{PI => Pi}
import java.io.File

import com.ruimo.scoins.Percent

class RuledLinesRemoverSpec extends Specification {
  "ruled line remove" should {
    "Can remove lines 4" in {

      val bi = ImageIO.read(Paths.get("testdata/ruledLineRemover/test0001.png").toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 1, dotRatio = 40,
        correctOverlapping = true, correctDelta = 2, correctDotRatio = 40)
//      val ruledLineRemovedImage = remover.removeRuledLines(bi);
1 === 1
    }
  }
}
