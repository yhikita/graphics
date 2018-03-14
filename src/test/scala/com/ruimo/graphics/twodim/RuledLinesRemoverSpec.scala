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

  val white = new Color(255, 255, 255).getRGB()

  "ruled line remove" should {
    "Can remove lines" in {
      val correctDelta = 2
      val bi = ImageIO.read(Paths.get("testdata/ruledLineRemover/test0001.png").toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 1, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0001_out1.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // vertical 7 lines
      for (x <- List(1 to 8, 169 to 174, 649 to 655, 1087 to 1093, 1573 to 1579, 2046 to 2052, 2538 to 2539).flatten)
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      // horizontal 5 lines
      for (y <- List(92 to 98, 124 to 129, 154 to 159, 188 to 193, 1911 to 1918).flatten)
        for (x <- correctDelta until width - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      1 === 1
    }
    "Can remove lines except charactors" in {
      val correctDelta = 2
      val bi = ImageIO.read(Paths.get("testdata/ruledLineRemover/test0001.png").toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 1, dotRatio = 40,
        correctOverlapping = true, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0001_out2.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // vertical 7 lines
      var buf = scala.collection.mutable.ListBuffer.empty[Int]
      for (x <- List(1 to 8, 169 to 174, 649 to 655, 1087 to 1093, 1573 to 1579, 2046 to 2052, 2538 to 2539).flatten)
        for (y <- correctDelta until height - correctDelta)
          buf += ruledLineRemovedImage.getRGB(x, y)

      buf.filter(_ != white).size !== 0

      // horizontal 5 lines
      buf = scala.collection.mutable.ListBuffer.empty[Int]
      for (y <- List(92 to 98, 124 to 129, 154 to 159, 188 to 193, 1911 to 1918).flatten)
        for (x <- correctDelta until width - correctDelta)
          buf += ruledLineRemovedImage.getRGB(x, y)

      buf.filter(_ != white).size !== 0
    }
  }
}
