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
  val black = new Color(0, 0, 0).getRGB()

  "ruled line remove" should {
    "Can remove vertical/horizontal lines" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 1, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out1.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check all lines removed
      for (x <- correctDelta until width - correctDelta)
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      1 === 1
    }

    "Can remove vertical lines" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 10, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out2.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check vertical lines removed
      for (x <-
        List(
          List(0 until 2).flatten // thickness 2
            , List(100 until 104).flatten // thickness 4
            , List(200 until 201).flatten // thickness 1
            , List(300 until 303).flatten // thickness 3
            , List(395 until 400).flatten // thickness 5
        ).flatten )
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      1 === 1
    }

    "Can remove vertical lines that thickness are equal to or more than 2" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 2, deltaY = 10, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out3.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check vertical lines removed if thickness >= 2
      for (x <-
        List(
          List(0 until 2).flatten // thickness 2
            , List(100 until 104).flatten // thickness 4
            , List(300 until 303).flatten // thickness 3
            , List(395 until 400).flatten // thickness 5
        ).flatten )
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      // check vertical lines NOT removed if thickness < 2
      for (x <-
        List(
          List(200 until 201).flatten // thickness 1
        ).flatten )
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === black

      1 === 1
    }

    "Can remove vertical lines that thickness are equal to or more than 3" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 3, deltaY = 10, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out4.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check vertical lines removed if thickness >= 2
      for (x <-
        List(
          List(100 until 104).flatten // thickness 4
            , List(300 until 303).flatten // thickness 3
            , List(395 until 400).flatten // thickness 5
        ).flatten )
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === white

      // check vertical lines NOT removed if thickness < 2
      for (x <-
        List(
          List(0 until 2).flatten // thickness 2
            , List(200 until 201).flatten // thickness 1
        ).flatten )
        for (y <- correctDelta until height - correctDelta)
          ruledLineRemovedImage.getRGB(x, y) === black

      1 === 1
    }

    "Can remove horizontal lines" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 10, deltaY = 1, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out5.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check vertical lines removed
      for (x <- correctDelta until width - correctDelta)
        for (y <-
          List(
            List(0 until 2).flatten // thickness 2
              , List(100 until 104).flatten // thickness 4
              , List(197 until 200).flatten // thickness 3
          ).flatten )
          ruledLineRemovedImage.getRGB(x, y) === white

      1 === 1
    }

    "Can remove horizontal lines that thickness are equal to or more than 3" in {
      val fileName = "testdata/ruledLineRemover/test0002.png"
      // create test data
      if (!Paths.get(fileName).toFile.exists())
        createTestData(fileName)

      val correctDelta = 2
      val bi = ImageIO.read(Paths.get(fileName).toFile)
      val remover = new RuledLinesRemover(deltaX = 10, deltaY = 3, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0002_out6.png"))

      val width = ruledLineRemovedImage.getWidth
      val height = ruledLineRemovedImage.getHeight

      // check horizontal lines removed if thickness >= 3
      for (x <- correctDelta until width - correctDelta)
        for (y <-
          List(
            List(100 until 104).flatten // thickness 4
              , List(197 until 200).flatten // thickness 3
          ).flatten )
          ruledLineRemovedImage.getRGB(x, y) === white

      // check horizontal lines NOT removed if thickness < 3
      for (x <- correctDelta until width - correctDelta)
        for (y <-
          List(
            List(0 until 2).flatten // thickness 2
          ).flatten )
          ruledLineRemovedImage.getRGB(x, y) === black

      1 === 1
    }

    "Can remove lines" in {
      val correctDelta = 2
      val bi = ImageIO.read(Paths.get("testdata/ruledLineRemover/test0001.png").toFile)
      val remover = new RuledLinesRemover(deltaX = 1, deltaY = 1, dotRatio = 40,
        correctOverlapping = false, correctDelta = correctDelta, correctDotRatio = 40)
      val ruledLineRemovedImage = remover.removeRuledLines(bi);
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0001_out1.png"))

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
      //ImageIO.write(ruledLineRemovedImage, "png", new File("testdata/ruledLineRemover/test0001_out2.png"))

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

  private def createTestData(fileName: String): Unit = {
    println("creating test data: " + fileName)
    val width = 400
    val height = 200
    val image: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (x <- 0 until width) {
      for (y <- 0 until height) {
        // vertical lines
        if (List(
          List(0 until 2).flatten // thickness 2
            , List(100 until 104).flatten // thickness 4
            , List(200 until 201).flatten // thickness 1
            , List(300 until 303).flatten // thickness 3
            , List(395 until 400).flatten // thickness 5
        ).flatten.contains(x))
          image.setRGB(x, y, black)
        // horizontal lines
        else if (List(
          List(0 until 2).flatten // thickness 2
            , List(100 until 104).flatten // thickness 4
            , List(197 until 200).flatten // thickness 3
        ).flatten.contains(y))
          image.setRGB(x, y, black)
        /*
        // dots
        else if (x == 50 && y == 50) // 1x1 pixcel
          image.setRGB(x, y, black)
        else if (List(150 until 152).flatten.contains(x) // 2x2 pixcel
          && List(50 until 52).flatten.contains(y))
          image.setRGB(x, y, black)
        else if (List(250 until 253).flatten.contains(x) // 3x3 pixcel
          && List(50 until 53).flatten.contains(y))
          image.setRGB(x, y, black)
        else if (List(350 until 354).flatten.contains(x) // 4x4 pixcel
          && List(50 until 54).flatten.contains(y))
          image.setRGB(x, y, black)
         */
        else
          image.setRGB(x, y, white)
      }
    }
    ImageIO.write(image, "png", new File(fileName))
  }

}
