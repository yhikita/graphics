package com.ruimo.graphics.twodim

import org.specs2.mutable.Specification
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import javax.imageio.ImageIO
import java.nio.file.Files
import java.nio.file.Path
import Hugh.FoundLine
import scala.collection.immutable
import Math.{PI => Pi}

class HughSpec extends Specification {
  "Hugh" should {
    "Detect horizontal line" in {
      val file: Path = Files.createTempFile(null, ".jpg")
      file.toFile.deleteOnExit
      val bi = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
      val g: Graphics2D = bi.createGraphics()
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, 10, 10)
      g.setColor(Color.BLACK)
      g.drawLine(0, 0, 9, 0)
      ImageIO.write(bi, "JPG", file.toFile)

      val found: immutable.IndexedSeq[FoundLine] = Hugh.perform(file, roResolution = 100, thResolution = 360)
      val most = found.filter(_.count == 10)
      val ave = most.foldLeft(0.0) { (sum, e) => sum + e.th } / most.size

      ave must beCloseTo(0, 0.1)
    }

    "Detect vertical line" in {
      val file: Path = Files.createTempFile(null, ".jpg")
      file.toFile.deleteOnExit
      val bi = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
      val g: Graphics2D = bi.createGraphics()
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, 10, 10)
      g.setColor(Color.BLACK)
      g.drawLine(1, 0, 1, 9)
      ImageIO.write(bi, "JPG", file.toFile)

      val found: immutable.IndexedSeq[FoundLine] = Hugh.perform(file, roResolution = 100, thResolution = 360)
      val most = found.filter(_.count == 10).map(_.th).map(th => if (th < 0) th + Pi else th)
      val ave = most.foldLeft(0.0) { (sum, th) => sum + th } / most.size

      ave must beCloseTo(Pi / 2, 0.1)
    }

    "Detect diagnoal line" in {
      val file: Path = Files.createTempFile(null, ".jpg")
      file.toFile.deleteOnExit
      val bi = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
      val g: Graphics2D = bi.createGraphics()
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, 10, 10)
      g.setColor(Color.BLACK)
      g.drawLine(0, 0, 9, 9)
      ImageIO.write(bi, "JPG", file.toFile)

      val found: immutable.IndexedSeq[FoundLine] = Hugh.perform(file, roResolution = 100, thResolution = 360)
      val most = found.filter(_.count == 10).map(_.th)
      val ave = most.foldLeft(0.0) { (sum, th) => sum + th } / most.size

      ave must beCloseTo(Pi / 4, 0.1)
    }
  }
}

