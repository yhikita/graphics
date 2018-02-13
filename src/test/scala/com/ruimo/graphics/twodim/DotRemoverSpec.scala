package com.ruimo.graphics.twodim

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Paths
import javax.imageio.ImageIO

import org.specs2.mutable.Specification

class DotRemoverSpec extends Specification {
  "dot remover" should {
    "Can remove top left corner dot" in {
      // □: white dot
      // ■: black dot
      //
      // ■□□□
      // □□□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 0, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove top dot" in {
      // □: white dot
      // ■: black dot
      //
      // □■□□
      // □□□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 0, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove top right dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□■
      // □□□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(3, 0, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove left dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // ■□□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 1, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove center dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □■□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 1, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove right dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□■
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(3, 1, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove left bottom dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // ■□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 2, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove bottom dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // □■□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 2, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove right bottom dot" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // □□□■
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(3, 2, 0)

      DotRemover.removeDots(1, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove top left corner dots" in {
      // □: white dot
      // ■: black dot
      //
      // ■■□□
      // □■□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 0, 0)
      img0.setRGB(1, 0, 0)
      img0.setRGB(1, 1, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(0, 0) === 0
      img0.getRGB(1, 0) === 0
      img0.getRGB(1, 1) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove top dots" in {
      // □: white dot
      // ■: black dot
      //
      // □■■□
      // □■□□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 0, 0)
      img0.setRGB(2, 0, 0)
      img0.setRGB(1, 1, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(1, 0) === 0
      img0.getRGB(2, 0) === 0
      img0.getRGB(1, 1) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove top right dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□■■
      // □□■□
      // □□□□
      val img0 = new BufferedImage(4, 3, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 2)
          img0.setRGB(x, y, -1)

      img0.setRGB(2, 0, 0)
      img0.setRGB(3, 0, 0)
      img0.setRGB(2, 1, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(2, 0) === 0
      img0.getRGB(3, 0) === 0
      img0.getRGB(2, 1) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 2)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove left dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // ■■□□
      // ■□□□
      // □□□□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 1, 0)
      img0.setRGB(1, 1, 0)
      img0.setRGB(0, 2, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(0, 1) === 0
      img0.getRGB(1, 1) === 0
      img0.getRGB(0, 2) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove center dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □■■□
      // □■□□
      // □□□□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 1, 0)
      img0.setRGB(2, 1, 0)
      img0.setRGB(1, 2, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(1, 1) === 0
      img0.getRGB(2, 1) === 0
      img0.getRGB(1, 2) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove right dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□■■
      // □□■□
      // □□□□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(2, 1, 0)
      img0.setRGB(3, 1, 0)
      img0.setRGB(2, 2, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(2, 1) === 0
      img0.getRGB(3, 1) === 0
      img0.getRGB(2, 2) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove left bottom dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // ■■□□
      // ■□□□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(0, 2, 0)
      img0.setRGB(1, 2, 0)
      img0.setRGB(0, 3, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(0, 2) === 0
      img0.getRGB(1, 2) === 0
      img0.getRGB(0, 3) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove bottom dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // □■■□
      // □■□□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 2, 0)
      img0.setRGB(2, 2, 0)
      img0.setRGB(1, 3, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(1, 2) === 0
      img0.getRGB(2, 2) === 0
      img0.getRGB(1, 3) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove right bottom dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□
      // □□□□
      // □□■■
      // □□■□
      val img0 = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 3)
        for (y <- 0 to 3)
          img0.setRGB(x, y, -1)

      img0.setRGB(2, 2, 0)
      img0.setRGB(3, 2, 0)
      img0.setRGB(2, 3, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      img0.getRGB(2, 2) === 0
      img0.getRGB(3, 2) === 0
      img0.getRGB(2, 3) === 0

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 3)
        for (y <- 0 to 3)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Can remove couple of dots" in {
      // □: white dot
      // ■: black dot
      //
      // □□□□□
      // □■□□□
      // □□□□□
      // □□■■□
      // □□■□□
      val img0 = new BufferedImage(5, 5, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 4)
        for (y <- 0 to 4)
          img0.setRGB(x, y, -1)

      img0.setRGB(1, 1, 0)
      img0.setRGB(2, 3, 0)
      img0.setRGB(3, 3, 0)
      img0.setRGB(2, 4, 0)

      DotRemover.removeDots(1, 250, img0) // should not remove dots sice size = 1.

      (img0.getRGB(1, 1) & 0xffffff) === 0xffffff
      img0.getRGB(2, 3) === 0
      img0.getRGB(3, 3) === 0
      img0.getRGB(2, 4) === 0


      img0.setRGB(1, 1, 0)

      DotRemover.removeDots(2, 250, img0)

      for (x <- 0 to 4)
        for (y <- 0 to 4)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }

    "Bug 0" in {
      // □: white dot
      // ■: black dot
      //
      // 　０１２３４５６７８９０１２３
      // ０□□□□□□□□□□□■□□
      // １□□□□□□□□□□■■■□
      // ２□□□□□□□□□■■■■■
      // ３■■□□□□□□□■■■■■
      // ４■□□□□□□□□□□■□□
      val img0 = new BufferedImage(14, 5, BufferedImage.TYPE_INT_ARGB)
      for (x <- 0 to 13)
        for (y <- 0 to 4)
          img0.setRGB(x, y, -1)

      img0.setRGB(11, 0, 0)
      img0.setRGB(10, 1, 0)
      img0.setRGB(11, 1, 0)
      img0.setRGB(12, 1, 0)
      img0.setRGB(9, 2, 0)
      img0.setRGB(10, 2, 0)
      img0.setRGB(11, 2, 0)
      img0.setRGB(12, 2, 0)
      img0.setRGB(13, 2, 0)
      img0.setRGB(0, 3, 0)
      img0.setRGB(1, 3, 0)
      img0.setRGB(9, 3, 0)
      img0.setRGB(10, 3, 0)
      img0.setRGB(11, 3, 0)
      img0.setRGB(12, 3, 0)
      img0.setRGB(13, 3, 0)
      img0.setRGB(0, 4, 0)
      img0.setRGB(11, 4, 0)

      DotRemover.removeDots(5, 250, img0) // should not remove dots sice size = 1.

      for (x <- 0 to 13)
        for (y <- 0 to 4)
          (img0.getRGB(x, y) & 0xffffff) === 0xffffff

      1 === 1
    }
  }
}
