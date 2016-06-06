package com.ruimo.graphics.twodim

import java.awt.image.BufferedImage
import java.nio.file.Paths
import javax.imageio.ImageIO

import org.specs2.mutable.Specification

class Bits2dSpec extends Specification {
  "Bits 2d" should {
    "Convert bits" in {
      // □■
      // □□
      val img0 = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
      img0.setRGB(0, 0, 0)
      img0.setRGB(1, 0, -1)
      img0.setRGB(0, 1, 0)
      img0.setRGB(1, 1, 0)

      val bits = Bits2d(img0)
      bits(0, 0) === false
      bits(1, 0) === true
      bits(0, 1) === false
      bits(1, 1) === false
    }

    "Convert bits with offset" in {
      // □■□
      // □■□
      // □□□
      val img0 = new BufferedImage(3, 3, BufferedImage.TYPE_INT_ARGB)
      img0.setRGB(0, 0, 0)
      img0.setRGB(1, 0, -1)
      img0.setRGB(2, 0, 0)
      img0.setRGB(0, 1, 0)
      img0.setRGB(1, 1, -1)
      img0.setRGB(2, 1, 0)
      img0.setRGB(0, 2, 0)
      img0.setRGB(1, 2, 0)
      img0.setRGB(2, 2, 0)

      val bits = Bits2d(img0)
      bits(0, 0) === false
      bits(1, 0) === true
      bits(2, 0) === false
      bits(0, 1) === false
      bits(1, 1) === true
      bits(2, 1) === false
      bits(0, 2) === false
      bits(1, 2) === false
      bits(2, 2) === false

      val bitsWithOffset = Bits2d(img0, area = Some(Rectangle(1, 1, 2, 2)))
      bitsWithOffset(1, 1) === true
      bitsWithOffset(2, 1) === false
      bitsWithOffset(1, 2) === false
      bitsWithOffset(2, 2) === false
    }
  }
}
