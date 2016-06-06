package com.ruimo.graphics.twodim

import java.awt.image.{BufferedImage, ImageObserver}

object ImageScaler {
  def scale(img: BufferedImage, factor: Double): BufferedImage = {
    val ret = new BufferedImage(
      (img.getWidth * factor + 0.5).toInt,
      (img.getHeight * factor + 0.5).toInt,
      img.getType
    )

    val completed = ret.createGraphics.drawImage(
      img,
      0, 0,
      ret.getWidth, ret.getHeight,
      0, 0,
      img.getWidth, img.getHeight,
      null
    )
    // BufferedImage are rendered synchronously.
    if (! completed) throw new Error("Logic error.")
    ret
  }
}