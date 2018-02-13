package com.ruimo.graphics.twodim

import com.ruimo.scoins.Percent

case class Area(
  x: Percent, y: Percent, w: Percent, h: Percent
) {
  def toPixelArea(imageWidth: Int, imageHeight: Int): Rectangle = Rectangle(
    (imageWidth * x / 100).asInstanceOf[Int],
    (imageHeight * y / 100).asInstanceOf[Int],
    (imageWidth * w / 100).asInstanceOf[Int],
    (imageHeight * h / 100).asInstanceOf[Int]
  )
}

object Area {
  def apply(parm: Seq[java.lang.Double]): Area = Area(parm(0), parm(1), parm(2), parm(3))
}

