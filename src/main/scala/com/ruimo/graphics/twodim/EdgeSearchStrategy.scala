package com.ruimo.graphics.twodim

import scala.collection.immutable

trait EdgeSearchStrategy {
  def find(line: Seq[Rgb]): (Boolean, EdgeSearchStrategy)
}

case class BlackEdgeSearchStrategy(blackBrightnessThreshold: Double) extends EdgeSearchStrategy {
  def find(line: Seq[Rgb]): (Boolean, EdgeSearchStrategy) = {
    val black = (line.foldLeft(0L)(_ + _.brightness))
    (black / line.size < blackBrightnessThreshold, this)
  }
}
