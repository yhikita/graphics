package com.ruimo.graphics.twodim

case class Offset(
  x: Int, y: Int
)

object Offset {
  def apply(seq: Seq[Integer]): Offset = Offset(seq(0), seq(1))
}
