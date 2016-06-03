package com.ruimo.graphics.twodim

case class Size(
  width: Int, height: Int
)

object Size {
  def apply(seq: Seq[Integer]): Size = Size(seq(0), seq(1))
}
