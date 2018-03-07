package com.ruimo.graphics.twodim

import scala.collection.{immutable => imm, mutable => mut}
import scala.annotation.tailrec

sealed trait DetectedLineWithDots {
  val sortedDots: List[(Int, Int)]
  val dots: imm.Set[(Int, Int)]
  val errorAllowance: Int
}

case class HorizontalLineWithDots(dots: imm.Set[(Int, Int)], errorAllowance: Int) extends DetectedLineWithDots {
  lazy val left: (Int, Int) = sortedDots.head
  lazy val right: (Int, Int) = sortedDots.last
  lazy val length = right._1 - left._1
  lazy val y = (left._2 + right._2) / 2

  lazy val sortedDots: List[(Int, Int)] = dots.toList.sortBy(_._1)

  def split: imm.Seq[HorizontalLineWithDots] = {
    @tailrec def splitter(
      remaining: List[(Int, Int)], current: imm.Set[(Int, Int)], result: List[HorizontalLineWithDots], lastX: Int = -1
    ): List[HorizontalLineWithDots] = remaining match {
      case Nil => if (current.isEmpty) result else HorizontalLineWithDots(current, errorAllowance) :: result
      case head::tail =>
        if (lastX == -1) {
          splitter(tail, current + head, result, head._1)
        }
        else {
          if (head._1 - lastX <= errorAllowance) splitter(tail, current + head, result, head._1)
          else splitter(tail, imm.HashSet(head), HorizontalLineWithDots(current, errorAllowance) :: result, head._1)
        }
    }

    splitter(sortedDots, imm.HashSet(), List())
  }
}

case class VerticalLineWithDots(dots: imm.Set[(Int, Int)], errorAllowance: Int) extends DetectedLineWithDots {
  lazy val top: (Int, Int) = sortedDots.head
  lazy val bottom: (Int, Int) = sortedDots.last
  lazy val length = bottom._2 - top._2
  lazy val x = (top._1 + bottom._1) / 2

  lazy val sortedDots: List[(Int, Int)] = dots.toList.sortBy(_._2)

  def split: imm.Seq[VerticalLineWithDots] = {
    @tailrec def splitter(
      remaining: List[(Int, Int)], current: imm.Set[(Int, Int)], result: List[VerticalLineWithDots], lastY: Int = -1
    ): List[VerticalLineWithDots] = remaining match {
      case Nil => if (current.isEmpty) result else VerticalLineWithDots(current, errorAllowance) :: result
      case head::tail =>
        if (lastY == -1) {
          splitter(tail, current + head, result, head._2)
        }
        else {
          if (head._2 - lastY <= errorAllowance) splitter(tail, current + head, result, head._2)
          else splitter(tail, imm.HashSet(head), VerticalLineWithDots(current, errorAllowance) :: result, head._2)
        }
    }

    splitter(sortedDots.toList, imm.TreeSet(), List())
  }
}
