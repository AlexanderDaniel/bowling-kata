package lachdrache.bowling.parser

case class Frame(first: Int, second: Option[Int] = None)

object Frame {
  def apply(first: Int, second: Int): Frame = apply(first, Some(second))
}
