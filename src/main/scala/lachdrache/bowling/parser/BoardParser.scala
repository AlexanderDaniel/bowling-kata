package lachdrache.bowling.parser

import util.parsing.combinator.RegexParsers

class BoardParser(frameCnt: Int) extends RegexParsers {

  def board: Parser[Board] = repN(frameCnt, frame)~bonus ^^ {
    case frames~bonus => Board(frames, bonus)
  }

  def frame: Parser[Frame] = strike | spare | incomplete

  def strike: Parser[Frame] = "X" ^^ {
    case "X" => Frame(10)
  }
  def spare: Parser[Frame] = c<~"/" ^^ {
    case c1 => Frame(c1, 10-c1)
  }
  def incomplete: Parser[Frame] = c~c ^^ {
    case c1 ~ c2 => Frame(c1, c2)
  }

  def bonus: Parser[List[Int]] = spareAsList | rep(c)
  def spareAsList: Parser[List[Int]] = spare ^^ {
    case frame => List(frame.first, frame.second.get)
  }

  def c: Parser[Int] = digit | dash | x
  def digit: Parser[Int] = """\d""".r ^^ {
    case s if s.length == 1 => s.head - '0'
    case _ => throw new IllegalArgumentException
  }
  def dash: Parser[Int] = "-" ^^^ {
    0
  }
  def x: Parser[Int] = "X" ^^^ {
    10
  }

  def apply(in: String): Board = parseAll(board, in) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"Parsing error: $msg")
  }
}

object BoardParser {
  def apply(in: String, frameCnt: Int) = {
    val parser = new BoardParser(frameCnt)
    parser(in)
  }
}
