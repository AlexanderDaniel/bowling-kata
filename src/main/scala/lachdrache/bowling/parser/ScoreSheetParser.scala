package lachdrache.bowling.parser

import util.parsing.combinator.RegexParsers

/**
 * Parses a [[https://en.wikipedia.org/wiki/File:Bowlstrike.PNG Bowling score sheet]]
 *
 * The score sheet is a string as described at http://www.codingdojo.org/cgi-bin/wiki.pl?KataBowling
 */
class ScoreSheetParser(frameCnt: Int) extends RegexParsers {

  def sheet: Parser[ScoreSheet] = repN(frameCnt, frame)~bonus ^^ {
    case frames~bonus => ScoreSheet(frames, bonus)
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

  def apply(in: String): ScoreSheet = parseAll(sheet, in) match {
    case Success(result, _) => result
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"Parsing error: $msg")
  }
}

object ScoreSheetParser {
  def apply(in: String, frameCnt: Int) = {
    val parser = new ScoreSheetParser(frameCnt)
    parser(in)
  }
}
