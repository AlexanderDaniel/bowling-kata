package lachdrache.bowling.parser

import org.specs2.mutable.Specification

/**
 * Using specs2 auto-examples (see http://etorreborre.github.com/specs2/guide/org.specs2.guide.Structure.html)
 */
class BoardParserSpec extends Specification {

  "game with 1 frame without bonus rolls" should {
    {
      BoardParser("23", 1) === Board(List(Frame(2, 3)))
    }.eg

    {
      BoardParser("-9", 1) === Board(List(Frame(0, 9)))
    }.eg
  }

  "game with 2 frames without bonus rolls" should {
    {
      BoardParser("2754", 2) === Board(List(Frame(2, 7), Frame(5, 4)))
    }.eg

    {
      BoardParser("3/21", 2) === Board(List(Frame(3, 7), Frame(2,1)))
    }.eg

    {
      BoardParser("X44", 2) === Board(List(Frame(10), Frame(4, 4)))
    }.eg
  }

  "game with 1 frame with bonus rolls" should {
    {
      BoardParser("8/7", 1) === Board(List(Frame(8,2)), List(7))
    }.eg

    {
      BoardParser("X12", 1) === Board(List(Frame(10)), List(1,2))
    }.eg

    {
      BoardParser("1/X", 1) === Board(List(Frame(1,9)), List(10))
    }.eg

    {
      BoardParser("XXX", 1) === Board(List(Frame(10)), List(10,10))
    }.eg

    {
      BoardParser("X3/", 1) === Board(List(Frame(10)), List(3,7))
    }.eg

    {
      BoardParser("8/X", 1) === Board(List(Frame(8,2)), List(10))
    }.eg
  }

  "game with 2 frames with bonus rolls" should {
    {
      BoardParser("123/5", 2) === Board(List(Frame(1,2),Frame(3,7)), List(5))
    }.eg

    {
      BoardParser("1/2/5", 2) === Board(List(Frame(1,9),Frame(2,8)), List(5))
    }.eg

    {
      BoardParser("XXXX", 2) === Board(List(Frame(10),Frame(10)), List(10,10))
    }.eg
  }

  "game with 10 frames with bonus rolls" should {
    {
      BoardParser("XXXXXXXXXXXX", 10) === Board(List.fill(10)(Frame(10)), List(10,10))
    }.eg

    {
      BoardParser("9-9-9-9-9-9-9-9-9-9-", 10) === Board(List.fill(10)(Frame(9,0)))
    }.eg

    {
      BoardParser("5/5/5/5/5/5/5/5/5/5/5", 10) === Board(List.fill(10)(Frame(5,5)), List(5))
    }.eg
  }

}
