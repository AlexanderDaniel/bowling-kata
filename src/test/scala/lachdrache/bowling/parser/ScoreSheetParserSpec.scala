package lachdrache.bowling.parser

import org.specs2.mutable.Specification

/**
 * Using specs2 auto-examples (see http://etorreborre.github.com/specs2/guide/org.specs2.guide.Structure.html)
 */
class ScoreSheetParserSpec extends Specification {

  "game with 1 frame without bonus rolls" should {
    {
      ScoreSheetParser("23", 1) === ScoreSheet(List(Frame(2, 3)))
    }.eg

    {
      ScoreSheetParser("-9", 1) === ScoreSheet(List(Frame(0, 9)))
    }.eg
  }

  "game with 2 frames without bonus rolls" should {
    {
      ScoreSheetParser("2754", 2) === ScoreSheet(List(Frame(2, 7), Frame(5, 4)))
    }.eg

    {
      ScoreSheetParser("3/21", 2) === ScoreSheet(List(Frame(3, 7), Frame(2,1)))
    }.eg

    {
      ScoreSheetParser("X44", 2) === ScoreSheet(List(Frame(10), Frame(4, 4)))
    }.eg
  }

  "game with 1 frame with bonus rolls" should {
    {
      ScoreSheetParser("8/7", 1) === ScoreSheet(List(Frame(8,2)), List(7))
    }.eg

    {
      ScoreSheetParser("X12", 1) === ScoreSheet(List(Frame(10)), List(1,2))
    }.eg

    {
      ScoreSheetParser("1/X", 1) === ScoreSheet(List(Frame(1,9)), List(10))
    }.eg

    {
      ScoreSheetParser("XXX", 1) === ScoreSheet(List(Frame(10)), List(10,10))
    }.eg

    {
      ScoreSheetParser("X3/", 1) === ScoreSheet(List(Frame(10)), List(3,7))
    }.eg

    {
      ScoreSheetParser("8/X", 1) === ScoreSheet(List(Frame(8,2)), List(10))
    }.eg
  }

  "game with 2 frames with bonus rolls" should {
    {
      ScoreSheetParser("123/5", 2) === ScoreSheet(List(Frame(1,2),Frame(3,7)), List(5))
    }.eg

    {
      ScoreSheetParser("1/2/5", 2) === ScoreSheet(List(Frame(1,9),Frame(2,8)), List(5))
    }.eg

    {
      ScoreSheetParser("XXXX", 2) === ScoreSheet(List(Frame(10),Frame(10)), List(10,10))
    }.eg
  }

  "game with 10 frames with bonus rolls" should {
    {
      ScoreSheetParser("XXXXXXXXXXXX", 10) === ScoreSheet(List.fill(10)(Frame(10)), List(10,10))
    }.eg

    {
      ScoreSheetParser("9-9-9-9-9-9-9-9-9-9-", 10) === ScoreSheet(List.fill(10)(Frame(9,0)))
    }.eg

    {
      ScoreSheetParser("5/5/5/5/5/5/5/5/5/5/5", 10) === ScoreSheet(List.fill(10)(Frame(5,5)), List(5))
    }.eg
  }

}
