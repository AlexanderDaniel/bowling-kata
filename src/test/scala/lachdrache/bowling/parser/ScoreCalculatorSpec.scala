package lachdrache.bowling.parser

import org.specs2.mutable.Specification

class ScoreCalculatorSpec extends Specification {

  "one frame without bonus rolls" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 2)))) === 3
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(4, 5)))) === 9
    }.eg
  }

  "one frame with bonus rolls" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 9)), List(7))) === 17
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(10)), List(1, 2))) === 13
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(10)), List(10, 10))) === 30
    }.eg
  }

  "two frames without spares and strikes" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 2), Frame(3, 4)), List())) === (3 + 7)
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(4, 5), Frame(5, 4)), List())) === (9 + 9)
    }.eg
  }

  "two frames with spare or strike as last frame" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 2), Frame(1, 9)), List(7))) === (3 + 17)
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 2), Frame(10)), List(10, 10))) === (3 + 30)
    }.eg
  }

  "two frames with spare as first frame" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 9), Frame(1, 2)), List())) === (11 + 3)
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(1, 9), Frame(1, 9)), List(1))) === (11 + 11)
    }.eg
  }

  "two frames with strike as first frame" should {
    {
      ScoreCalculator(ScoreSheet(List(Frame(10), Frame(1, 2)), List())) === (13 + 3)
    }.eg

    {
      ScoreCalculator(ScoreSheet(List(Frame(10), Frame(10)), List(10, 10))) === (30 + 30)
    }.eg
  }

  "game with 1 frame with spares (one bonus roll)" should {
    {
      ScoreCalculator("21", 1) === 3
    }.eg
  }

  "game with 10 frames" should {
    "return max score 300 for XXXXXXXXXXXX (12 rolls: 12 strikes)" in {
      ScoreCalculator("XXXXXXXXXXXX", 10) === 300
    }
    "return 90 for 9-9-9-9-9-9-9-9-9-9- (20 rolls: 10 pairs of 9 and miss)" in {
      ScoreCalculator("9-9-9-9-9-9-9-9-9-9-", 10) === 90
    }
    "return 150 for 5/5/5/5/5/5/5/5/5/5/5 (21 rolls: 10 pairs of 5 and spare, with a final 5)" in {
      ScoreCalculator("5/5/5/5/5/5/5/5/5/5/5", 10) === 150
    }
  }
}
