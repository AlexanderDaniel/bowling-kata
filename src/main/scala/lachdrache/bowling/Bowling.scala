package lachdrache.bowling

/**
 * Bowling kata
 *
 * Description at http://www.codingdojo.org/cgi-bin/wiki.pl?KataBowling
 *
 * I implemented this solution after doing the kata at the Vienna Scala User Group http://meetup.scala-vienna.org/events/105141312/
 *
 * Normally a bowling game consists of 10 frames. But {{{scoreOf}}} can compute the result for a bowling game of any
 * number of frames which eases unit testing.
 */
object Bowling {

  def scoreOf(frameCnt: Int)(scoreSheet: String): Int = frameCnt match {
    case 0 => 0
    case _ => {
      val (scoreOfRolls, rest) = consumeFrame(scoreSheet)
      val scoreOfBonusRolls = bonusRolls(rest, noOfBonusRolls(scoreOfRolls))
      val scoreOfFrame = scoreOfRolls.sum + scoreOfBonusRolls.sum
      scoreOfFrame + scoreOf(frameCnt-1)(rest)
    }
  }

  def noOfBonusRolls(scoreOfRolls: List[Int]): Int = scoreOfRolls match {
    case 10 :: Nil => 2
    case n1 :: n2 :: Nil if n1+n2==10 => 1
    case _ => 0
  }

  def charToScore(c :Char): Int = c match {
    case '-' => 0
    case 'X' => 10
    case d if '0'<=d && d<='9' => d-'0'
    case _ => throw new IllegalArgumentException
  }

  /** @return the bonus rolls as ints */
  def bonusRolls(board: String, count: Int): List[Int] = count match {
    case 0 => List()
    case 1 => List(charToScore(board.head))
    case 2 => consumeFrame(board) match {
      case (List(10), rest) => {
        val secondBonus = bonusRolls(rest, 1)
        10 :: secondBonus
      }
      case (frame, _) => frame
    }
    case _ => throw new IllegalArgumentException
  }

  /** @return the rolls as ints and the rest of the board */
  def consumeFrame(board: String):(List[Int], String) = board.toList match {
    case 'X' :: rest => (List(10), rest.mkString)
    case c1 :: '/' :: rest => {
      val score1 = charToScore(c1)
      (List(score1, 10-score1), rest.mkString)
    }
    case c1 :: c2 :: rest => (List(charToScore(c1), charToScore(c2)), rest.mkString)
    case _ => throw new IllegalArgumentException
  }
}
