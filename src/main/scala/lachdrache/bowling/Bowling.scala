package lachdrache.bowling

/**
 * Bowling kata
 *
 * Description at http://www.codingdojo.org/cgi-bin/wiki.pl?KataBowling
 *
 * I implemented this solution after doing the kata at the Vienna Scala User Group http://meetup.scala-vienna.org/events/105141312/
 *
 * Normally a bowling game consists of 10 frames. But {{{scoreOf}}} can compute the result for a bowling game of any
 * number of frames. It sees if there is no more input available and stops. This also eases unit testing because one
 * can test for a game with 1 or 2 frames.
 */
object Bowling {

  def scoreOf(board: String): Int = {
    val (scoreOfRolls, rest) = consumeFrame(board)
    val (scoreOfBonusRolls, restOfBonusRolls) = consumeRolls(rest, bonusRolls(scoreOfRolls))
    val scoreOfFrame = scoreOfRolls.sum + scoreOfBonusRolls.sum
    restOfBonusRolls match {
      case "" => scoreOfFrame
      case _ => scoreOfFrame + scoreOf(rest)
    }
  }

  def bonusRolls(scoreOfRolls: List[Int]): Int = scoreOfRolls match {
    case 10 :: Nil => 2
    case n1 :: n2 :: Nil if n1+n2==10 => 1
    case _ => 0
  }

  def charToScore(c :Char): Int = c match {
    case '-' => 0
    case 'X' => 10
    case c1 if '0'<=c1 && c1<='9' => c1-'0'
    case _ => throw new IllegalArgumentException
  }

  /** @return the bonus rolls as ints and the rest of the board */
  def consumeRolls(board: String, count: Int): (List[Int], String) = count match {
    case 0 => (List(), board)
    case 1 => (List(charToScore(board.head)), board.tail)
    case 2 => consumeFrame(board) match {
      case (List(10), rest) => {
        val (secondBonus, restOfRest) = consumeRolls(rest, 1)
        (10 :: secondBonus, restOfRest)
      }
      case tuple => tuple
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
