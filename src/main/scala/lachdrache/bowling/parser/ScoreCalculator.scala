package lachdrache.bowling.parser

object ScoreCalculator {

  def apply(sheet: ScoreSheet): Int = totalScore(sheet.frames, sheet.bonus)

  def apply(sheet: String, frameCnt: Int): Int = {
    apply(ScoreSheetParser(sheet, frameCnt))
  }

  def totalScore(frames: List[Frame], bonus: List[Int]): Int = frames match {
    case Nil => 0
    case _ :: rest => scoreOfHeadFrame(frames, bonus) + totalScore(rest, bonus)
  }

  def scoreOfHeadFrame(frames: List[Frame], bonus: List[Int]): Int = {
    val firstFrame = frames.head
    firstFrame.first + firstFrame.second.getOrElse(0) + bonusScore(numberOfBonusRolls(firstFrame), frames.tail, bonus)
  }

  def bonusScore(rolls: Int, frames: List[Frame], bonus: List[Int]): Int = frames match {
    case Nil => bonus.sum
    case _ => bonusRolls(rolls, frames, bonus).sum
  }

  def numberOfBonusRolls(frame: Frame): Int = frame match {
    case Frame(10, None) => 2
    case Frame(f, Some(s)) if f+s==10 => 1
    case _ => 0
  }

  def bonusRolls(rolls: Int, frames: List[Frame], bonus: List[Int]): List[Int] = (rolls, frames) match {
    case (0, _) => Nil
    case (1, Nil) => bonus.head :: Nil
    case (1, _) => frames.head.first :: Nil
    case (2, Nil) => bonus
    case (2, _) if frames.head.second.isDefined => frames.head.first :: frames.head.second.get :: Nil
    case (2, _) => frames.head.first :: bonusRolls(1, frames.tail, bonus)
    case _ => throw new IllegalArgumentException
  }
}
