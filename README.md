Bowling Kata
============

[Problem Description](http://www.codingdojo.org/cgi-bin/wiki.pl?KataBowling)

[Bowling](src/main/scala/lachdrache/bowling/Bowling.scala) calculates the total score directly on the input string
and does not have a separate parser step (to convert into Scala data structures).

The parser combinator [ScoreSheetParser](src/main/scala/lachdrache/bowling/parser/ScoreSheetParser.scala) returns a
case class ScoreSheet with a list of frames and a bonus list.

[ScoreCalculator](src/main/scala/lachdrache/bowling/parser/ScoreCalculator.scala) uses the ScoreSheet data structure to
calculate the total score.

[specs2](http://etorreborre.github.com/specs2/) unit tests are available for all classes.
