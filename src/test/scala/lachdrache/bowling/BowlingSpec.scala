package lachdrache.bowling

import org.specs2.mutable.Specification
import Bowling._

class BowlingSpec extends Specification {

  "game with 1 frame without strikes and spares" should {
    "return score 3 for 21" in {
      scoreOf("21") === 3
    }
    "return score 9 for 45" in {
      scoreOf("45") === 9
    }
    "return score 0 for --" in {
      scoreOf("--") === 0
    }
    "return score 7 for -7" in {
      scoreOf("-7") == 7
    }
  }

  "game with 1 frame with spares (one bonus roll)" should {
    "return score 11 for 7/1" in {
      scoreOf("7/1") === 11
    }
    "return score 19 for -/9" in {
      scoreOf("-/9") === 19
    }
    "return score 10 for 9/-" in {
      scoreOf("9/-") === 10
    }
  }

  "game with 1 frame with strike (two bonus rolls)" should {
    "return score 12 for X11" in {
      scoreOf("X11") === 12
    }
    "return score 21 for XX1" in {
      scoreOf("XX1") === 21
    }
    "return score 30 for XXX" in {
      scoreOf("XXX") === 30
    }
    "return score 20 for X3/" in {
      scoreOf("X3/") === 20
    }
  }

  "game with 2 frames" should {
    "return score 14 for 3443" in {
      scoreOf("3443") === 14
    }
    "return score 32=13+19 for 2/3/9" in {
      scoreOf("2/3/9") === 32
    }
    "return score 60=30+30 for XXXX" in {
      scoreOf("XXXX") === 60
    }
    "return score 43=23+20 for XX3/" in {
      scoreOf("XX3/") === 43
    }
  }

  "game with 10 frames (12 rolls: 12 strikes)" should {
    "return max score 300 for XXXXXXXXXXXX" in {
      scoreOf("XXXXXXXXXXXX") === 300
    }
    "return 90 for 9-9-9-9-9-9-9-9-9-9- (20 rolls: 10 pairs of 9 and miss)" in {
      scoreOf("9-9-9-9-9-9-9-9-9-9-") === 90
    }
    "return 150 for 5/5/5/5/5/5/5/5/5/5/5 (21 rolls: 10 pairs of 5 and spare, with a final 5)" in {
      scoreOf("5/5/5/5/5/5/5/5/5/5/5") === 150
    }
  }
}
