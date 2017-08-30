import scala.io.StdIn

object BWMatching {
  case class Mer(head: Array[Char], last: Array[Char], inHead: Array[Int], inLast: Array[Int]) {
    override lazy val toString: String = {
      s"CountingSortResult(\n" +
        s"\thead: $head\n" +
        s"\tlast: $last\n" +
        s"\tinHead: $inHead\n" +
        s"\tinLast: $inLast\n" +
      ")"
    }
  }

  object Alphabet {
    val size: Int = 5

    implicit def index(char: Char): Int = {
      if (char == '$') 0
      else if (char == 'A') 1
      else if (char == 'C') 2
      else if (char == 'G') 3
      else if (char == 'T') 4
      else throw new IllegalArgumentException
    }

    implicit def letter(idx: Int): Char = {
      if (idx == 0) '$'
      else if (idx == 1) 'A'
      else if (idx == 2) 'C'
      else if (idx == 3) 'G'
      else if (idx == 4) 'T'
      else throw new IllegalArgumentException
    }

    val indices: Range = 0 until size

    def apply(idx: Int): Char = letter(idx)
  }

  def countingSort(bwt: Array[Char]): Mer = {
    val org = bwt.clone()

    val counts = Array.fill(Alphabet.size)(0)
    for (i <- org.indices) {
      val alpha = org(i)
      val alphaIndex = Alphabet.index(alpha)
      counts(alphaIndex) = counts(alphaIndex) + 1
    }

    var i = 0
    for (a <- Alphabet.indices) {
      val alpha = Alphabet(a)
      val count = counts(a)
      if (count > 0) {
        for (j <- 0 until count) {
          org(i + j) = alpha
        }
        i = i + count
      }
    }

    val scans = scan(counts)
    val seens = Array.fill(Alphabet.size)(0)

    val inBwt = Array.fill(org.length)(0)
    val inOrg = Array.fill(bwt.length)(0)

    for (i <- bwt.indices) {
      val alpha = bwt(i)
      val a = Alphabet.index(alpha)
      val j = scans(a) + seens(a)
      inOrg(i) = j
      inBwt(j) = i
      seens(a) = seens(a) + 1
    }

    Mer(org, bwt, inOrg, inBwt)
  }

  def scan(counts: Array[Int]): Array[Int] = {
    val scans = counts.clone()
    scans(0) = 0
    for (i <- 1 until scans.length) {
      scans(i) = scans(i - 1) + counts(i - 1)
    }
    scans
  }

  case class BWP(starts: Array[Int], occurs: Array[Array[Int]])

  def preprocess(bwt: Array[Char]): BWP = {
    val mer = countingSort(bwt)
    val head = mer.head
    val starts = Array.fill(Alphabet.size)(-1)
    for (i <- head.indices) {
      val alpha = head(i)
      val idx = Alphabet.index(alpha)
      if (starts(idx) == -1) {
        starts(idx) = i
      }
    }

    val occurs = new Array[Array[Int]](Alphabet.size)
    for (i <- starts.indices) {
      if (starts(i) != -1) {
        occurs(i) = Array.fill(bwt.length + 1)(0)
      } else {
        occurs(i) = null
      }
    }

    for (i <- 1 to bwt.length) {
      val current = bwt(i - 1)
      for (j <- occurs.indices) {
        if (occurs(j) != null) {
          val alpha = Alphabet.letter(j)
          val step = if (alpha == current) 1 else 0
          occurs(j)(i) = occurs(j)(i - 1) + step
        }
      }
    }

    BWP(starts, occurs)
  }

  def numOccurences(bwt: Array[Char], starts: Array[Int], occurs: Array[Array[Int]])(pattern: Array[Char]): Int = {
    var top = 0
    var bottom = bwt.length - 1
    var pLen = pattern.length
    while (top <= bottom) {
      if (pLen > 0) {
        pLen = pLen - 1
        val char = pattern(pLen)
        val charIdx = Alphabet.index(char)
        if (occurs(charIdx) == null) {
          return 0
        }

        val topOccur = occurs(charIdx)(top)
        val bottomOccur = occurs(charIdx)(bottom + 1)
        val start = starts(charIdx)

        if (bottomOccur > topOccur) {
          top = start + topOccur
          bottom = start + bottomOccur - 1
        } else {
          return 0
        }
      } else {
        return bottom - top + 1
      }
    }
    0
  }

  def main(args: Array[String]): Unit = {
    val bwt = StdIn.readLine().toCharArray
    val nPatterns = StdIn.readLine().toInt
    val patterns = StdIn.readLine().split(" ")
    val bwp = preprocess(bwt)
    val numOccurF = numOccurences(bwt, bwp.starts, bwp.occurs)_
    val patOccurs = patterns.map(p => numOccurF(p.toCharArray))
    println(patOccurs.mkString(" "))
  }
}
