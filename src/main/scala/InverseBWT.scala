import scala.io.StdIn

object InverseBWT {
  case class Mer(head: Array[Char], last: Array[Char], inHead: Array[Int], inLast: Array[Int])

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

  def apply(bwt: String): String = {
    reconstruct(countingSort(bwt.toCharArray))
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

  private def reconstruct(mer: Mer): String = {
    val chars = mer.head.clone()

    var i = 0
    var j = chars.length - 1

    while (j >= 0) {
      val letter = mer.head(i)
      chars(j) = letter
      j = j - 1
      i = mer.inHead(i)
    }

    chars.mkString("")
  }

  def main(args: Array[String]): Unit = {
    val original = InverseBWT(StdIn.readLine())
    println(original)
  }
}
