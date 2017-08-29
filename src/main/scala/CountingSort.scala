import scala.io.StdIn

object CountingSort {
  def sort(bwt: Array[Char]): Unit = {
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

    println(inBwt.mkString(""))
    println(inOrg.mkString(""))
  }

  def scan(counts: Array[Int]): Array[Int] = {
    val scans = counts.clone()
    scans(0) = 0
    for (i <- 1 until scans.length) {
      scans(i) = scans(i - 1) + counts(i - 1)
    }
    scans
  }

  def main(args: Array[String]): Unit = {
    val in = StdIn.readLine()
    val out = in.toCharArray
    sort(out)
    println(out.mkString(""))
  }
}
