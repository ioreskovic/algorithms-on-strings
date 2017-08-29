import scala.io.StdIn
import scala.util.Sorting

case class BWT(in: String, out: String) {
}

object BWT {
  def apply(s: String): BWT = {
    BWT(s, transform(s))
  }

  private def transform(s: String): String = {
    val chars = s.toCharArray
    val len = chars.length
    val matrix = (0 until len).map(i => rotated(chars, i)).toArray
    Sorting.quickSort(matrix)(charArrayOrdering)
    matrix.map(_.last).mkString("")
  }

  private def rotated[T](arr: Array[T], k: Int) = {
    val r = arr.clone()
    rotate(r, k)
    r
  }

  private def rotate[T](arr: Array[T], k: Int): Unit = rotate(arr, 0, arr.length - 1, k)

  private def rotate[T](arr: Array[T], from: Int, to: Int, k: Int): Unit = {
    reverse(arr, from, from + k)
    reverse(arr, from + k + 1, to)
    reverse(arr, from, to)
  }

  private def reverse[T](arr: Array[T], from: Int, to: Int): Unit = {
    val hLen = (to - from + 1) / 2
    for (i <- 0 until hLen) {
      swap(arr, from + i, to - i)
    }
  }

  private def swap[T](arr: Array[T], i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  private val charArrayOrdering = new Ordering[Array[Char]] {
    override def compare(x: Array[Char], y: Array[Char]): Int = {
      var i = 0
      while (i < x.length && i < y.length) {
        if (x(i) < y(i)) return -1
        else if (x(i) > y(i)) return 1
        else i = i + 1
      }
      0
    }
  }

  def main(args: Array[String]): Unit = {
    val bwt = BWT(StdIn.readLine())
    println(bwt.out)
  }
}
