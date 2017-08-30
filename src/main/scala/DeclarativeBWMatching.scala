import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.StdIn

case class CountingSortResult[T](sorted: Vector[T], original: Vector[T], inSorted: Vector[Int], inOriginal: Vector[Int]) {
  override lazy val toString: String = {
    s"CountingSortResult(\n" +
      s"\tsorted: $sorted\n" +
      s"\toriginal: $original\n" +
      s"\tinSorted: $inSorted\n" +
      s"\tinOriginal: $inOriginal\n" +
      ")"
  }
}

object Nucleobase {
  val nucleobaseOrdering = new Ordering[Nucleobase] {
    override def compare(x: Nucleobase, y: Nucleobase): Int = x.priority.compareTo(y.priority)
  }
}

trait Nucleobase {
  def priority: Int

  def repr: Char

  override def toString: String = repr.toString
}

case object Stop extends Nucleobase {
  val priority: Int = 0
  val repr = '$'
}

case object Adenine extends Nucleobase {
  val priority: Int = 1
  val repr = 'A'
}

case object Cytosine extends Nucleobase {
  val priority: Int = 2
  val repr = 'C'
}

case object Guanine extends Nucleobase {
  val priority: Int = 3
  val repr = 'G'
}

case object Thymine extends Nucleobase {
  val priority: Int = 4
  val repr = 'T'
}

case object DeclarativeCountingSort {
  def sort[T](original: Vector[T])(alphabet: Vector[T]): CountingSortResult[T] = {
    @tailrec
    def count(in: Vector[T], counts: Map[T, Int]): Map[T, Int] = in match {
      case head +: tail => count(tail, counts + (head -> (counts(head) + 1)))
      case _ => counts
    }

    @tailrec
    def construct(buckets: Vector[T], counts: Map[T, Int], acc: Vector[T]): Vector[T] = buckets match {
      case head +: tail if counts(head) > 0 => construct(tail, counts, acc ++ Vector.fill(counts(head))(head))
      case head +: tail => construct(tail, counts, acc)
      case _ => acc
    }

    @tailrec
    def scan(in: Vector[T], counts: Map[T, Int], scans: Map[T, Int], pos: Int): Map[T, Int] = in match {
      case head +: tail => scan(tail, counts, scans + (head -> pos), pos + counts(head))
      case _ => scans
    }

    @tailrec
    def reference(i: Int, seens: Map[T, Int], scans: Map[T, Int], inSorted: Vector[Int], inOriginal: Vector[Int]): (Vector[Int], Vector[Int]) = {
      if (i < 0 || i >= original.length) (inSorted, inOriginal)
      else {
        val t = original(i)
        val j = scans(t) + seens(t)
        reference(i + 1, seens + (t -> (seens(t) + 1)), scans, inSorted.updated(i, j), inOriginal.updated(j, i))
      }
    }

    val counts = count(original, Map().withDefaultValue(0))
    val sorted = construct(alphabet, counts, Vector())
    val scans = scan(alphabet, counts, Map().withDefaultValue(0), 0)
    val (inSorted, inOriginal) = reference(0, Map().withDefaultValue(0), scans, Vector.fill(original.length)(0), Vector.fill(original.length)(0))
    CountingSortResult(sorted, original, inSorted, inOriginal)
  }
}

object DeclarativeAlphabet {
  private lazy val indexMap = HashMap[Int, Nucleobase](
    Stop.priority -> Stop,
    Adenine.priority -> Adenine,
    Cytosine.priority -> Cytosine,
    Guanine.priority -> Guanine,
    Thymine.priority -> Thymine
  )

  private lazy val reprMap = HashMap[Char, Nucleobase](
    Stop.repr -> Stop,
    Adenine.repr -> Adenine,
    Cytosine.repr -> Cytosine,
    Guanine.repr -> Guanine,
    Thymine.repr -> Thymine
  )

  lazy val symbols: Vector[Nucleobase] = indexMap.values.toVector.sorted(Nucleobase.nucleobaseOrdering)

  def apply(idx: Int): Nucleobase = indexMap.getOrElse(idx, throw new IllegalArgumentException(s"Invalid index $idx"))

  def apply(repr: Char): Nucleobase = reprMap.getOrElse(repr, throw new IllegalMonitorStateException(s"Invalid representation $repr"))

  lazy val size: Int = symbols.size
}

object DeclarativeBWMatching {

  case class BWPreprocessedData[T](starts: Map[T, Int], occurs: Map[T, Vector[Int]])

  def start[T](in: Vector[T]): Map[T, Int] = {
    @tailrec
    def loop(i: Int, starts: Map[T, Int]): Map[T, Int] = {
      if (i < 0 || i >= in.length) starts
      else {
        val symbol = in(i)
        if (!starts.contains(symbol)) loop(i + 1, starts + (symbol -> i))
        else loop(i + 1, starts)
      }
    }

    loop(0, Map().withDefaultValue(0))
  }

  def occur[T](in: Vector[T], starts: Map[T, Int]): Map[T, Vector[Int]] = {
    val occurs = starts.mapValues(_ => Vector.fill(in.length + 1)(0))
    val keys = occurs.keys.toList

    @tailrec
    def loop(i: Int, sx: List[T], result: Map[T, Vector[Int]]): Map[T, Vector[Int]] = {
      if (i < 1 || i > in.length) result
      else sx match {
        case nextSymbol :: tail => {
          val currentSymbol = in(i - 1)
          val step = if (currentSymbol == nextSymbol) 1 else 0
          val prevVector = result(nextSymbol)
          val nextVector = prevVector.updated(i, prevVector(i - 1) + step)
          loop(i, tail, result + (nextSymbol -> nextVector))
        }
        case Nil => loop(i + 1, keys, result)
      }
    }

    loop(1, keys, occurs)
  }

  def preprocess[T](in: Vector[T], alphabet: Vector[T]): BWPreprocessedData[T] = {
    val csr = DeclarativeCountingSort.sort(in)(alphabet)
    val starts = start(csr.sorted)
    val occurs = occur(in, starts)
    BWPreprocessedData(starts, occurs)
  }

  def numOccurrences[T](in: Vector[T], bwp: BWPreprocessedData[T])(pattern: Vector[T]): Int = {
    @tailrec
    def loop(top: Int, bottom: Int, px: List[T]): Int = {
      if (top > bottom) 0
      else px match {
        case p :: ps => bwp.occurs.get(p) match {
          case None => 0
          case Some(pOccurs) => {
            val topOccur = pOccurs(top)
            val botOccur = pOccurs(bottom + 1)
            val start = bwp.starts(p)

            if (botOccur > topOccur) {
              loop(start + topOccur, start + botOccur - 1, ps)
            } else {
              0
            }
          }
        }
        case Nil => bottom - top + 1
      }
    }

    loop(0, in.length - 1, pattern.toList.reverse)
  }

  def main(args: Array[String]): Unit = {
    val bwt = StdIn.readLine().toVector.map(DeclarativeAlphabet(_))
    val nPatterns = StdIn.readLine().toInt
    val patterns = StdIn.readLine().split(" ").map(p => p.toVector.map(DeclarativeAlphabet(_)))
    val bwp = preprocess(bwt, DeclarativeAlphabet.symbols)
    val numOccurF = numOccurrences(bwt, bwp) _
    val patOccurs = patterns.map(p => numOccurF(p))
    println(patOccurs.mkString(" "))
  }
}
