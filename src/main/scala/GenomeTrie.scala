import scala.annotation.tailrec

class GenomeTrie(var a: Option[GenomeTrie] = None, var c: Option[GenomeTrie] = None, var g: Option[GenomeTrie] = None, var t: Option[GenomeTrie] = None, var x: Option[GenomeTrie] = None, var i: Int = -1) {

  def isEmpty: Boolean = {
    a.isEmpty && c.isEmpty && g.isEmpty && t.isEmpty && x.isEmpty
  }

  def links: List[GenomeTrie] = List(a, c, g, t, x).flatten

  def hasLink(char: Char): Boolean = char match {
    case 'A' => a.isDefined
    case 'C' => c.isDefined
    case 'G' => g.isDefined
    case 'T' => t.isDefined
    case '$' => x.isDefined
    case _ => throw new IllegalArgumentException
  }

  def apply(char: Char): Option[GenomeTrie] = char match {
    case 'A' => a
    case 'C' => c
    case 'G' => g
    case 'T' => t
    case '$' => x
    case _ => throw new IllegalArgumentException
  }

  def update(char: Char, gt: GenomeTrie): GenomeTrie = char match {
    case 'A' => a = Some(gt); this;
    case 'C' => c = Some(gt); this;
    case 'G' => g = Some(gt); this;
    case 'T' => t = Some(gt); this;
    case '$' => x = Some(gt); this;
    case _ => throw new IllegalArgumentException
  }

  def consume(string: String, loc: Int): GenomeTrie = {
    @tailrec
    def loop(gt: GenomeTrie, chars: List[Char]): Unit = chars match {
      case Nil =>
      case head :: tail => {
        val ngt = gt(head) match {
          case None => new GenomeTrie(i = loc)
          case Some(ogt) => ogt
        }

        gt(head) = ngt
        loop(ngt, tail)
      }
    }

    loop(this, string.toList)
    this
  }

  override def toString: String = {
    val links = List(('A', a), ('C', c), ('G', g), ('T', t), ('X', x))
    val linksString = links.collect{ case (char, Some(trie)) => s"$char -> $trie" }.mkString(", ")
    s"Node($i)[$linksString]"
  }

  def locations(s: String): List[Int] = {
    @tailrec
    def collect(tx: List[GenomeTrie], res: List[Int]): List[Int] = {
      if (tx.nonEmpty) {
        if (tx.head.isEmpty) {
          collect(tx.tail, tx.head.i :: res)
        }
        else {
          collect(tx.head.links ::: tx.tail, res)
        }
      }
      else res
    }

    @tailrec
    def traverse(gt: GenomeTrie, cx: List[Char]): List[Int] = cx match {
      case last :: Nil if last == '$' => collect(List(gt), Nil)
      case head :: tail if gt.hasLink(head) => traverse(gt(head).get, tail)
      case _ => Nil
    }

    traverse(this, (s + '$').toList)
  }
}

object GenomeTrie {
  def apply(): GenomeTrie = new GenomeTrie()

  def suffixTrie(s: String): GenomeTrie = {
    val trie = new GenomeTrie()
    s.tails.zipWithIndex.foreach{ case (x, i) => trie.consume(x + '$', i) }
    trie
  }
}
