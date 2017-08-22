import scala.annotation.tailrec
import scala.io.StdIn

object immutable_suffix_trie_p4 {
  trait ImmutableSuffixTree {
    def links: Map[List[Char], ImmutableSuffixTree]
    def isEmpty: Boolean = links.isEmpty
    def consume(chars: List[Char], position: Int): ImmutableSuffixTree

    def edges: List[String] = {
      @tailrec
      def loop(q: List[ImmutableSuffixTree], res: List[String] = Nil): List[String] = {
        if (q.isEmpty) res
        else q.head match {
          case SuffixTreeLeaf(_) => loop(q.tail, res)
          case SuffixTreeNode(lx) => loop(lx.values.toList ::: q.tail, lx.keys.map(_.mkString).toList ::: res)
        }
      }

      loop(List(this))
    }

  }

  case class SuffixTreeLeaf(position: Int) extends ImmutableSuffixTree {
    override lazy val links: Map[List[Char], ImmutableSuffixTree] = Map()
    override def consume(chars: List[Char], position: Int): ImmutableSuffixTree = SuffixTreeNode(Map(chars -> SuffixTreeLeaf(position)))
    override lazy val toString: String = s"Leaf($position)"
  }

  case class SuffixTreeNode(links: Map[List[Char], ImmutableSuffixTree]) extends ImmutableSuffixTree {
    override def consume(chars: List[Char], position: Int): ImmutableSuffixTree = {
      val c = chars.head
      val link = links.keys.find(_.head == c)
      link match {
        case None => SuffixTreeNode(links + (chars -> SuffixTreeLeaf(position)))
        case Some(key) => {
          val (prefix, aKey, bKey) = ImmutableSuffixTree.branch(chars, key)
          if (prefix == key) {
            if (aKey == Nil) this
            else SuffixTreeNode(links + (prefix -> links(prefix).consume(aKey, position)))
          }
          else links(key) match {
            case SuffixTreeLeaf(oldPosition) => SuffixTreeNode(links - key + (prefix -> SuffixTreeNode(Map()).consume(aKey, position).consume(bKey, oldPosition)))
            case SuffixTreeNode(_) => SuffixTreeNode(links + (key -> links(key).consume(aKey, position))) // FIXME: AAA$
          }
        }
      }
    }

    override lazy val toString: String = links.mkString("Node[", ", ", "]")
  }

  object ImmutableSuffixTree {
    @tailrec
    final def branch(ax: List[Char], bx: List[Char], res: List[Char] = Nil): (List[Char], List[Char], List[Char]) = (ax, bx) match {
      case (a :: as, b :: bs) if a == b => branch(as, bs, a :: res)
      case _ => (res.reverse, ax, bx)
    }

    def apply(): ImmutableSuffixTree = SuffixTreeLeaf(0)
    def apply(str: String): ImmutableSuffixTree = {
      (0 until str.length).foldLeft(ImmutableSuffixTree()){ case (t, i) => t.consume(str.substring(i).toList, i) }
    }
  }

  def main(args: Array[String]): Unit = {
    ImmutableSuffixTree(StdIn.readLine()).edges.foreach(println)
  }
}
