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
    def consume(cbx: List[Char], position: Int): ImmutableSuffixTree = {
      links.keys.find(l => l.nonEmpty && l.head == cbx.head) match {
        case None => SuffixTreeNode(links + (cbx -> SuffixTreeLeaf(position)))
        case Some(cax) => ImmutableSuffixTree.branch(cax, cbx) match {
            case (cx, Nil, Nil) => this
            case (cx, Nil, bx) => SuffixTreeNode(links + (cx -> links(cx).consume(bx, position)))
            case (cx, ax, Nil) => throw new IllegalArgumentException("Should not happen: CAX=" + cax.mkString + " vs CBX=" + cbx.mkString)
            case (cx, ax, bx) => SuffixTreeNode(links - cax + (cx -> SuffixTreeNode(Map(ax -> links(cax))).consume(bx, position)))
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
