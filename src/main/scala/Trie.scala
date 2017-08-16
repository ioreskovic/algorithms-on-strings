abstract class Trie {
  def isEmpty: Boolean = links.isEmpty
  def links: Map[Char, Trie]
  def hasLink(char: Char): Boolean = links.contains(char)
  def consume(chars: List[Char]): Trie
  def contains(chars: List[Char]): Boolean

  def transitions: List[Transition] = {
    def loop(from: Int, t: Trie): List[Transition] = t match {
      case Leaf() => Nil
      case _ => t.links.foldLeft(List[Transition]()){ case (acc, ((c, y))) => Transition(from, from + acc.size + 1, c) :: loop(from + acc.size + 1, y) ::: acc }
    }

    loop(0, this)
  }
}

case class Transition(from: Int, to: Int, char: Char)

object Trie {
  def apply(): Trie = Root(Map().withDefaultValue(Leaf()))
  def apply(s: String): Trie = Trie().consume(s.toList)
}

case class Root(links: Map[Char, Trie] = Map().withDefaultValue(Leaf())) extends Trie {
  override def consume(chars: List[Char]): Trie = chars match {
    case c :: cs if hasLink(c) => Root(links + (c -> links(c).consume(cs)))
    case c :: cs => Root(links + (c -> Leaf().consume(cs)))
    case _ => this
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => links(c).contains(cs)
    case _ => true
  }

  lazy override val toString: String = {
    "Root[\n" + links.mkString(", \n") + "\n]"
  }
}

case class Node(links: Map[Char, Trie] = Map().withDefaultValue(Leaf())) extends Trie {
  override def consume(chars: List[Char]): Trie = chars match {
    case c :: cs if hasLink(c) => Node(links + (c -> links(c).consume(cs)))
    case c :: cs => Node(links + (c -> Leaf().consume(cs)))
    case _ => this
  }

  lazy override val toString: String = {
    "\tNode[\n" + links.mkString(", \n") + "\n\t]"
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => links(c).contains(cs)
    case _ => true
  }
}

case class Leaf() extends Trie {
  override def hasLink(char: Char): Boolean = false

  override def links: Map[Char, Trie] = Map()

  override def consume(chars: List[Char]): Trie = chars match {
    case c :: cs => Node((links + (c -> Leaf().consume(cs))).withDefaultValue(Leaf()))
    case _ => this
  }

  lazy override val toString: String = {
    "\t\tLeaf[\n" + links.mkString(", \n") + "\n\t\t]"
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => false
    case _ => true
  }
}
