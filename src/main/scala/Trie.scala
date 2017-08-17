import scala.annotation.tailrec

abstract class Trie {
  def isEmpty: Boolean = links.isEmpty

  def links: Map[Char, Trie]

  def hasLink(char: Char): Boolean = links.contains(char)

  def consume(chars: List[Char], len: Int): Trie

  def consume(s: String): Trie = consume(s.toList, s.length)

  def consume(s: String, i: Int): Trie = consume(s.toList, i)

  def contains(chars: List[Char]): Boolean

  def contains(s: String): Boolean = contains(s.toList)

  def locations(s: String): List[Int] = locations((s + '$').toList)

  def locations(chars: List[Char]): List[Int] = {

    def collectLeaves(t: Trie): List[Int] = t match {
      case Leaf(i) => List(i)
      case _ => t.links.values.flatMap(y => collectLeaves(y)).toList
    }

    @tailrec
    def traverse(t: Trie, cx: List[Char]): List[Int] = (t, cx) match {
      case (_, c :: Nil) if c == '$' => collectLeaves(t)
      case (_, c :: cs) if t.hasLink(c) => traverse(t.links(c), cs)
      case _ => Nil
    }

    traverse(this, chars)
  }

  def transitions: List[Transition] = {
    def loop(from: Int, t: Trie): List[Transition] = t match {
      case Leaf(_) => Nil
      case _ => t.links.foldLeft(List[Transition]()) { case (acc, ((c, y))) => Transition(from, from + acc.size + 1, c) :: loop(from + acc.size + 1, y) ::: acc }
    }

    loop(0, this)
  }
}

case class Transition(from: Int, to: Int, char: Char)

object Trie {
  def apply(): Trie = Root(Map().withDefaultValue(Leaf(-1)))

  def apply(s: String): Trie = Trie().consume(s)

  def suffix(s: String): Trie = {
    @tailrec
    def loop(t: Trie, i: Int, cx: List[Char]): Trie = cx match {
      case Nil => t.consume("$", i)
      case c :: cs => loop(t.consume(cx.mkString("") + '$', i), i + 1, cs)
    }

    loop(Trie(), 0, s.toList)
  }
}

case class Root(links: Map[Char, Trie] = Map().withDefaultValue(Leaf(-1))) extends Trie {
  override def consume(chars: List[Char], i: Int): Trie = chars match {
    case c :: cs if hasLink(c) => Root(links + (c -> links(c).consume(cs, i)))
    case c :: cs => Root(links + (c -> Leaf(i).consume(cs, i)))
    case _ => this
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => links(c).contains(cs)
    case _ => true
  }

  lazy override val toString: String = {
    "Root[" + links.mkString(",") + "]"
  }
}

case class Node(links: Map[Char, Trie] = Map().withDefaultValue(Leaf(-1))) extends Trie {
  override def consume(chars: List[Char], i: Int): Trie = chars match {
    case c :: cs if hasLink(c) => Node(links + (c -> links(c).consume(cs, i)))
    case c :: cs => Node(links + (c -> Leaf(i).consume(cs, i)))
    case _ => this
  }

  lazy override val toString: String = {
    "Node[" + links.mkString(",") + "]"
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => links(c).contains(cs)
    case _ => true
  }
}

case class Leaf(i: Int) extends Trie {
  override def hasLink(char: Char): Boolean = false

  override def links: Map[Char, Trie] = Map()

  override def consume(chars: List[Char], i: Int): Trie = chars match {
    case c :: cs => Node((links + (c -> Leaf(i).consume(cs, i))).withDefaultValue(Leaf(-1)))
    case _ => this
  }

  lazy override val toString: String = {
    "Leaf[" + i + "]"
  }

  override def contains(chars: List[Char]): Boolean = chars match {
    case c :: cs => false
    case _ => true
  }
}
