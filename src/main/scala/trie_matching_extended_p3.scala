import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap}
import scala.io.StdIn

object trie_matching_extended_p3 {

  case class TrieNode(links: MMap[Char, TrieNode] = MHashMap(), position: Option[Int] = None, private var maybeParent: Option[TrieNode] = None) {
    def hasLink(c: Char): Boolean = links.contains(c)

    def isEmpty: Boolean = links.isEmpty

    def withLink(c: Char, subTrie: TrieNode): TrieNode = {
      links.update(c, subTrie)
      this
    }

    def withParent(parent: TrieNode): TrieNode = {
      maybeParent = Some(parent)
      this
    }

    def apply(c: Char): TrieNode = {
      links(c)
    }

    def consume(s: String): TrieNode = consume(s, -1)

    def consume(s: String, location: Int): TrieNode = consume(s.toList, location)

    protected def consume(chars: List[Char], location: Int): TrieNode = {
      @tailrec
      def loop(trie: TrieNode, cx: List[Char]): TrieNode = cx match {
        case Nil => this
        case c :: Nil if trie.hasLink(c) => loop(trie(c), Nil)
        case c :: Nil => loop(trie.withLink(c, TrieNode(position = Some(location), maybeParent = Some(trie)))(c), Nil)
        case c :: cs if trie.hasLink(c) => loop(trie(c), cs)
        case c :: cs => loop(trie.withLink(c, TrieNode(maybeParent = Some(trie)))(c), cs)
      }

      loop(this, chars)
    }

    override def toString: String = {
      links.mkString(s"Trie($position)[", ", ", "]")
    }

    def locations(str: String): List[Int] = {
      @tailrec
      def collect(tx: List[TrieNode], res: List[Int]): List[Int] = tx match {
        case Nil => res
        case TrieNode(lx, Some(pos), _) :: ts => collect(lx.values.toList ::: ts, pos :: res)
        case t :: ts => collect(t.links.values.toList ::: ts, res)
      }

      @tailrec
      def traverse(trie: TrieNode, cx: List[Char]): List[Int] = cx match {
        case c :: Nil if c == '$' => collect(List(trie), Nil)
        case c :: cs if trie.hasLink(c) => traverse(trie(c), cs)
        case _ => Nil
      }

      traverse(this, (str + '$').toList)
    }

    def textLocations(str: String): List[Int] = {
      val chars = str.toCharArray

      @tailrec
      def prefixMatches(trie: TrieNode, i: Int): Boolean = {
        if (trie.isEmpty || trie.hasLink('$')) true
        else if (i < chars.length) {
          val c = chars(i)
          if (trie.hasLink(c)) prefixMatches(trie(c), i + 1)
          else false
        } else {
          false
        }
      }

      (0 until chars.length).foldLeft(List[Int]()){ case (acc, i) =>
        if (prefixMatches(this, i)) i :: acc
        else acc
      }.sorted
    }

    def transitions: List[TrieTransition] = {
      def loop(from: Int, t: TrieNode): List[TrieTransition] = {
        if (t.isEmpty) Nil
        else t.links.foldLeft(List[TrieTransition]()) { case (acc, ((c, y))) => TrieTransition(from, from + acc.size + 1, c) :: loop(from + acc.size + 1, y) ::: acc; }
      }

      loop(0, this)
    }
  }

  object TrieNode {
    def suffix(str: String): TrieNode = {
      val suffixTrie = TrieNode()
      str.tails.zipWithIndex.foreach{ case (suffix, position) => suffixTrie.consume(suffix + '$', position) }
      suffixTrie
    }
  }

  case class TrieTransition(from: Int, to: Int, char: Char)

  def main(args: Array[String]): Unit = {
    val text = StdIn.readLine()
    val nPatterns = StdIn.readLine().toInt
    val patterns = (0 until nPatterns).map(_ => StdIn.readLine())
    val timeStart = System.currentTimeMillis()
    val trie = patterns.foldLeft(TrieNode()){ case (t, p) => t.consume(p + '$') }
    val locations = trie.textLocations(text)
    val timeEnd = System.currentTimeMillis()
    println(locations.mkString(" "))
    println("Time: " + (timeEnd - timeStart))
  }
}
