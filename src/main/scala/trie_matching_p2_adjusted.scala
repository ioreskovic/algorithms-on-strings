import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, HashMap => MHashMap}
import scala.io.StdIn

object trie_matching_p2_adjusted {

  case class GenomeTrie(
                         var aLink: Option[GenomeTrie] = None,
                         var cLink: Option[GenomeTrie] = None,
                         var tLink: Option[GenomeTrie] = None,
                         var gLink: Option[GenomeTrie] = None,
                         var xLink: Option[GenomeTrie] = None,
                         var position: Option[Int] = None) {

    def links: Map[Char, GenomeTrie] = Map('A' -> aLink, 'C' -> cLink, 'G' -> gLink, 'T' -> tLink, '$' -> xLink).filter(_._2.nonEmpty).mapValues(_.get)

    def isEmpty: Boolean = aLink.isEmpty && cLink.isEmpty && tLink.isEmpty && gLink.isEmpty && xLink.isEmpty

    def hasLink(char: Char): Boolean = {
      if (char == 'A') aLink.nonEmpty
      else if (char == 'C') cLink.nonEmpty
      else if (char == 'T') tLink.nonEmpty
      else if (char == 'G') gLink.nonEmpty
      else if (char == '$') xLink.nonEmpty
      else throw new IllegalArgumentException("Wrong alphabet letter")
    }

    def withLink(char: Char, subTrie: GenomeTrie): GenomeTrie = {
      if (char == 'A') { aLink = Some(subTrie); this }
      else if (char == 'C') { cLink = Some(subTrie); this }
      else if (char == 'T') { tLink = Some(subTrie); this }
      else if (char == 'G') { gLink = Some(subTrie); this }
      else if (char == '$') { xLink = Some(subTrie); this }
      else throw new IllegalArgumentException("Wrong alphabet letter")
    }

    def apply(char: Char): GenomeTrie = {
      if (char == 'A') aLink.get
      else if (char == 'C') cLink.get
      else if (char == 'T') tLink.get
      else if (char == 'G') gLink.get
      else if (char == '$') xLink.get
      else throw new IllegalArgumentException("Wrong alphabet letter")
    }

    def consume(str: String, pos: Int): GenomeTrie = consume(str.toList, pos)

    private def consume(chars: List[Char], pos: Int): GenomeTrie = {
      @tailrec
      def loop(trie: GenomeTrie, cx: List[Char]): GenomeTrie = cx match {
        case Nil => this
        case c :: Nil if trie.hasLink(c) => loop(trie(c), Nil)
        case c :: Nil => loop(trie.withLink(c, GenomeTrie(position = Some(pos)))(c), Nil)
        case c :: cs if trie.hasLink(c) => loop(trie(c), cs)
        case c :: cs => loop(trie.withLink(c, GenomeTrie())(c), cs)
      }

      loop(this, chars)
    }

    override def toString: String = {
      val p = if (position.nonEmpty) position.get else ""
      Map('A' -> aLink, 'C' -> cLink, 'G' -> gLink, 'T' -> tLink, '$' -> xLink).filter(_._2.nonEmpty).mapValues(_.get).mkString(s"Trie($p)[", ", ", "]")
    }

    def locations(str: String): List[Int] = {
      @tailrec
      def collect(tx: List[GenomeTrie], res: List[Int]): List[Int] = tx match {
        case Nil => res
        case (t @ GenomeTrie(_, _, _, _, _, Some(pos))) :: ts => collect(t.links.values.toList ::: ts, pos :: res)
        case t :: ts => collect(t.links.values.toList ::: ts, res)
      }

      @tailrec
      def traverse(trie: GenomeTrie, cx: List[Char]): List[Int] = cx match {
        case c :: Nil if c == '$' => collect(List(trie), Nil)
        case c :: cs if trie.hasLink(c) => traverse(trie(c), cs)
        case _ => Nil
      }

      traverse(this, (str + '$').toList)
    }

    def transitions: List[GenomeTrieTransition] = {
      def loop(from: Int, t: GenomeTrie): List[GenomeTrieTransition] = {
        if (t.isEmpty) Nil
        else t.links.foldLeft(List[GenomeTrieTransition]()) { case (acc, ((c, y))) => GenomeTrieTransition(from, from + acc.size + 1, c) :: loop(from + acc.size + 1, y) ::: acc; }
      }

      loop(0, this)
    }
  }

  object GenomeTrie {
    def suffix(str: String): GenomeTrie = {
      val suffixTrie = GenomeTrie()
      str.tails.zipWithIndex.foreach{ case (suffix, position) => suffixTrie.consume(suffix + '$', position) }
      suffixTrie
    }
  }

  case class GenomeTrieTransition(from: Int, to: Int, char: Char)

  def main(args: Array[String]): Unit = {
    val text = StdIn.readLine()
    val nPatterns = StdIn.readLine().toInt
    val patterns = (0 until nPatterns).map(_ => StdIn.readLine())
    val timeStart = System.currentTimeMillis()
    val trie = GenomeTrie.suffix(text)
    val locations = patterns.foldLeft(Set[Int]()){ case (s, p) => s ++ trie.locations(p) }.toList.sorted
    val timeEnd = System.currentTimeMillis()
    println(locations.mkString(" "))
//    println("Time: " + (timeEnd - timeStart))
  }
}
