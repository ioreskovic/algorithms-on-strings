import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.control.Breaks._

object SuffixArray {

  case class Tree(text: String, next: ListBuffer[Node]) {
    def suffixArray: List[Int] = {
      @tailrec
      def loop(q: List[Int], res: TreeMap[String, Int]): TreeMap[String, Int] = q match {
        case n :: ns => {
          val currentNode = next(n)
          if (currentNode.haveNeighbours) {
            loop(ns ::: currentNode.neighbours, res)
          } else {
            loop(
              ns,
              res + (text.substring(currentNode.generalStart, currentNode.start + currentNode.offset + 1) -> currentNode.generalStart)
            )
          }
        }
        case _ => res
      }

      loop(List(0), TreeMap()).values.toList
    }
  }

  object Tree {
    def apply(text: String): Tree = {
      val tree = scala.collection.mutable.ListBuffer[Node]()
      var count: Int = 0
      tree.append(Node(0, -1, count))
      count = count + 1
      var length = text.length

      import java.util
      var j = 0
      while (j < length) {
        var initialStart = length - 1 - j
        var initialOffset = j
        var currentNode = tree(0)
        while (currentNode.next(letterToIndex(text.charAt(initialStart))) > 0) {
          currentNode = tree(currentNode.next(letterToIndex(text.charAt(initialStart))))
          val currentStart = currentNode.start
          val currentOffset = currentNode.offset
          var removeIndex = 1
          var i = 1

          breakable {
            while (i < currentOffset + 1) {
              if (text.charAt(currentStart + i) != text.charAt(initialStart + i)) break
              removeIndex += 1
              i += 1
            }
          }
          if (currentOffset + 1 - removeIndex > 0) {
            val newNode = Node(currentStart + removeIndex, currentOffset - removeIndex, {
              count += 1; count - 1
            })
            newNode.generalStart = currentNode.generalStart
            currentNode.start = initialStart
            currentNode.offset = removeIndex - 1
            tree.append(newNode)
            if (currentNode.haveNeighbours) {
              newNode.next = util.Arrays.copyOf(currentNode.next, currentNode.next.length)
              newNode.haveNeighbours = true
              currentNode.initNext
            }
            currentNode.next(letterToIndex(text.charAt(newNode.start))) = newNode.id
            currentNode.haveNeighbours = true
          }
          initialStart += removeIndex
          initialOffset -= removeIndex
        }
        val newNode = Node(initialStart, initialOffset, {
          count += 1; count - 1
        })
        newNode.generalStart = length - 1 - j
        tree.append(newNode)
        currentNode.next(letterToIndex(text.charAt(initialStart))) = newNode.id
        currentNode.haveNeighbours = true
        j += 1
      }
      Tree(text, tree)
    }
  }

  def letterToIndex(letter: Char): Int = letter match {
    case 'A' => 0
    case 'C' => 1
    case 'G' => 2
    case 'T' => 3
    case '$' => 4
    case _ => throw new IllegalArgumentException
  }

  class Node() {
    var start: Int = -1
    var offset: Int = -1
    var id: Int = -1
    var generalStart: Int = 0
    var next: Array[Int] = Array.fill(5)(-1)
    var haveNeighbours: Boolean = false

    def initNext: Node = {
      next = Array.fill(5)(-1)
      haveNeighbours = false
      this
    }

    def neighbours: List[Int] = next.filter(_ > 0).toList
  }

  object Node {
    def apply(): Node = {
      val node = new Node()
      node.next = Array.fill(5)(-1)
      node
    }

    def apply(start: Int, offset: Int, id: Int): Node = {
      val node = Node()
      node.start = start
      node.offset = offset
      node.id = id
      node
    }
  }

  def main(args: Array[String]): Unit = {
    println(Tree(StdIn.readLine()).suffixArray.mkString(" "))
  }
}
