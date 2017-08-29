object Alphabet {
  val size: Int = 5

  implicit def index(char: Char): Int = {
    if (char == '$') 0
    else if (char == 'A') 1
    else if (char == 'C') 2
    else if (char == 'G') 3
    else if (char == 'T') 4
    else throw new IllegalArgumentException
  }

  implicit def letter(idx: Int): Char = {
    if (idx == 0) '$'
    else if (idx == 1) 'A'
    else if (idx == 2) 'C'
    else if (idx == 3) 'G'
    else if (idx == 4) 'T'
    else throw new IllegalArgumentException
  }

  val indices: Range = 0 until size

  def apply(idx: Int): Char = letter(idx)
}
