sealed class EqClass private[EqClass](val ord: Int = 0) {
  @inline def same: EqClass = EqClass(ord)
  @inline def next: EqClass = EqClass(ord + 1)
  @inline override lazy val toString: String = s"EqC[$ord]"
}

object EqClass {
  @inline def apply(): EqClass = EqClass(0)
  @inline def apply(init: Int): EqClass = new EqClass(init)
  @inline implicit def toEqClass(i: Int): EqClass = EqClass(i)
  @inline implicit def toInt(eqClass: EqClass): Int = eqClass.ord
}
