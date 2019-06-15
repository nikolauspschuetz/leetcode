object Solution {

  /**
    * A scientist has index `h`
    * if `h` of his or her N papers have at least `h` citations each
    * and the other (N - h) papers have <= h citations each.
    *
    * https://www.pnas.org/content/pnas/102/46/16569.full.pdf
    */

  def hIndex(citations: Array[Int]): Int = {

    var h = citations.size
    val c = citations.sorted(Ordering.Int.reverse)

    def left(i: Int) = {
      val a = c.slice(0, i)

      if (a.size == 0) true
      else a.map(_ >= i).reduce(_ & _)
    }

    def right(i: Int) = {
      val a = c.slice(i, c.size)

      if (a.size == 0) true
      else a.map(_ <= i).reduce(_ & _)
    }

    while (!(left(h) & right(h))) h -= 1

    h
  }
}