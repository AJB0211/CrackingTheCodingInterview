package CTCI.Ch2

object SumLists {
  def sum(l1: List[Int], l2: List[Int]): List[Int] = {
    def adder(xs: List[Int], ys: List[Int], carry: Int): List[Int] = (xs,ys) match {
      case (Nil, Nil) => if (carry == 0) Nil else List(carry)
      case (x :: xs, Nil) => summer(x, carry, xs, Nil)
      case (Nil, y :: ys) => summer(y, carry, ys, Nil)
      case (x :: xs, y :: ys) => summer(x + carry, y, xs ,ys)
    }


    def summer(x: Int, y: Int, xs: List[Int], ys: List[Int]): List[Int] = {
      val sum = x + y
      sum % 10 :: adder(xs,ys, sum / 10)
    }

    adder(l1, l2, 0)
  }
}