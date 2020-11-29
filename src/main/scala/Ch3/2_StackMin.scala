package CTCI.Ch3

import CTCI.core.Stack

class MinStack extends Stack[Int] {
  private val stackOfMins: Stack[Int] = new Stack[Int]()

  override def push(elem: Int): Unit = {
    if (stackOfMins.isEmpty || elem <= stackOfMins.peek){stackOfMins.push(elem)}
    super.push(elem)
  }

  override def pop: Int = {
    val out = super.pop
    if (out <= stackOfMins.peek) stackOfMins.pop
    out
  }

  def min: Int = {
    if (stackOfMins.isEmpty) return Integer.MAX_VALUE
    stackOfMins.peek
  }


}