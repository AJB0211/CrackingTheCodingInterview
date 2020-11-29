package CTCI.Ch2

import collection.mutable.ListBuffer

object DeleteMiddleNode {
  def delete[T](node: ListBuffer[T]): Unit = {
    if (node == Nil || node.tail == Nil){
      throw new Exception("Cannot work on last node in the list or the Nil end node")
    }

    // Error is ListBuffer does not have members head or tail, this contradicts documentation
    // No idea what's going on here
    /*
    val terminal = node.tail
    node.head = terminal.head
    node.tail = terminal.tail
     */
  }
}