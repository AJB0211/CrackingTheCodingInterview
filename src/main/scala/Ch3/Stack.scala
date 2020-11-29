package CTCI.core

case class EmptyStackException(message: String) extends Exception(message)

class Stack[A] {
  private var elems: List[A] = List.empty[A]

  def push(elem: A): Unit = {elems = elem :: elems}

  def peek: A = {
    if (elems == Nil) {throw new EmptyStackException("Cannot peek empty stack")}
    return elems.head
  }

  def pop: A = {
    if (elems == Nil) {throw new EmptyStackException("Cannot pop Nil Element")}
    val out: A = elems.head
    elems = elems.tail
    out
  }

  def isEmpty: Boolean = elems.isEmpty
}