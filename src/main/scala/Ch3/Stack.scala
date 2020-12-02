package CTCI.core

case class EmptyStackException(message: String) extends Exception(message)

// Scala does not have a Stack by default, this was originally written for 2.13.3
// It's suggested to just us a var list, which is usually sufficient
// but for the purpose of these exercises a mutable data structure is convenient
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