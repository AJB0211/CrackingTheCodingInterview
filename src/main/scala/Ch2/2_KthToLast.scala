package CTCI.Ch2

object KthToLast {
  def tail[T](lst: List[T], k: Int): List[T] = {
    var slow: List[T] = lst
    var fast: List[T] = lst

    for (_ <- 0 until k) {
      if (fast == Nil) return fast
      else {fast = fast.tail}
    }

    while (fast != Nil){
      slow = slow.tail
      fast = fast.tail
    }

    slow
  }
}