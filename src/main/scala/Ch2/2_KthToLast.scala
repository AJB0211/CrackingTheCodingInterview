package CTCI.Ch2

object KthToLast {
  def tail[T](lst: List[T], k: Int): List[T] = {
    // Using fast/slow pointers to space positions in list
    var slow: List[T] = lst
    var fast: List[T] = lst

    // Accelerate the fast pointer
    // If the list isn't long enough, return the empty list
    for (_ <- 0 until k) {
      if (fast == Nil) return fast
      else {fast = fast.tail}
    }

    // Otherwise, move pointers together and when fast pointer hits the end
    // Slow pointer is where it needs to be
    while (fast != Nil){
      slow = slow.tail
      fast = fast.tail
    }

    slow
  }
}