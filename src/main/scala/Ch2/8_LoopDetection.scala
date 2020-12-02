package CTCI.Ch2

object LoopDetection {
  def findLoop[T >: AnyRef](lst: List[T]): List[T] = {
    var slow : List[T] = lst
    var fast : List[T] = lst

    while ( !(fast.isEmpty) || !(fast.tail.isEmpty) || (fast eq slow)) {
      slow = slow.tail
      fast = fast.tail.tail
    }

    if ( fast.isEmpty || fast.tail.isEmpty ) return List.empty[T]

    slow = lst

    while (slow ne fast){
      slow = slow.tail
      fast = fast.tail
    }

    fast
  }
}