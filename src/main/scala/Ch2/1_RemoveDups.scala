package CTCI.Ch2


object RemoveDups{
  def dedup[T](xs: List[T]): List[T] = {

    def recurse[T](lst: List[T], set: Set[T] = Set[T]()): List[T] = lst match {
      case y :: ys if (set contains y) => recurse(ys, set)
      case y :: ys => y :: recurse(ys, set + y)
      case Nil => Nil
    }

    recurse(xs)
  }
}