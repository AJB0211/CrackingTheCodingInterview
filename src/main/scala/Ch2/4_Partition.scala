package CTCI.Ch2

import collection.mutable.ListBuffer

object Partition{

  def partition[T](lst: List[T], x: T)(implicit ord: Ordering[T]): List[T] = {
    import ord._

    val low = ListBuffer[T]()
    val high = ListBuffer[T]()

    lst.foreach{ (v: T) =>
      if (v < x){
        low += v
      } else {high += v}
    }

    (low ++ high).toList
  }
}