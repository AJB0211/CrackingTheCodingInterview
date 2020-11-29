package CTCI.Ch2

import collection.mutable.ListBuffer

object Partition{
  // How to write type constraint for T to be subtype of order?
  type T = Int

  def partition (lst: List[T], x: T): List[T] = {
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