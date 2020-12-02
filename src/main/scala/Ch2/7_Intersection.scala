package CTCI.Ch2

object Intersection {
  def test: () = {
    val l1 = List(1,2,3)

    val l2 = 4 :: 5 :: l1
    val l3 = 6 :: l1

    println(Intersection.getIntersection(l2,l3))
  }

  /**
   *  Find the intersection of two Lists
   *
   * @return Intersection if intersection else Nil
   */
  def getIntersection[T >: AnyRef](xs: List[T], ys: List[T]): List[T] = {
    // Fix xs to always be the longer list, easier to reason with
    if (ys.length > xs.length) {getIntersection(ys,xs)}

    val xss: List[T] = xs.drop(xs.length - ys.length)

    @annotation.tailrec
    def inner(xs: List[T], ys:List[T]): List[T] = {
      if (xs eq ys) xs
      else inner(xs.tail, ys.tail)
    }

    inner(xss, ys)
  }
}