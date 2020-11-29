package CTCI.Ch1

object RotateMatrix {
  type Matrix = Array[Array[Int]]

  def rotate(m: Matrix): Boolean = {
    // Easier than asking what the length is all the time
    // Also, math
    val n: Int = m.length

    // This operation cannot be performed on a nonsquare matrix
    if (!isSquare(m)){
      return false
    }

    for (
      // If n is odd we can ignore the middle cells
      start <- Range(0, n / 2);
      val end = n - 1 - start;
      i <- Range(start, end)
    ){
      val offset: Int = start - i;

      // Elements are rotated clockwise
      // But the order the writes are done is counterclockwise

      // Save first element that will be overwritten
      val right: Int = m(i)(end);

      // Right <- Top
      m(i)(end) = m(start)(i);

      // Top <- Left
      m(start)(i) = m(end - offset)(start);

      // Left <- Bottom
      m(end - offset)(start) = m(end)(end-offset);

      // Bottom <- Right
      m(end)(end-offset) = right;

    }

    true
  }

  def isSquare(m: Matrix): Boolean = {
    // Matrix is not empty
    if (m.length == 0){
      return false
    // None of the rows are the wrong length
    } else if (m.exists(_.length != m.length)){
      false
    } else {
      true
    }
  }


}