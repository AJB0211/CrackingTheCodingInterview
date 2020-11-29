package CTCI.Ch1

object CheckPermutation {
  def isPermutation(s1: String, s2: String): Boolean = {
    if (s1.length != s2.length){return false}

    val charCounts: Array[Int] = Array[Int](128)

    // Go through first string and count use of each character
    s1.foreach{ (c: Char) =>
      charCounts(c) += 1
    }

    // Go through second array and decrement for each character
    // If any value becomes negative then false
    s2.foreach{ (c: Char) =>
      charCounts(c) -= 1
      if (charCounts(c) < 0){
        return false
      }
    }

    return true
  }
}