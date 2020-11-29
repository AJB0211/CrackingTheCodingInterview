package CTCI.Ch1

object PalindromePermutation {
  def isPermutation(s: String): Boolean = {
    val m = collection.mutable.Map[Char,Int]()

    // Should probable handle spaces and other special characters
    // But these cases are poorly defined in the problem statement
    s.foreach{ (c: Char) =>
      m.update(c, m.getOrElse(c, 0) + 1)
    }

    var flag: Boolean = false

    m.values.foreach{ (n: Int) =>
      if ( (n % 2) != 0) {
        if (flag){
          return false
        } else {flag = true}
      }
    }

    return true
  }
}