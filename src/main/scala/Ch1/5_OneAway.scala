package CTCI.Ch1

object OneAway {
  def isOneAway(s1: String, s2: String): Boolean = {
    // Only these three operations are valid
    // Substitution
    if (s1.length == s2.length) {return isReplaceable(s1,s2)}
    // Deletion from string 1 is the same as insertion into string 2 so these can be handled with the same method
    if (s1.length == (s2.length - 1)) {return isInsertable(s1,s2)}
    if (s2.length == (s1.length - 1)) {return isInsertable(s2,s1)}

    return false
  }

  private def isReplaceable(s1: String, s2: String): Boolean = {
    // flag indicates whether a mismatch has occured
    // a second mismatch while the flag is true will lead to an early return
    var flag: Boolean = false

    s1.zip(s2).foreach { case ((c1: Char, c2: Char)) =>
      if (c1 != c2) {
        if (flag){
          return false
        } else {flag = true}
      }
    }

    return true
  }

  private def isInsertable(s1: String, s2: String): Boolean = {
    // Given our earlier call to this function...
    require(s1.length < s2.length, "String 1 should be shorter than String 2, this constraint is placed in isOneAway")

    // Use a double iterator
    // Shift the second iterator similar to the boolean flag from isReplaceable
    // If the mismatch occurs again and the iterator is already shifted, return false instantly
    var it1: Int = 0
    var it2: Int = 0

    while((it2 < s2.length) && (it1 < s1.length)) {
      if (s1.charAt(it1) != s2.charAt(it2)) {
        if (it1 != it2){
          return false
        } else {
          it2 += 1
        }
      } else {
        it1 += 1
        it2 += 1
      }
    }

    return true
  }
}