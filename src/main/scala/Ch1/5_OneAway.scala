package CTCI.Ch1

object OneAway {
  def isOneAway(s1: String, s2: String): Boolean = {
    if (s1.length == s2.length) {return isReplaceable(s1,s2)}
    if (s1.length == (s2.length - 1)) {return isInsertable(s1,s2)}
    if (s2.length == (s1.length - 1)) {return isInsertable(s2,s1)}

    return false
  }

  def isReplaceable(s1: String, s2: String): Boolean = {
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

  def isInsertable(s1: String, s2: String): Boolean = {
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