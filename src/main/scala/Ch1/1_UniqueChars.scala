package CTCI.Ch1

object UniqueChars {
  def isUnique(s: String): Boolean = {
    // If there are limitations on the character set they can be imposed to make this default for long strings

    var check: Int = 0
    for (i <- Range(0,s.length)){
      val charVal = s.charAt(i).toInt
      if ((check & (1 << charVal)) > 0) {
        return false
      } else {
        check |= (1 << charVal)
      }
    }

    return true
  }
}