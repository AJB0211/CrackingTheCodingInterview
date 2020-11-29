package CrackingTheCodingInterview

object StringRotation{
  // Problem statement says "given a isSubstring method"
  // Scala has the "contains" method on strings

  def isRotation(s1: String, s2: String): Boolean = if ((s1.length == s2.length) && (s1.length >0)) {
      (s1 + s1) contains s2
    } else {false}

}