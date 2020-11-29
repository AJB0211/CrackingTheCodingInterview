package CTCI.Ch1

object URLify {
  // This is cute but I would use regex here anyways
  def replaceSpaces(s: String, trueLength: Int): String = {
    var spaceCount: Int = 0
    s.substring(0,trueLength).foreach{ (c: Char) =>
      if (c == ' ') {spaceCount += 1}
    }

    var builderIt: Int = trueLength + spaceCount * 2

    // Must use default string for the builder so length is created properly
    // Just setting capacity is insufficient
    val builder = new StringBuilder(builderIt, " "*builderIt)

    // Otherwise indexing will be one space ahead due to zero-indexing
    builderIt -= 1
    // This is inefficient but convenient
    s.substring(0, trueLength).reverse.foreach { (c: Char) =>
      if (c == ' ') {
        builder(builderIt) = '0'
        builder(builderIt - 1) = '2'
        builder(builderIt - 2) = '%'
        builderIt -= 3
      } else {
        builder(builderIt) = c
        builderIt -= 1
      }

    }

    builder.toString
  }
}