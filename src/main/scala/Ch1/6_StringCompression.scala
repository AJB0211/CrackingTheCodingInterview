package CTCI.Ch1

object StringCompression {
  def compress(s: String): String = {
    // Check if compression will yield benefit
    // If not, return
    val finalLen: Int = countCompressedLength(s)
    if (finalLen > s.length){
      return s
    }

    val out = new collection.mutable.StringBuilder(finalLen)
    var run: Int = 0

    for (i <- Range(0, s.length)){
      run += 1

      if ( (i+1 >= s.length) || (s.charAt(i) != s.charAt(i+1))){
        out.append(s.charAt(i))
        out.append(run.toString)
        run = 0
      }
    }

    out.toString
  }


  def countCompressedLength(s: String): Int = {
    var length: Int = 0
    var run: Int = 0

    for (i <- Range(0, s.length)){
      run += 1

      if ( (i+1 >= s.length) || (s.charAt(i) != s.charAt(i+1))){
        length += 1 + run.toString.length
        run = 0
      }
    }

    length
  }
}