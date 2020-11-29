package CTCI.Ch1

object ZeroMatrix {
  // MxN matrix
  // If element is zero, zero entire row and col of that element

  type Matrix = Array[Array[Int]]

  def zeroRow(m: Matrix, rowIdx: Int): Unit = (0 until m(rowIdx).length).foreach{ m(rowIdx)(_) = 0 }

  def zeroCol(m:Matrix, colIdx: Int): Unit = (0 until m.length).foreach{ m(_)(colIdx) = 0 }

  def setZeros(m: Matrix): Unit = {
    require(validateMxN(m), "Needs to be an m x n array, a matrix")

    // Check if first row and col have zeros
    // Need to differentiate between a natural zero in these ranges
    // And one placed there later
    var zeroInRow: Boolean = false
    var zeroInCol: Boolean = false

    // Exists is far easier to use because the break statement in Scala is not easily accessible
    if (m(0).exists(_ == 0)){zeroInRow = true}
    if (m.exists(_(0) == 0)){zeroInCol = true}

    // Scan all other cells
    // Use first row to store col zeros
    // Use first col to store row zeros
    for (i <- Range(1, m.length);
         j <- Range(1, m(0).length)){
      if (m(i)(j) ==0){
        m(i)(0) = 0
        m(0)(j) = 0
      }
    }

    // Scan first row, zero cols
    for (i <- Range(0, m(0).length)){
      if (m(0)(i) == 0){
        zeroCol(m, i)
      }
    }

    // Scan first col, zero rows
    for (i <- Range(0, m.length)){
      if (m(i)(0) == 0){
        zeroRow(m, i)
      }
    }

    // Revisit Boolean flags and zero
    if (zeroInCol){zeroCol(m,0)}
    if (zeroInRow){zeroRow(m, 0)}

  }

  def validateMxN(m: Matrix): Boolean = {
    require(m.length != 0, "Matrix must not be empty")

    val n: Int = m(0).length

    m.forall( _.length == n)
  }

}