package CTCI.Ch2

object Palindrome {
  /**
   * Convenience function to allow string inputs. This is supposed to be a list exercise.
   * @param str Input string
   * @return Boolean, whether string is palindrome
   */
  def isPalindrome(str: String): Boolean = isPalindrome(str.toList)

  /**
   * Checks whether input list is a palindrome
   * @param lst Input list
   * @tparam T Any type
   * @return Boolean, whether list is palindrome
   */
  def isPalindrome[T](lst: List[T]): Boolean = {
    var (first, last) = splitList(lst, lst.length /2)

    if (last.length%2 != 0) {last = last.tail}

    first == last
  }

  /**
   * Splits list into tuple of two lists
   * Reversing the first list in the process
   *
   * @param lst Input list to split
   * @param n Number of elements to take, placed in first list in tuple
   *
   * @return 2-tuple with first list of reversed n elements and second list undisturbed
   */
  def splitList[T](lst: List[T], n: Int): (List[T], List[T]) = {
    require(n>0, "Nonsensical to take negative increments from a list")

    @annotation.tailrec
    def inner(lst: List[T], n: Int, acc: List[T] = List.empty[T]): (List[T], List[T]) = lst match {
      case x :: Nil => (x::acc, List.empty[T])
      case x :: xs if (n > 0) => inner(xs, n-1, x::acc)
      case x :: xs if (n == 0) => (acc, lst)
      case _ => throw new Exception("This case should not be hit")
    }

    inner(lst, n)
  }
}