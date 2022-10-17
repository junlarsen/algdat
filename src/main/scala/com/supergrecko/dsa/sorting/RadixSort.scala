package com.supergrecko.dsa.sorting

/** RadixSort is a sorting algorithm.
  *
  * It's a stable sorting algorithm, backed by another stable sorting algorithm,
  * in a lot of cases, CountingSort.
  *
  * RadixSort works by working through the different digits in the number,
  * starting at the back, sorting the list the numbers by their number at the
  * each digit.
  *
  * The sorting implementation of RadixSort has to be stable to ensure that the
  * elements don't swap places between digits.
  *
  * The time complexity depends on the backing sorting algorithm, but with a
  * standard implementation of CountingSort it has:
  *
  * - Best case of $$\Theta(d(n + k))$$ where $$d$$ is the number of digits in
  *   the largest number.
  * - Average case of $$\Theta(d(n + k))$$ for same reason
  * - Worst case of $$\Theta(d(n + k))$$ for same reason
  */
object RadixSort {
  def sort(xs: Array[Int]): Unit = {
    val max = xs.max
    var exp = 1
    // Effectively, for n in digits(max(xs))
    while (max / exp > 0) {
      countingSort(xs, exp)
      exp *= 10
    }
  }

  /**
    * Stable, in-place implementation of CountingSort as intermediary stable
    * sorting algorithm for RadixSort.
    */
  def countingSort(xs: Array[Int], exponent: Int): Unit = {
    val size = xs.length
    val max = xs.max
    val merges = new Array[Int](size)
    val count = new Array[Int](max)
    for (j <- xs.indices) {
      // We are not counting occurrences of numbers, but occurrences of numbers
      // that have the same digit at the nth index.
      count((xs(j) / exponent) % 10) += 1
    }
    for (i <- 1 until max) {
      count(i) += count(i - 1)
    }
    var j = size - 1
    while (j >= 0) {
      // Repeat the same calculation for the moving of numbers
      merges(count((xs(j) / exponent) % 10) - 1) = xs(j)
      count((xs(j) / exponent) % 10) -= 1
      j -= 1
    }
    // Because we want the numbers in-place unlike regular CountingSort we need
    // to copy them all over.
    for (i <- merges.indices) {
      xs(i) = merges(i)
    }
  }
}
