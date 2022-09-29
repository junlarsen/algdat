package com.supergrecko.dsa.sorting

/** CountingSort is a sorting algorithm.
  *
  * It is a counting-based, stable algorithm that copies the sorted elements
  * into a new array.
  *
  * CountingSort works by counting occurrences of each element in the array and
  * creating determining the final offset for the element in the new array.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$\Theta(n + k)$$
  * - Worst case of $$\Theta(n + k)$$
  * - Average case of $$\Theta(n + k)$$
  *
  * It should be noted that CountingSort can be space intensive for arrays
  * where the maximum value is large, as it allocates a counting array the
  * length of the largest value.
  *
  * This implementation could be optimized further in terms of space complexity
  * by making the counting array the size of the value range instead of 0-max.
  */
object CountingSort {
  def sort(xs: Array[Int]): Array[Int] = sort(xs, xs.length, xs.max)
  def sort(xs: Array[Int], size: Int, max: Int): Array[Int] = {
    val merges = new Array[Int](size)
    val count = new Array[Int](max)
    for (j <- xs) {
      // Minus one to offset 0-indexing
      count(j - 1) += 1
    }
    for (i <- 1 until max) {
      count(i) += count(i - 1)
    }
    // Count down from the top, minus one to offset 0-indexing. The reason we
    // count downwards is to preserve stability. If we count from the start, we
    // do not necessarily preserve stability because we would have to know how
    // many occurrences of the current item there was to properly offset.
    var j = size - 1
    while (j >= 0) {
      // Minus one, both places to offset 0-indexing
      merges(count(xs(j) - 1) - 1) = xs(j)
      count(xs(j) - 1) -= 1
      j -= 1
    }
    merges
  }
}
