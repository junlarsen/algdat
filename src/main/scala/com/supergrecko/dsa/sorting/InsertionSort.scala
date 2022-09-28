package com.supergrecko.dsa.sorting

/** Insertion Sort is a sorting algorithm.
  *
  * It is a comparison-based, stable sorting algorithm that sorts the elements
  * in-place in the input array.
  *
  * InsertionSort works by selecting each element from the list and moving it as
  * far down the list as possible by comparing its right neighbour.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$O(n)$$ in the case that the input array is already
  *   sorted.
  * - Worst case of $$O(n^2^)$$ in the case that the input is reverse sorted.
  *   This means that the inner loop has to work through the entire array,
  *   comparing (and failing each time).
  * - Average case of $$O(n^2^)$$ because it will typically run many
  *   iterations in the inner loop for a uniformly filled array.
  */
object InsertionSort {
  import Ordering.Implicits._
  def sort[T : Ordering](xs: Array[T]): Unit = sort(xs, xs.length)
  def sort[T : Ordering](xs: Array[T], size: Int): Unit = {
    // Sorting has to visit each element to ensure the list is sorted.
    for (i <- 0 until size) {
      val key = xs(i)
      var j = i - 1
      // Compare the current element with the previous elements in the list
      while (j >= 0 && xs(j) > key) {
        // Shift left neighbour to the right for as long as the comparison
        // fails
        xs(j + 1) = xs(j)
        j -= 1
      }
      // Place the element in the right place. The code has +1 here because the
      // previous loop reduced j by one more than "necessary".
      xs(j + 1) = key
    }
  }
}
