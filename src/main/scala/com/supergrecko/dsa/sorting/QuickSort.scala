package com.supergrecko.dsa.sorting

/** QuickSort is a divide & conquer sorting algorithm.
  *
  * It is a comparison-based, unstable sorting algorithm that sorts the elements
  * in-place in the input array.
  *
  * It operates by a sub-procedure called Partition which picks a pivot and
  * ensures the pivot is in its right slot on the sorted list and ensures all the elements to the
  * left are smaller, and all elements to the right are larger.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$O(n \lg n)$$
  * - Average case of $$O(n \lg n)$$
  * - Worst case of $$O(n^2^)$$ for cases where the array is sorted in either
  *   direction or all the items are equal.
  *
  * This is because the sub-procedure Partition takes linear time, while
  * QuickSort at the same time divides the elements into a recursion tree of
  * size $$\lg n$$.
  *
  * Because the performance of QuickSort depends heavily on the pivot element
  * selected you can often achieve better runtime with a random pivot if you
  * don't know what the partitions will look like.
  */
object QuickSort {
  import Ordering.Implicits._
  def sort[T : Ordering](xs: Array[T]): Unit = sort(xs, 0, xs.length - 1)
  def sort[T : Ordering](xs: Array[T], low: Int, high: Int): Unit = {
    // Keep going as long as cursors have not crossed
    if (low < high) {
      val pivot = partition(xs, low, high)
      sort(xs, low, pivot - 1)
      sort(xs, pivot + 1, high)
    }
  }

  /** Partition is the driver for QuickSort.
    *
    * It selects a pivot element (in this case, the rightmost item), places it
    * in its right slot on the sorted list and ensures all the elements to the
    * left are smaller, and all elements to the right are larger.
    *
    * The return value from Partition is the index the pivot was placed at.
    */
  def partition[T : Ordering](xs: Array[T], low: Int, high: Int): Int = {
    val pivot = xs(high)
    var index = low - 1
    // Iterate through all the items in the span
    for (j <- low until high) {
      val current = xs(j)
      // If the current element is less than the pivot, move it forwards
      if (current <= pivot) {
        index += 1
        // Move the current towards the left (because it's smaller than pivot)
        swap(xs, index, j)
      }
    }
    // Move pivot to the right position
    swap(xs, index + 1, high)
    index + 1
  }

  /** Swap two elements in an array in-place. */
  def swap[T](xs: Array[T], a: Int, b: Int): Unit = {
    val temp = xs(a)
    xs(a) = xs(b)
    xs(b) = temp
  }
}
