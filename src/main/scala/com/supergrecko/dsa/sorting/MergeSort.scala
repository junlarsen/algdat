package com.supergrecko.dsa.sorting

import scala.reflect.ClassTag

/** MergeSort is a divide & conquer sorting algorithm.
  *
  * It is a comparison-based, unstable sorting algorithm that sorts the elements
  * in-place in the input array.
  *
  * It operates by a sub-procedure called Merge which splits the array at the
  * boundary and sorts one side of the pivot.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$O(n \lg n)$$ because merge sort will always go through the
  *   entire list, calling itself twice with the halves.
  * - Average case of $$O(n \lg n) because it will do the same amount of
  *   iterations
  * - Worst case of $$O(n \lg n) because it cannot end up iterating more.
  *
  * This is because the sub-procedure Merge takes linear time, while MergeSort
  * at the same time divides the elements into a recursion tree of size
  * $$\lg n$$.
  */
object MergeSort {
  import Ordering.Implicits._
  def sort[T : Ordering : ClassTag](xs: Array[T]): Unit = sort(xs, 0, xs.length - 1)
  def sort[T : Ordering : ClassTag](xs: Array[T], low: Int, high: Int): Unit = {
    // Keep going as long as cursors have not crossed
    if (low < high) {
      val middle = ((high + low) / 2).floor.toInt
      sort(xs, low, middle)
      sort(xs, middle + 1, high)
      merge(xs, low, middle, high)
    }
  }

  /** Merge is the driver for MergeSort.
    *
    * It merges two subsequences of the array, ensuring that the span of the two
    * subsequences are sorted after the call. You already assume that both
    * subsequences are sorted (MergeSort calls Merge after the recursive call
    * ensuring the subsequences are sorted).
    *
    * Merge will then pick elements from the top of each sequence, merging them
    * into one sequence where all the elements are sorted. In MergeSort, this
    * is the main sorting and comparison step.
    *
    * The complexity of merge is $$O(n)$$ where $$n$$ is the size span of
    * the original array.
    */
  def merge[T : Ordering : ClassTag](xs: Array[T], low: Int, mid: Int, high: Int): Unit = {
    val lsz = mid - low + 1
    val rsz = high - mid
    // Initialize arrays
    val left = new Array[T](lsz).zipWithIndex.map { case (_, i) => xs(low + i) }
    val right = new Array[T](rsz).zipWithIndex.map { case (_, i) => xs(mid + i + 1) }

    var i = 0
    var j = 0
    var k = low
    // Go through the entire boundary, or until one of left/right are empty
    while (k < high && i < lsz && j < rsz) {
      if (left(i) <= right(j)) {
        xs(k) = left(i)
        i += 1
      } else {
        xs(k) = right(j)
        j += 1
      }
      k += 1
    }

    // Empty the left list if not empty
    for (x <- i until lsz) {
      xs(k) = left(x)
      k += 1
    }

    // Empty the right list if not empty
    for (x <- j until rsz) {
      xs(k) = right(x)
      k += 1
    }
  }
}
