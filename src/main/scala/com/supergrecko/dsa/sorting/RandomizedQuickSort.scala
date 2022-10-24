package com.supergrecko.dsa.sorting

import scala.util.Random

/** RandomizedQuickSort is a divide & conquer sorting algorithm.
  *
  * It is derived from QuickSort and has the same properties as QuickSort. The
  * only difference is that the pivot element in partition is randomly chosen
  * and thus you're more likely to get good pivots over a large distribution
  * meaning you're less likely to hit the worst case scenario of QuickSort.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$O(n \lg n)$$
  * - Average case of $$O(n \lg n)$$
  * - Worst case of $$O(n \lg n)$$ because especially for large array you are
  *   close to never going to hit the worst case of QuickSort.
  *
  * See Wikipedia analysis: https://en.wikipedia.org/wiki/Quicksort#Analysis_of_randomized_quicksort
  */
object RandomizedQuickSort {
  def sort[T : Ordering](xs: Array[T]): Unit = sort(xs, 0, xs.length - 1)
  def sort[T : Ordering](xs: Array[T], low: Int, high: Int): Unit = {
    // Keep going as long as cursors have not crossed
    if (low < high) {
      val pivot = partition(xs, low, high)
      sort(xs, low, pivot - 1)
      sort(xs, pivot + 1, high)
    }
  }

  /** RandomizedPartition is the driver for RandomizedQuickSort.
    *
    * It selects a pivot element (in this case, a random one), places it in its
    * right slot on the sorted list, and ensures all the elements to the left
    * are smaller, and all elements to the right are larger.
    *
    * The return value from Partition is the index the pivot was placed at.
    */
  def partition[T : Ordering](xs: Array[T], low: Int, high: Int): Int = {
    // Random.between is (inclusive, exclusive]
    val pivot = Random.between(low, high + 1)
    QuickSort.swap(xs, high, pivot)
    QuickSort.partition(xs, low, high)
  }
}
