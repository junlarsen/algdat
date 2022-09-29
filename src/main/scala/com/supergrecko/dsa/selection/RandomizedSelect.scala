package com.supergrecko.dsa.selection

import com.supergrecko.dsa.sorting.RandomizedQuickSort

/** RandomizedSelect is a divide & conquer selection algorithm.
  *
  * It is a mix of QuickSort and BinarySearch, but it has the unique perk
  * that it has linear time expected runtime.
  *
  * It works by finding selecting a pivot in the array (in most cases, by random
  * meaning we can re-use RandomizedQuickSort's implementation of Partition).
  *
  * Because the pivot knows it's positioned correctly, and that all items on the
  * sides are either all larger or smaller it can compare its own index with the
  * target index to determine which side of the array to search through on next
  * iteration.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$\Theta(n)$$ since recurrence analysis breaks down to
  *   $$2n - 1$$.
  * - Average case of $$\Theta(n)$$
  * - Worst case of $$\Theta(n^2^)$$ because any algorithm taking a partition
  *   can hit squared complexity in worst case. Note that this is close to
  *   impossible over a larger array with randomized pivot.
  *
  * Note: this implementation differs from CLRS which finds the number of items
  * in the span and uses that to determine next iteration. This works just as
  * well, but is simpler because it skips that extra step.
  */
object RandomizedSelect {
  def select[T : Ordering](xs: Array[T], x: Int): T = select(xs, 0, xs.length - 1, x)
  def select[T : Ordering](xs: Array[T], low: Int, high: Int, x: Int): T = {
    // If there is only one element, this must be the target
    if (high == low) {
      xs(low)
    } else {
      val pivotIndex = RandomizedQuickSort.partition(xs, low, high)
      // Select which side to iterate to (or break recursion) by comparing
      // the pivot index with the desired index
      if (pivotIndex == x) {
        xs(pivotIndex)
      } else if (x < pivotIndex) {
        select(xs, low, pivotIndex - 1, x)
      } else {
        select(xs, pivotIndex + 1, high, x)
      }
    }
  }
}
