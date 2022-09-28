package com.supergrecko.dsa.search

import scala.annotation.tailrec

/** BinarySearch is a divide & conquer search algorithm.
  *
  * It works on an already-sorted list, cutting the list in half for each
  * recursive step. It can do this because it knows that because the list is
  * sorted, the item we are looking for cannot be in the other half.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$\O(1)$$ in the case that the item we are searching for
  *   is the item in the middle of the array.
  * - Worst case of $$O(\lg n)$$ in the case that the item is not in the array.
  * - Average case of $$O(\lg n)$$ due to halving the array size each time.
  */
object BinarySearch {
  import Ordering.Implicits._
  def search[T : Ordering](xs: Array[T], x: T): Option[Int] = search(xs, 0, xs.length - 1, x)
  def search[T : Ordering](xs: Array[T], low: Int, high: Int, x: T): Option[Int] = {
    // If the cursors have not crossed each other, we can continue.
    if (low <= high) {
      // Split the list in half
      val middle = ((low + high) / 2).floor.toInt
      val target = xs(middle)
      // If this is the item we are looking for, return it
      if (target == x) {
        Some(middle)
      } else if (x < target) {
        // Recurse, cutting off top half
        search(xs, low, middle - 1, x)
      } else {
        // Recurse, cutting off bottom half
        search(xs, middle + 1, high, x)
      }
    } else {
      None
    }
  }
}
