package com.supergrecko.dsa.linear

import scala.collection.mutable

class MaxHeap[T : Ordering] {
  import Ordering.Implicits._

  val data: mutable.ListBuffer[T] = mutable.ListBuffer()
  var size: Int = 0

  /** Get the left child node index of the node at the given index.
    *
    * This is done in constant time $$O(1)$$.
    */
  def left(idx: Int): Int = 2 * idx + 1

  /** Get the right child node index of the node at the given index.
    *
    * This is done in constant time $$O(1)$$.
    */
  def right(idx: Int): Int = 2 * idx + 2

  /** Get the parent node index of the node at the given index.
    *
    * This is done in constant time $$O(1)$$.
    */
  def parent(idx: Int): Int = (idx - 1) / 2

  /** Ensure the nodes in the subtree at the given index is properly ordered.
    *
    * This algorithm works by taking the given node and comparing itself with
    * its children and potentially swapping positions with it.
    *
    * Because a node swap might cause the next subtree to be wrong, it has to
    * call itself recursively down.
    *
    * Its time complexities are as follows:
    *
    * - Best case $$O(1)$$ in the case that the tree is empty or the node is a
    *   leaf node
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def heapify(idx: Int): Unit = {
    // If this is a leaf node, break the recursion
    val isLeaf = idx > (size / 2) && idx <= size
    if (isLeaf) {
      return
    }

    val l = left(idx)
    val r = right(idx)
    var m = if (l < size && data(l) > data(idx)) {
      l
    } else {
      idx
    }
    if (r < size && data(r) > data(m)) {
      m = r
    }

    if (m != idx) {
      // Swap the two items, call yourself recursively
      val tmp = data(idx)
      data(idx) = data(m)
      data(m) = tmp
      heapify(m)
    }
  }
}

object MaxHeap {
  /** Build a new MaxHeap from an array.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(n)$$, while it might seem like a $$O(n \lg n)$$ it is
    *   linear because of how the work in heapify is split up, work converges
    *   for the leaf nodes
    * - Average case of $$O(n)$$
    * - Worst case of $$O(n)$$
    */
  def build[T : Ordering](xs: Array[T]): MaxHeap[T] = {
    val heap = new MaxHeap()
    heap.size = xs.length
    heap.data.addAll(xs)
    for (i <- (xs.length / 2) to 0 by -1) {
      heap.heapify(i)
    }
    heap
  }
}
