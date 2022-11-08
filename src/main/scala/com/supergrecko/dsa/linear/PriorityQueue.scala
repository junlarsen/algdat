package com.supergrecko.dsa.linear

sealed trait PriorityQueueError
case object PriorityQueueUnderflow extends PriorityQueueError

class PriorityQueue[T : Ordering] extends MaxHeap[T] {
  /** Get the maximum value from the priority queue.
    *
    * This is done in constant time $$O(1)$$.
    */
  def max(): Either[PriorityQueueError, T] = {
    if (size < 1) {
      Left(PriorityQueueUnderflow)
    } else {
      // Max item is the first in the list
      Right(data.head)
    }
  }

  /** Extract the maximum value from the priority queue.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case $$O(1)$$ in the case that the queue is empty
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def extract(): Either[PriorityQueueError, T] = {
    val maximum = max() match {
      case Left(err) => return Left(err)
      case Right(value) => value
    }
    data(0) = data.last
    size -= 1
    heapify(0)
    Right(maximum)
  }
}
