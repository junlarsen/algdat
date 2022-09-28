package com.supergrecko.dsa.linear

import scala.reflect.ClassTag

sealed trait QueueError
case object QueueOverflow extends QueueError
case object QueueUnderflow extends QueueError

/** Queue is a linear data structure without arbitrary indexing. It is a
  * first-in-first-out data structure.
  *
  * Every operation on the queue is done in constant time.
  *
  * This is a queue implemented as a RingBuffer. It has a vulnerability with
  * integer overflow where Queue.size will grow to infinitely large, but this
  * is a sufficient implementation for demonstration.
  */
class Queue[T : ClassTag](var size: Int) {
  val data: Array[T] = new Array[T](size)
  /** Reading offset, may grow beyond size, but reads perform modulo to access */
  var read: Int = 0
  /** Writing offset, may grow beyond size, but writes perform modulo to access */
  var write: Int = -1

  /** Determine if the Queue is full
    *
    * This is determined by calculating the difference between the write and
    * read offsets. The +1 is added to make up for zero-indexing into the array.
    */
  def isFull: Boolean = (write - read) + 1 == size

  /** Determine if the Queue is empty
    *
    * This is determined by checking if the write pointer is positioned behind
    * the reading pointer. This works because write is default to -1.
    */
  def isEmpty: Boolean = write < read

  /** Add an item to the Queue.
    *
    * This is done in constant time $$O(1)$$
    */
  def enqueue(value: T): Either[QueueError, Unit] = {
    if (isFull) {
      Left(QueueOverflow)
    } else {
      write += 1
      // Modulus because size grows infinitely large
      data(write % size) = value
      Right()
    }
  }

  /** Remove an item from the Queue.
    *
    * This is done in contant time $$O(1)$$
    */
  def dequeue(): Either[QueueError, T] = {
    if (isEmpty) {
      Left(QueueUnderflow)
    } else {
      // Modulus because size grows infinitely large
      val elem = data(read % size)
      read += 1
      Right(elem)
    }
  }
}
