package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class QueueTest extends AnyFunSuite {
  test("it can queue and dequeue items") {
    val queue = new Queue[Int](1)
    assert(queue.isEmpty)
    val maybeEnqueueError = queue.enqueue(5)
    assert(queue.isFull)
    assert(maybeEnqueueError.isRight)
    val maybeDequeueError = queue.dequeue()
    assert(maybeDequeueError.contains(5))
  }

  test("it will overflow at max items") {
    val queue = new Queue[Int](3)
    queue.enqueue(1)
    queue.enqueue(1)
    queue.enqueue(1)
    assert(queue.isFull)
    val overflow = queue.enqueue(1)
    assert(overflow.swap.contains(QueueOverflow))
  }

  test("it will underflow with zero items") {
    val queue = new Queue[Int](3)
    assert(queue.isEmpty)
    val underflow = queue.dequeue()
    assert(underflow.swap.contains(QueueUnderflow))
  }
}
