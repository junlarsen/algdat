package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class MaxHeapTest extends AnyFunSuite {
  test("it can build a heap from an array") {
    val xs = Array(1, 5, 3, 2, 4)
    val heap = MaxHeap.build(xs)
    assert(heap.data == Seq(5, 4, 3, 2, 1).toBuffer)
  }
}
