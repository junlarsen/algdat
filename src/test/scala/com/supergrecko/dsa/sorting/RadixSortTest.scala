package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class RadixSortTest extends AnyFunSuite {
  test("it can sort an array") {
    val xs = Array(1, 8232, 993, 92, 776, 0, 3232, 4444)
    val clone = xs.sorted
    RadixSort.sort(xs)
    assert(xs sameElements clone)
  }
}
