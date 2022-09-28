package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class MergeSortTest extends AnyFunSuite {
  test("it can merge a subset of an array") {
    val xs = Array(1, 9, 3, 6, 8, 2)
    MergeSort.merge(xs, 0, 3, xs.length - 1)
    assert(xs sameElements Array(1, 8, 2, 9, 3, 6))
    MergeSort.merge(xs, 0, 1, 3)
    assert(xs sameElements Array(1, 2, 8, 9, 3, 6))
  }

  test("it can sort an array") {
    val xs = Array(4, 5, 9, 2, 123, 38, 92)
    MergeSort.sort(xs)
    assert(xs sameElements Array(2, 4, 5, 9, 38, 92, 123))
  }
}
