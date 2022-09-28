package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class QuickSortTest extends AnyFunSuite {
  test("it can sort an array") {
    val xs = Array(12, 5, 98, 823, 182, 39, 2, 4, 57, 83)
    QuickSort.sort(xs)
    assert(xs sameElements Array(2, 4, 5, 12, 39, 57, 83, 98, 182, 823))
  }

  test("it can partition an array, placing the pivot in the right place") {
    val xs = Array(10, 4, 6, 8, 3, 1, 2, 5)
    QuickSort.partition(xs, 0, 7)
    assert(xs sameElements Array(4, 3, 1, 2, 5, 6, 8, 10))
  }

  test("it can swap two elements in array") {
    val xs = Array(10, 4, 8, 2)
    QuickSort.swap(xs, 0, 2)
    assert(xs sameElements Array(8, 4, 10, 2))
  }
}
