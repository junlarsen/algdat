package com.supergrecko.dsa.search

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTest extends AnyFunSuite {
  test("it will find items in odd-sized arrays") {
    val xs = Array(1, 2, 3, 4, 5)
    val four = BinarySearch.search(xs, 4)
    assert(four.contains(3))
    val none = BinarySearch.search(xs, 12)
    assert(none.isEmpty)
  }

  test("it will find items in even-sized arrays") {
    val xs = Array(1, 5, 9, 12, 43, 77)
    val twelve = BinarySearch.search(xs, 12)
    assert(twelve.contains(3))
    val none = BinarySearch.search(xs, 1238)
    assert(none.isEmpty)
  }
}
