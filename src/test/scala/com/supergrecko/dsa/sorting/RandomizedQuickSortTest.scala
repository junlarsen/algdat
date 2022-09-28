package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class RandomizedQuickSortTest extends AnyFunSuite {
  test("it can sort an array") {
    // Run over a distributed amount of runs because it has random parts.
    for (_ <- 0 to 100) {
      val xs = Array(12, 5, 98, 823, 182, 39, 2, 4, 57, 83)
      RandomizedQuickSort.sort(xs)
      assert(xs sameElements Array(2, 4, 5, 12, 39, 57, 83, 98, 182, 823))
    }
  }
}
