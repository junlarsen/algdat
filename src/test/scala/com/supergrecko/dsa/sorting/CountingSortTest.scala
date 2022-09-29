package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class CountingSortTest extends AnyFunSuite {
  test("it can sort an array") {
    val xs = Array(1, 49, 8, 234, 7, 5, 32, 11)
    val res = CountingSort.sort(xs)
    assert(res sameElements xs.sorted)
  }
}
