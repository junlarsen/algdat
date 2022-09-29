package com.supergrecko.dsa.selection

import org.scalatest.funsuite.AnyFunSuite

class RandomizedSelectTest extends AnyFunSuite {
  test("it can find the nth smallest element in an array") {
    for (_ <- 0 to 100) {
      val xs = Array(10, 2, 6, 8, 92, 123, 832, 772, 9)
      val smallest = RandomizedSelect.select(xs, 0)
      assert(smallest == 2)
      val largest = RandomizedSelect.select(xs, 8)
      assert(largest == 832)
      val fifth = RandomizedSelect.select(xs, 4)
      assert(fifth == 10)
    }
  }
}
