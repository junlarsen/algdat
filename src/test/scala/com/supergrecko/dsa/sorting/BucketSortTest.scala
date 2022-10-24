package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class BucketSortTest extends AnyFunSuite {
  test("it can sort an array of floating point numbers") {
    val xs = Array(
      0.2f, 0.23f, 0.123f, 0.94f, 0.3f, 0.45f, 0.566f, 0.61f, 0.7f, 0.83f, 0.0f,
      0.43f, 0.75f, 0.999f, 0.8327f, 0.4f, 0.66f
    )
    val res = BucketSort.sort(xs)
    assert(res sameElements xs.sorted)
  }
}
