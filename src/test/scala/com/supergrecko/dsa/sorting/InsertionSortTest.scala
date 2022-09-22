package com.supergrecko.dsa.sorting

import org.scalatest.funsuite.AnyFunSuite

class InsertionSortTest extends AnyFunSuite {
  val cases: Seq[(Array[Int], Array[Int])] = Seq(
    (Array(1, 2, 3), Array(1, 2, 3)),
    (Array(), Array()),
    (Array(3, 2, 1), Array(1, 2, 3))
  )

  test("it sorts expected outputs") {
    for ((value, expected) <- cases) {
      InsertionSort.sort(value)
      assert(value sameElements expected)
    }
  }
}
