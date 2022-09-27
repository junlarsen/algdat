package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class DynamicTableTest extends AnyFunSuite {
  test("it will automatically grow") {
    val arr = new DynamicTable[Int](4)
    arr.push(1)
    arr.push(1)
    arr.push(1)
    arr.push(1)
    assert(arr.capacity == 4)
    arr.push(1)
    assert(arr.capacity == 8)
  }
}
