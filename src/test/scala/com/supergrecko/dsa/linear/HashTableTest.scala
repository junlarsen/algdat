package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class HashTableTest extends AnyFunSuite {
  test("it can get and put entries") {
    val table = new HashTable[Int, Int](32)
    table.put(10, 100)
    val hundred = table.get(10)
    assert(hundred.contains(100))
  }

  test("it can handle duplicate keys") {
    val table = new HashTable[Int, Int](32)
    // should give the same hash
    table.put(10, 100)
    table.put(42, 200)

    assert(10.hashCode() % 32 == 42.hashCode() % 32)
    // hits a collision
    val twoHundred = table.get(42)
    assert(twoHundred.contains(200))
    val hundred = table.get(10)
    assert(hundred.contains(100))
  }
}
