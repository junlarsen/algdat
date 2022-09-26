package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class LinkedListTest extends AnyFunSuite {
  test("it can prepend nodes to the front of the list") {
    val ls = new LinkedList[Int]
    val n1 = ls.Node(100, None, None)
    val n2 = ls.Node(100, None, None)
    assert(ls.get.isEmpty)
    ls.prepend(n1)
    assert(ls.get.contains(n1))
    ls.prepend(n2)
    assert(ls.get.contains(n2))
  }

  test("it can search through the list of nodes") {
    val ls = new LinkedList[Int]
    val n1 = ls.Node(1, None, None)
    val n2 = ls.Node(2, None, None)
    val n3 = ls.Node(3, None, None)
    ls.prepend(n1)
    ls.prepend(n2)
    ls.prepend(n3)
    val missing = ls.search(100)
    assert(missing.isEmpty)
    val last = ls.search(3)
    assert(last.contains(n3))
  }

  test("it can delete an item from the node") {
    val ls = new LinkedList[Int]
    val n1 = ls.Node(1, None, None)
    val n2 = ls.Node(2, None, None)
    val n3 = ls.Node(3, None, None)
    ls.prepend(n1)
    ls.prepend(n2)
    ls.prepend(n3)
    assert(ls.get.get.next.contains(n2))
    ls.delete(n2)
    val missing = ls.search(2)
    assert(missing.isEmpty)
    assert(ls.get.get.next.contains(n1))
  }

  test("it allows nodes to place without interaction with the list") {
    val ls = new LinkedList[Int]
    val n1 = ls.Node(1, None, None)
    val n2 = ls.Node(2, None, None)
    val n3 = ls.Node(3, None, None)
    ls.prepend(n1)
    n1.insert(n2)
    ls.prepend(n3)
    // n2 should be in front (closer to head) than n1 now
    assert(ls.get.get.next.contains(n1))
  }
}
