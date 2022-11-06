package com.supergrecko.dsa.tree

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class InorderWalkTest extends AnyFunSuite {
  test("it can traverse through each node in order") {
    val tree = new BinaryTree[Int]
    tree.root = Some(
      tree.Node(1, Some(
        tree.Node(2, Some(
          tree.Node(6, None, None, None)
        ), None, None)
      ), Some(
        tree.Node(4, None, None, None)
      ), None)
    )
    val xs = mutable.ArrayBuffer[Int]()
    InorderWalk.walk(tree, xs.addOne)
    assert(xs.toArray sameElements Array(6, 2, 1, 4))
  }
}
