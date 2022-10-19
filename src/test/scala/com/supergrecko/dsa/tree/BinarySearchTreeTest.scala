package com.supergrecko.dsa.tree

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTreeTest extends AnyFunSuite {
  test("it can insert nodes in the right place into the binary tree") {
    val bst = new BinarySearchTree[Int]
    bst.insert(12)
    // First item placed at root
    assert(bst.root.contains(bst.Node(12, None, None)))
    bst.insert(6)
    // Lower item should be on the left
    assert(bst.root.get.left.contains(bst.Node(6, None, None)))
    bst.insert(62)
    // Higher item should be on the right
    assert(bst.root.get.right.contains(bst.Node(62, None, None)))
    bst.insert(32)
    // Higher than first, yet lower than right should land right-left
    assert(bst.root.get.right.get.left.contains(bst.Node(32, None, None)))
  }
}
