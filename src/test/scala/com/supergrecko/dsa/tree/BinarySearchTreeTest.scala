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

  test("it can search through the binary tree for a given value") {
    val bst = new BinarySearchTree[Int]
    bst.insert(1)
    bst.insert(74)
    bst.insert(23)
    bst.insert(44)
    assert(bst.search(1).isDefined)
    assert(bst.search(43).isEmpty)
  }

  test("it can determine minimum and maximum values in a tree") {
    val bst = new BinarySearchTree[Int]
    bst.insert(0)
    bst.insert(-23)
    bst.insert(7443)
    assert(bst.max().isDefined)
    assert(bst.max().get.value == 7443)
    assert(bst.min().isDefined)
    assert(bst.min().get.value == -23)
  }
}
