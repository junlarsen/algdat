package com.supergrecko.dsa.tree

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTreeTest extends AnyFunSuite {
  test("it can insert nodes in the right place into the binary tree") {
    val bst = new BinarySearchTree[Int]
    bst.insert(12)
    // First item placed at root
    assert(bst.root.get.value == 12)
    bst.insert(6)
    // Lower item should be on the left
    assert(bst.root.get.left.get.value == 6)
    bst.insert(62)
    // Higher item should be on the right
    assert(bst.root.get.right.get.value == 62)
    bst.insert(32)
    // Higher than first, yet lower than right should land right-left
    assert(bst.root.get.right.get.left.get.value == 32)
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

    val empty = new BinarySearchTree[Int]
    assert(empty.max().isEmpty)
    assert(empty.min().isEmpty)
  }

  test("it can find the successor of a node") {
    val bst = new BinarySearchTree[Int]
    bst.insert(1)
    val intermediary = bst.insert(2)
    bst.insert(3)
    val succ = bst.successor(intermediary)
    assert(succ.get.value == 3)

    val empty = new BinarySearchTree[Int]
    val root = empty.insert(1)
    assert(empty.successor(root).isEmpty)
  }
}
