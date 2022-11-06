package com.supergrecko.dsa.tree

/** Binary Search Tree is a special kind of binary tree where all nodes on the
  * left of a given node hold a value lower than the current node, and all nodes
  * on the right hold a higher value.
  *
  * This implementation does not support duplicates in the binary tree.
  */
class BinarySearchTree[T : Ordering] extends BinaryTree[T] {
  import Ordering.Implicits._

  /** Insert a new value into the binary tree.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case that it's the first node in the tree
    * - Average case of $$O(\lg n)$$ because it divides the tree in half each
    *   iteration.
    * - Worst case of $$O(\lg n)$$ for the same reason.
    */
  def insert(value: T): Node = insert(root, value)
  def insert(root: Option[Node], value: T): Node = {
    var parent = root
    val z = Node(value, None, None, None)
    var target: Option[Node] = None
    while (parent.isDefined) {
      target = parent
      // Safe access, because parent.isDefined is true. Here we determine which
      // side of the parent node to insert the new value.
      if (value < parent.get.value) {
        parent = parent.get.left
      } else {
        parent = parent.get.right
      }
    }
    // Give the new node a parent
    z.parent = target
    target match {
      // The target node exists, determine which side of the search tree to
      // insert the node at
      case Some(node) => if (value < node.value) {
        node.left = Some(z)
      } else {
        node.right = Some(z)
      }
      // If the target node is still None, it means the tree is empty, and
      // that the new node should be placed at the root.
      case None => this.root = Some(z)
    }
    z
  }

  /** Search through the binary search tree for a given value.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case that the tree is empty, or the root
    *   node of the tree is the search key.
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def search(key: T): Option[Node] = search(root, key)
  def search(node: Option[Node], key: T): Option[Node] = {
    node match {
      case Some(n) => if (n.value == key) {
        node
      } else if (n.value > key) {
        search(n.right, key)
      } else {
        search(n.left, key)
      }
      case None => None
    }
  }

  /** Get the node with the lowest value from the tree.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case that the tree is empty.
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def max(): Option[Node] = max(root)
  def max(start: Option[Node]): Option[Node] = {
    var node = start
    while (node.isDefined && node.get.right.isDefined) {
      node = node.get.right
    }
    node
  }

  /** Get the node with the largest value from the tree.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case that the tree is empty.
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def min(): Option[Node] = min(root)
  def min(start: Option[Node]): Option[Node] = {
    var node = start
    while (node.isDefined && node.get.left.isDefined) {
      node = node.get.left
    }
    node
  }

  /** Find the node with the smallest key greater than the given node's key.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(\lg n)$$
    * - Average case of $$O(\lg n)$$
    * - Worst case of $$O(\lg n)$$
    */
  def successor(node: Node): Option[Node] = {
    node.right match {
      case Some(_) => min(node.right)
      case None =>
        var start = node
        var target = node.parent
        while (target.isDefined && target.get.right.contains(start)) {
          start = target.get
          target = target.get.parent
        }
        target
    }
  }
}
