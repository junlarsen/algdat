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
    *
    */
  def insert(value: T): Unit = {
    var parent = root
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
    // This implementation of a binary tree does not keep a pointer to its
    // parent, so the `z.p = y` step of the CLRS pseudocode is omitted.
    target match {
      // The target node exists, determine which side of the search tree to
      // insert the node at
      case Some(node) => if (value < node.value) {
        node.left = Some(Node(value, None, None))
      } else {
        node.right = Some(Node(value, None, None))
      }
      // If the target node is still None, it means the tree is empty, and
      // that the new node should be placed at the root.
      case None => root = Some(Node(value, None, None))
    }
  }
}
