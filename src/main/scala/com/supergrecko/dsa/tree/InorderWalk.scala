package com.supergrecko.dsa.tree

/** InorderWalk is a tree-traversal algorithm, in this case, operating on a
  * binary tree in this case.
  *
  * It walks through the tree, recursing through each node in the tree. This
  * implementation is left-favored, forming a strategy similar to depth-first
  * search.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case: $$O(n)$$ because you visit each node.
  * - Average case: $$O(n)$$ for the same reason.
  * - Worst case: $$O(n)$$ for the same reason.
  */
object InorderWalk {
  def walk[T](tree: BinaryTree[T], effect: T => Unit): Unit = {
    def walk(node: Option[tree.Node], effect: T => Unit): Unit = {
      node match {
        case Some(inner) =>
          walk(inner.left, effect)
          effect(inner.value)
          walk(inner.right, effect)
        case None =>
      }
    }
    walk(tree.root, effect)
  }
}
