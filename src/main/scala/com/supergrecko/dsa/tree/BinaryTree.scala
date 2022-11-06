package com.supergrecko.dsa.tree

class BinaryTree[T] {
  case class Node(value: T, var left: Option[Node], var right: Option[Node], var parent: Option[Node])
  var root: Option[Node] = None
}
