package com.supergrecko.dsa.tree

class BinaryTree[T] {
  case class Node(value: T, left: Option[Node], right: Option[Node])
  var root: Option[Node] = None
}
