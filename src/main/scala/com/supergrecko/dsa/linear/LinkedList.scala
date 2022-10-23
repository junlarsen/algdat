package com.supergrecko.dsa.linear

/** LinkedList is a linear data structure with arbitrary indexing.
  *
  * It works by connecting a set of nodes together by making each node point
  * to its neighbours.
  *
  * This is a doubly-linked list where each node has a previous and next element
  * pointer.
  */
class LinkedList[T] {
  case class Node(private val data: T, var next: Option[Node], var prev: Option[Node]) {
    def get: T = data

    /** Insert a given node in front of this
      *
      * This is done in constant time $$O(1)$$.
      */
    def insert(node: Node): Unit = {
      next = node.next
      prev = Some(node)
      // If the new node had a next value, connect
      node.next match {
        case Some(value) => value.prev = Some(this)
        case None => ()
      }
      node.next = Some(this)
    }
  }

  private var head: Option[Node] = None
  def get: Option[Node] = head

  /** Add a node to the front of the LinkedList
    *
    * This is done in constant time $$O(1)$$.
    */
  def prepend(node: Node): Unit = {
    node.next = head
    node.prev = None
    // If there is a value in the list, connect its backlink to the new node
    head match {
      case Some(value) => value.prev = Some(node)
      case None => ()
    }
    head = Some(node)
  }

  /** Delete the given node from the LinkedList
    *
    * This is done in constant time $$O(1)$$.
    */
  def delete(node: Node): Unit = {
    node.prev match {
      case Some(value) => value.next = node.next
      // The node has no previous element, meaning it is the list head
      case None => head = node.next
    }
    node.next match {
      case Some(value) => value.prev = node.prev
      case None => ()
    }
  }

  /** Search for a given value in the LinkedList
    *
    * This iterates through the nodes in the list until it finds a node whose
    * value is equal to the search value. In case of no match, None is returned.
    *
    * - Best case: $$O(1)$$ if the search element is the first in the list
    * - Worst case: $$O(n)$$ if the search element is not in the list
    * - Average case: $$O(n)$$
    */
  def search(value: T): Option[Node] = {
    var node = head
    while (node.isDefined && !node.map(_.get).contains(value)) {
      node = node.get.next
    }
    node
  }

  /** Search for a given value in the LinkedList that matches the predicate
    *
    * This implementation is almost identical to the original one, except it
    * matches the item against a predicate. It has the same time complexities
    * as the original search implementation.
    *
    * This function is not a part of the curriculum.
    */
  def search(fn: Node => Boolean): Option[Node] = {
    var node = head
    while (node.isDefined && !fn(node.get)) {
      node = node.get.next
    }
    node
  }
}
