package com.supergrecko.dsa.linear

class HashTable[K, V](val maxBuckets: Int) {
  /** A single entry in the hash table. The key is also kept here to make search
    * with collisions quicker as you only need to search for the key.
    */
  case class Entry(key: K, value: V)

  val data: Array[LinkedList[Entry]] = new Array[Entry](maxBuckets)
    .map(_ => new LinkedList[Entry])

  /** Get the value for a given key in the hash table.
    *
    * This performs in best case (no bucket collisions) in constant time, but
    * may take up to n collisions in worst case.
    */
  def get(key: K): Option[V] = {
    val hash = key.hashCode() % maxBuckets
    val list = data(hash)
    // In best case, this is constant time. Worst case you have to move through
    // the list until you find it
    list.search(_.get.key == key).map(_.get.value)
  }

  /** Insert a value for a given key into the hash table.
    *
    * This is done in constant time $$O(1)$$
    */
  def put(key: K, value: V): Unit = {
    val hash = key.hashCode() % maxBuckets
    val list = data(hash)
    val node = list.Node(Entry(key, value), None, None)
    list.prepend(node)
  }
}
