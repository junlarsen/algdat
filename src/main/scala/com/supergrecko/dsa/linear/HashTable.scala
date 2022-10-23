package com.supergrecko.dsa.linear

class HashTable[K, V](val maxBuckets: Int) {
  /** A single entry in the hash table. The key is also kept here to make search
    * with collisions quicker as you only need to search for the key.
    */
  case class Entry(key: K, value: V)

  val data: Array[LinkedList[Entry]] = new Array[Entry](maxBuckets)
    .map(_ => new LinkedList[Entry])

  /** Hash the given key.
    *
    * This is done in constant time $$O(1)$$.
    */
  def hashKey(key: K): Int = {
    key.hashCode() % maxBuckets
  }

  /** Get the value for a given key in the hash table.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case there are no collisions on the key's
    *   hash.
    * - Average case of $$O(1)$$ since a good hash function will not cause
    *   collisions.
    * - Worst case of $$O(n)$$ if there are bad collisions on the hash.
    */
  def get(key: K): Option[V] = {
    val hash = hashKey(key)
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
    val hash = hashKey(key)
    val list = data(hash)
    val node = list.Node(Entry(key, value), None, None)
    list.prepend(node)
  }

  /** Delete a given key from the hash table.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ in the case there are no collisions on the key's
    * hash.
    * - Average case of $$O(1)$$ since a good hash function will not cause
    * collisions.
    * - Worst case of $$O(n)$$ if there are bad collisions on the hash.
    */
  def delete(key: K): Unit = {
    val hash = hashKey(key)
    val list = data(hash)
    list.search(x => x.get.key == key) match {
      case Some(node) => list.delete(node)
      case None =>
    }
  }
}
