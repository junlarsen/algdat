package com.supergrecko.dsa.sorting

import scala.collection.mutable

/** BucketSort is a sorting algorithm.
  *
  * It is stable if the underlying sorting algorithm used is stable.
  *
  * BucketSort works by assuming that the input is uniformly distributed across
  * the range [0, 1), splitting the input into buckets and sorting each bucket.
  *
  * Because there should be few items in each bucket, there should be a rough
  * estimate of runtime of $$O(n)$$.
  *
  * Its time complexity properties are as follows:
  *
  * - Best case of $$O(n)$$
  * - Average case of $$O(n)$$
  * - Worst case of $$O(n^2^)$$
  */
object BucketSort {
  def sort(xs: Array[Float]): Array[Float] = sort(xs, xs.length)
  def sort(xs: Array[Float], size: Int): Array[Float] = {
    val buckets = new Array[mutable.ArrayBuffer[Float]](size).map(_ => mutable.ArrayBuffer[Float]())
    // For each number, add it to the correct bucket
    for (i <- 0 until size) {
      val index = xs(i) * size
      val bucket = buckets(index.toInt)
      bucket.addOne(xs(i))
    }

    // Sort each of the lists, in this case, using Scala collections sort
    val res = for (bucket <- buckets.iterator) yield bucket.sorted
    res.flatten.toArray
  }
}
