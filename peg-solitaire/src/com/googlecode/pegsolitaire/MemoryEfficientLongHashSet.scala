package com.googlecode.pegsolitaire

object MemoryEfficientLongHashSet {
}

/**
 * A memory-efficient hash set optimized for Longs
 * based on the java HashSet implementation by Google Inc.
 */
class MemoryEfficientLongHashSet extends LongHashSet {

	private val int_hash_set = new IntHashSet
	private val long_hash_set = new StandardLongHashSet

	override def size = int_hash_set.size + long_hash_set.size

	override def used = (int_hash_set.used + long_hash_set.used) / 2.0

	
	/**
	 * positions can be used to create a HashSetIterator that only work on a subset of the HashSet
	 * e.g. to read multiple elements from a HashSet at a time without synchronization
	 */
	class Iterator(val groupID: Int = 0, val groupSize: Int = 1) extends HashSetIterator {
		require(groupID >= 0)
		require(groupSize > 0)
		require(groupID < groupSize)

		private val int_hash_set_iterator = int_hash_set.iter(groupID, groupSize)
		private val long_hash_set_iterator = long_hash_set.iter(groupID, groupSize)

		override def hasNext = int_hash_set_iterator.hasNext || long_hash_set_iterator.hasNext

		override def unsafe_next: Long = {
			if(int_hash_set_iterator.hasNext)
				int_hash_set_iterator.unsafe_next
			else
				long_hash_set_iterator.unsafe_next
		}
	}
	
	def this(expectedSize: Int) = this()

	def this(c: LongHashSet) {
		this()
		this += c
	}

	override def +=(c: LongHashSet) {
		if(c.isInstanceOf[MemoryEfficientLongHashSet]) {
			val other = c.asInstanceOf[MemoryEfficientLongHashSet]
			int_hash_set += other.int_hash_set
			long_hash_set += other.long_hash_set
		} else {
			c.foreach(this += _)
		}
	}

	override def +=(e: Long) {
		if(e <= Int.MaxValue)
			int_hash_set += e.asInstanceOf[Int]
		else
			long_hash_set += e
	}

	override def clear() {
		int_hash_set.clear()
		long_hash_set.clear()
	}

	override def clear(new_expected_size: Int) {
		clear()
	}

	override def contains(o: Long) = {
		if(o <= Int.MaxValue)
			int_hash_set.contains(o.asInstanceOf[Int])
		else
			long_hash_set.contains(o)
	}

	override def iter: HashSetIterator = new Iterator
	override def iter(groupID: Int, groupSize: Int): HashSetIterator = new Iterator(groupID, groupSize)

	override def ensureSizeFor(expectedSize: Int) {}

	override def depth: HashSetDepth = {
		HashSetDepth(0,0,0)
		//long_hash_set.depth
	}
}
