package com.googlecode.pegsolitaire

/**
 * A memory-efficient hash set optimized for Longs
 * based on the java HashSet implementation by Google Inc.
 */
class MemoryEfficientLongHashSet extends LongHashSet {

	// contains all numbers until 0xffffffff
	private val int_hash_set_lower = new IntHashSet
	// contains all numbers until 0x1ffffffff
	private val int_hash_set_higher = new IntHashSet
	// contains all numbers above 0x1ffffffff
	private val long_hash_set = new StandardLongHashSet

	private var contains_bit_33_element = false

	override def size = int_hash_set_lower.size + int_hash_set_higher.size + long_hash_set.size

	override def used = (int_hash_set_lower.used + int_hash_set_higher.used + long_hash_set.used) / 3.0

	private final def convertIntToLong(i: Int): Long = if(i<0) (1L<<31 | (i & 0x7fffffff).asInstanceOf[Long]) else i.asInstanceOf[Long]

	/**
	 * positions can be used to create a HashSetIterator that only work on a subset of the HashSet
	 * e.g. to read multiple elements from a HashSet at a time without synchronization
	 */
	class Iterator(val groupID: Int = 0, val groupSize: Int = 1) extends HashSetIterator {
		require(groupID >= 0)
		require(groupSize > 0)
		require(groupID < groupSize)

		private val int_hash_set_lower_iterator = int_hash_set_lower.iter(groupID, groupSize)
		private val int_hash_set_higher_iterator = int_hash_set_higher.iter(groupID, groupSize)
		private val long_hash_set_iterator = long_hash_set.iter(groupID, groupSize)

		override def hasNext = int_hash_set_lower_iterator.hasNext || int_hash_set_higher_iterator.hasNext || long_hash_set_iterator.hasNext

		override def unsafe_next: Long = {
			if(int_hash_set_lower_iterator.hasNext)
				convertIntToLong(int_hash_set_lower_iterator.unsafe_next)
			else if(int_hash_set_higher_iterator.hasNext)
				convertIntToLong(int_hash_set_higher_iterator.unsafe_next) | 0x100000000L
			else if(contains_bit_33_element)
				0x100000000L
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
			int_hash_set_lower += other.int_hash_set_lower
			int_hash_set_higher += other.int_hash_set_higher
			long_hash_set += other.long_hash_set
			contains_bit_33_element = contains_bit_33_element || other.contains_bit_33_element
		} else {
			c.foreach(this += _)
		}
	}

	override def +=(o: Long) {
		if(o <= 0xffffffffL)
			int_hash_set_lower += o.asInstanceOf[Int]
		else if(o <= 0x1ffffffffL) {
			if(o == 0x100000000L)
				contains_bit_33_element = true
			else
				int_hash_set_higher += (o & 0xffffffffL).asInstanceOf[Int]
		} else
			long_hash_set += o
	}

	override def contains(o: Long) = {
		if(o <= 0xffffffffL)
			int_hash_set_lower.contains(o.asInstanceOf[Int])
		else if(o <= 0x1ffffffffL) {
			if(o == 0x100000000L)
				contains_bit_33_element
			else
				int_hash_set_higher.contains((o & 0xffffffffL).asInstanceOf[Int])
		} else
			long_hash_set.contains(o)
	}

	override def clear() {
		int_hash_set_lower.clear()
		int_hash_set_higher.clear()
		long_hash_set.clear()
	}

	override def clear(new_expected_size: Int) {
		clear()
	}

	override def iter: HashSetIterator = new Iterator
	override def iter(groupID: Int, groupSize: Int): HashSetIterator = new Iterator(groupID, groupSize)

	class MemoryEfficientLongHashSetDepth extends HashSetDepthTrait {
		override def toString = "int_hash_set_lower :" + int_hash_set_lower.depth + "\n" +
								"int_hash_set_higher:" + int_hash_set_higher.depth + "\n" +
								"long_hash_set      :" + long_hash_set.depth
	}

	override def depth: HashSetDepthTrait = new MemoryEfficientLongHashSetDepth
}
