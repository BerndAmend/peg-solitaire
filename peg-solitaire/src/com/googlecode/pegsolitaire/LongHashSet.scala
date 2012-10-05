/**
 * Peg Solitaire Solver
 * Copyright (C) 2010-2012 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 *
 * based on the java implementation by Google Inc.
 *
 * Copyright 2009 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package com.googlecode.pegsolitaire

import Helper._

final object LongHashSet {
	/**
	 * In the interest of memory-savings, we start with the smallest feasible
	 * power-of-two table size that can hold three items without rehashing. If we
	 * started with a size of 2, we'd have to expand as soon as the second item
	 * was added.
	 */
	final val INITIAL_TABLE_SIZE = 1 << 5

	final val INVALID_ELEMENT = Long.MinValue

	final def allocateTableMemory(size: Int) = {
		val r = new Array[Long](size)
		java.util.Arrays.fill(r, INVALID_ELEMENT)
		r
	}
}

/**
 * A memory-efficient hash set optimized for Longs
 * based on the java HashSet implementation by Google Inc.
 */
final class LongHashSet(t: Array[Long], s: Int) { // extends scala.collection.Iterable[Long] {

	protected var _size = s

	/**
	 * number of elements inside the set
	 */
	def size = _size

	/**
	 * current fill state in percent
	 */
	def used = if (table == null) 1.0 else (_size.toDouble / table.length.toDouble)

	private var table = t
	private var table_length_minus_1 = if (t == null) 0 else (t.length - 1)

	/**
	 * positions can be used to create a HashSetIterator that only work on a subset of the HashSet
	 * e.g. to read multiple elements from a HashSet at a time without synchronization
	 */
	final class HashSetIterator(val groupID: Int = 0, val groupSize: Int = 1) {
		require(groupID >= 0)
		require(groupSize > 0)
		require(groupID < groupSize)

		private var _index = groupID
		private val table_length = table.length

		advanceToItem()

		final def hasNext = _index < table_length

		final def next: Long = {
			if (!hasNext)
				throw new java.util.NoSuchElementException()

			unsafe_next
		}

		/**
		 * call this function ONLY if you really know what you are doing
		 */
		final def unsafe_next: Long = {
			val toReturn = table(_index)
			_index += groupSize
			advanceToItem()
			toReturn
		}

		private final def advanceToItem() {
			while (_index < table_length && (table(_index) == LongHashSet.INVALID_ELEMENT)) {
				_index += groupSize
			}
		}
	}

	def this() = this(LongHashSet.allocateTableMemory(LongHashSet.INITIAL_TABLE_SIZE), 0)

	def this(expectedSize: Int) {
		this(null, 0)
		require(expectedSize >= 0)
		ensureSizeFor(expectedSize)
	}

	def this(c: LongHashSet) {
		this(c.size)
		this += c
	}

	final def isEmpty = _size == 0

	final def +=(c: LongHashSet) {
		ensureSizeFor(_size + c.size)
		internal_addAll(c.table)
	}

	/**
	 * Works just like    { @link # addAll ( Collection ) }, but for arrays. Used to avoid
	 * having to synthesize a collection in    { @link Sets }.
	 * ignores INVALID_ELEMENT entries in the Array
	 */
	final def +=(elements: Array[Long]) {
		ensureSizeFor(_size + elements.length)
		internal_addAll(elements)
	}

	final def +=(e: Long) {
		require(e != LongHashSet.INVALID_ELEMENT)
		ensureSizeFor(size + 1)
		internal_add(e)
	}

	// add the elements without checking if there is enough space
	private final def internal_addAll(elements: Array[Long]) {
		val length = elements.length
		var i = 0
		while (i < length) {
			if (elements(i) != LongHashSet.INVALID_ELEMENT)
				internal_add(elements(i))
			i += 1
		}
	}

	// adds the element without checking if there is enough space or if e is invalid
	private final def internal_add(e: Long) {
		val index = findOrEmpty(e)
		if (table(index) == LongHashSet.INVALID_ELEMENT) {
			_size += 1
			table(index) = e
		}
	}

	final def clear() {
		java.util.Arrays.fill(table, LongHashSet.INVALID_ELEMENT)
		_size = 0
	}

	final def clear_and_free() {
		table = LongHashSet.allocateTableMemory(LongHashSet.INITIAL_TABLE_SIZE)
		table_length_minus_1 = table.length - 1
		_size = 0
	}

	final def contains(o: Long) = table(findOrEmpty(o)) != LongHashSet.INVALID_ELEMENT

	final def iterator = new HashSetIterator
	final def iterator(groupID: Int, groupSize: Int) = new HashSetIterator(groupID, groupSize)

	/**
	 * Ensures the set is large enough to contain the specified number of entries.
	 */
	private final def ensureSizeFor(expectedSize: Int) {
		if (table != null && table.length * 3 >= expectedSize * 4)
			return

		// calculate table size
		var newCapacity = if (table == null) 1 else table.length
		while (newCapacity * 3 < expectedSize * 4)
			newCapacity <<= 1

		val old_table = table
		val old_size = _size
		table = LongHashSet.allocateTableMemory(newCapacity)
		table_length_minus_1 = table.length - 1
		_size = 0
		if (old_size != 0)
			internal_addAll(old_table)
		require(_size == old_size)
	}

	/**
	 * Returns the index in the table at which a particular item resides, or -1 if
	 * the item is not in the table.
	 */
	private final def find(o: Long): Int = {
		val index = findOrEmpty(o)
		if (table(index) == LongHashSet.INVALID_ELEMENT)
			-1
		else
			index
	}

	/**
	 * Returns the index in the table at which a particular item resides, or the
	 * index of an empty slot in the table where this item should be inserted if
	 * it is not already in the table.
	 * @return index
	 */
	private final def findOrEmpty(o: Long): Int = {
		@scala.annotation.tailrec
		def loop(index: Int): Int = {
			val existing = table(index)
			if (existing == LongHashSet.INVALID_ELEMENT || o == existing)
				index
			else
				loop((index+1) % table.length)
		}

		loop(getIndex(o))
	}

	private final def getIndex(value: Long): Int = {
		var h = (value ^ (value >>> 32)).asInstanceOf[Int] // hashCode
		// Copied from Apache's AbstractHashedMap; prevents power-of-two collisions.
		h += ~(h << 9)
		h ^= (h >>> 14)
		h += (h << 4)
		h ^= (h >>> 10)
		// Power of two trick.
		h & table_length_minus_1
	}

	final def foreach(func: Long => Unit) {
		val iter = iterator

		while (iter.hasNext) {
			func(iter.unsafe_next)
		}
	}

	final def toList = {
		var r = List[Long]()
		val iter = iterator

		while (iter.hasNext) {
			r ::= iter.unsafe_next
		}
		r
	}

	case class Depth(average: Double, max: Int, oneAccessPercent: Double) {
		override def toString = "(HashSet accessDepth average=" + average + " max=" + max + " 1 access required=" + oneAccessPercent + ")"
	}

	/**
	 * @return the search deep required to access elements (average, max, oneAccessPercent)
	 */
	def depth: Depth = {
		var averageValue = BigDecimal(0)
		var maxValue = 0
		var oneAccessElements = 0

		var index = 0
		val table_length = table.length
		while (index < table_length) {
			val v = table(index)
			val designated_index = getIndex(v) // where the element should be

			if (v != LongHashSet.INVALID_ELEMENT) {
				var d = 1
				if (index == designated_index)
					oneAccessElements += 1
				else if (designated_index < index) {
					d += index - designated_index
				} else {
					d += index + (table_length - designated_index)
				}

				maxValue = math.max(maxValue, d)
				averageValue += d
			}
			index += 1
		}

		Depth((averageValue / size).toDouble, maxValue, oneAccessElements.toDouble / size.toDouble)
	}

	def bitDistribution: Array[Long] = {
		val iter = iterator
		val result = Array.fill[Long](64)(0L)

		while (iter.hasNext) {
			val elem = iter.unsafe_next

			var i = 0
			while (i < 64) {
				if ((elem & (1L << i)) != 0)
					result(i) += 1
				i += 1
			}
		}

		result
	}

	def bitDistributionString: String = {
		val bd = bitDistribution
		val max = bd.max.asInstanceOf[Double]

		val r = new StringBuilder
		r append "bd:"

		for (i <- 0 until bd.length) {
			if (bd(i) != 0) {
				r append " "
				r append i
				r append ":"
				r append "%3f".format(bd(i).asInstanceOf[Double] / max)
			}
		}

		r.result
	}
}

