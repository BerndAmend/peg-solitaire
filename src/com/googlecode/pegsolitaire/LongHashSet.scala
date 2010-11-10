/**
 * Peg Solitaire Solver  Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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
import java.util.{Collection, Iterator, NoSuchElementException}

/**
 * A memory-efficient hash set optimized for Longs
 * based on the java HashSet implementation by Google Inc.
 */
final class LongHashSet {

	/**
	 * In the interest of memory-savings, we start with the smallest feasible
	 * power-of-two table size that can hold three items without rehashing. If we
	 * started with a size of 2, we'd have to expand as soon as the second item
	 * was added.
	 */
	private val INITIAL_TABLE_SIZE = 4

	final val INVALID_ELEMENT = java.lang.Long.MIN_VALUE

	/**
	 * Number of objects in this set; transient due to custom serialization.
	 * Default access to avoid synthetic accessors from inner classes.
	 */
	protected var _size = 0

	def size = _size

	def used = _size.toDouble / table.length.toDouble

	/**
	 * Backing store for all the objects; transient due to custom serialization.
	 * Default access to avoid synthetic accessors from inner classes.
	 */
	protected var table = Array.fill[Long](INITIAL_TABLE_SIZE)(INVALID_ELEMENT)

	class HashSetIterator {
		private var index = 0
		private[LongHashSet] var last = -1

		advanceToItem()

		def hasNext = index < table.length

		def next = {
			if (!hasNext)
				throw new NoSuchElementException()

			last = index
			val toReturn = table(index)
			index += 1
			advanceToItem()
			toReturn
		}

		def remove() {
			if (last < 0)
				throw new IllegalStateException()

			internalRemove(last)
			if (table(last) != INVALID_ELEMENT)
				index = last

			last = -1
		}

		private def advanceToItem() {
			while (index < table.length) {
				if (table(index) != INVALID_ELEMENT)
					return
				index += 1
			}
		}
	}

	def this(expectedSize: Int) {
		this ()
		ensureSizeFor(expectedSize)
	}

	def this(c: Iterable[Long]) {
		this ()
		addAll(c)
	}

	def this(c: LongHashSet) {
		this ()
		addAll(c)
	}

	def this(c: Collection[Long]) {
		this ()
		addAll(c)
	}

	def +=(c: Iterable[Long]) = addAll(c)

	def addAll(c: Iterable[Long]) = {
		ensureSizeFor(_size + c.size)
		for (e <- c)
			add(e)
	}

	def +=(c: LongHashSet) = addAll(c)

	def addAll(c: LongHashSet) = {
		ensureSizeFor(_size + c.size)
		val citer = c.iterator
		while (citer.hasNext)
			add(citer.next)
	}

	def +=(c: Collection[Long]) = addAll(c)

	def addAll(c: Collection[Long]) = {
		ensureSizeFor(_size + c.size())
		val citer = c.iterator
		while (citer.hasNext)
			add(citer.next)
	}

	/**
	 * Works just like    { @link # addAll ( Collection ) }, but for arrays. Used to avoid
	 * having to synthesize a collection in    { @link Sets }.
	 */
	def addAll(elements: Array[Long]) {
		ensureSizeFor(_size + elements.length)
		for (e <- elements) {
			var index = findOrEmpty(e)
			if (table(index) == INVALID_ELEMENT) {
				_size += 1
				table(index) = e
			}
		}
	}

	def +=(e: Long) = add(e)

	def add(e: Long) = {
		ensureSizeFor(size + 1)
		val index = findOrEmpty(e)
		if (table(index) == INVALID_ELEMENT) {
			_size += 1
			table(index) = e
			true
		} else
			false
	}

	def shrink {
		var newCapacity = INITIAL_TABLE_SIZE;
		while (newCapacity * 3 < _size * 4)
			newCapacity <<= 1
		// shrink only if required

		if (table.length <= newCapacity)
			return

		val newHashSet = new LongHashSet(this)

		//println("LongHashSet: fillState before=" + used + " after=" + newHashSet.used)

		table = newHashSet.table
		_size = newHashSet._size
	}

	def clear {
		table = Array.fill[Long](INITIAL_TABLE_SIZE)(INVALID_ELEMENT)
		_size = 0
	}

	def contains(o: Long) = find(o) >= 0

	def iterator = new HashSetIterator

	def remove(o: Long): Boolean = {
		var index = find(o)
		if (index < 0)
			return false
		internalRemove(index)
		true
	}

	/**
	 * Removes the item at the specified index, and performs internal management
	 * to make sure we don't wind up with a hole in the table. Default access to
	 * avoid synthetic accessors from inner classes.
	 */
	private def internalRemove(index: Int) {
		table(index) = INVALID_ELEMENT
		_size -= 1
		plugHole(index)
	}

	/**
	 * Ensures the set is large enough to contain the specified number of entries.
	 */
	private def ensureSizeFor(expectedSize: Int) {
		if (table.length * 3 >= expectedSize * 4)
			return

		var newCapacity = table.length << 1
		while (newCapacity * 3 < expectedSize * 4)
			newCapacity <<= 1

		var oldTable = table;
		table = Array.fill[Long](newCapacity)(INVALID_ELEMENT)
		for (o <- oldTable) {
			if (o != INVALID_ELEMENT) {
				var newIndex = getIndex(o)
				while (table(newIndex) != INVALID_ELEMENT) {
					newIndex += 1
					if (newIndex == table.length)
						newIndex = 0
				}
				table(newIndex) = o
			}
		}
	}

	/**
	 * Returns the index in the table at which a particular item resides, or -1 if
	 * the item is not in the table.
	 */
	private def find(o: Long): Int = {
		var index = getIndex(o)
		while (true) {
			val existing = table(index)
			if (existing == INVALID_ELEMENT)
				return -1

			if (o == existing)
				return index

			index += 1
			if (index == table.length)
				index = 0
		}
		-1
	}

	/**
	 * Returns the index in the table at which a particular item resides, or the
	 * index of an empty slot in the table where this item should be inserted if
	 * it is not already in the table.
	 */
	private def findOrEmpty(o: Long): Int = {
		var index = getIndex(o)
		while (true) {
			var existing = table(index)
			if (existing == INVALID_ELEMENT)
				return index
			if (o == existing)
				return index

			index += 1
			if (index == table.length)
				index = 0
		}
		index
	}

	private def getIndex(value: Long): Int = {
		var h = (value ^ (value >>> 32)).asInstanceOf[Int] // hashCode
		// Copied from Apache's AbstractHashedMap; prevents power-of-two collisions.
		h += ~(h << 9)
		h ^= (h >>> 14)
		h += (h << 4)
		h ^= (h >>> 10)
		// Power of two trick.
		h & (table.length - 1)
	}

	/**
	 * Tricky, we left a hole in the map, which we have to fill. The only way to
	 * do this is to search forwards through the map shuffling back values that
	 * match this index until we hit a null.
	 */
	private def plugHole(_hole: Int) {
		var hole = _hole
		var index = hole + 1
		if (index == table.length)
			index = 0

		while (table(index) != INVALID_ELEMENT) {
			var targetIndex = getIndex(table(index))
			if (hole < index) {
				/*
				 * "Normal" case, the index is past the hole and the "bad range" is from
				 * hole (exclusive) to index (inclusive).
				 */
				if (!(hole < targetIndex && targetIndex <= index)) {
					// Plug it!
					table(hole) = table(index)
					table(index) = INVALID_ELEMENT
					hole = index
				}
			} else {
				/*
				 * "Wrapped" case, the index is before the hole (we've wrapped) and the
				 * "good range" is from index (exclusive) to hole (inclusive).
				 */
				if (index < targetIndex && targetIndex <= hole) {
					// Plug it!
					table(hole) = table(index)
					table(index) = INVALID_ELEMENT
					hole = index
				}
			}
			index += 1
			if (index == table.length)
				index = 0
		}
	}

	override def toString = {
		val str = new StringBuilder

		val citer = iterator
		while (citer.hasNext) {
			str ++= citer.next.toString
			str += ' '
		}

		str.toString
	}

	def toList = {
		var r = List[Long]()

		val iter = iterator
		while (iter.hasNext) {
			r ::= iter.next
		}

		r
	}

	def toHashSet = {
		val r = new scala.collection.mutable.HashSet[Long]

		val iter = iterator

		while (iter.hasNext) {
			r add iter.next
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

		var averageValue = 0.0
		var maxValue = 0
		var oneAccessElements = 0

		val iter = iterator
		val tableSize = table.size
		while (iter.hasNext) {
			val v = iter.next
			val index = getIndex(v); // where the element should be
			var d = 1
			if (index == iter.last)
				oneAccessElements += 1
			else if (index < iter.last)
				d += iter.last - index
			else
				d += iter.last + (tableSize - index)

			maxValue = math.max(maxValue, d)
			averageValue += d
		}

		averageValue /= size.toDouble

		Depth(averageValue, maxValue, oneAccessElements.toDouble / size.toDouble)
	}

}
