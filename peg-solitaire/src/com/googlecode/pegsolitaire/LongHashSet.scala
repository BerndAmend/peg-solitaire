/**
 * Peg Solitaire Solver
 * Copyright (C) 2010-2013 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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

object LongHashSet {
	/**
	 * In the interest of memory-savings, we start with the smallest feasible
	 * power-of-two table size that can hold three items without rehashing. If we
	 * started with a size of 2, we'd have to expand as soon as the second item
	 * was added.
	 */
	val INITIAL_TABLE_SIZE = 1 << 5

	val INVALID_ELEMENT = Long.MinValue

	var use_standard_hash_set = false

	def newInstance: LongHashSet =
		if(use_standard_hash_set)
			new StandardLongHashSet
		else
			new MemoryEfficientLongHashSet

	def newInstance(expectedSize: Int): LongHashSet =
		if(use_standard_hash_set)
			new StandardLongHashSet(expectedSize)
		else
			new MemoryEfficientLongHashSet(expectedSize)

	def newInstance(c: LongHashSet): LongHashSet =
		if(use_standard_hash_set)
			new StandardLongHashSet(c)
		else
			new MemoryEfficientLongHashSet(c)
}

trait HashSetIterator {
	def hasNext: Boolean

	def next: Long = {
		if (!hasNext)
			throw new java.util.NoSuchElementException()
		unsafe_next
	}

	/**
	 * call this function ONLY if you really know what you are doing
	 */
	def unsafe_next: Long
}

case class HashSetDepth(average: Double, max: Int, oneAccessPercent: Double) {
	override def toString = "(HashSet accessDepth average=" + average + " max=" + max + " 1 access required=" + oneAccessPercent + ")"
}

trait LongHashSet {

	/**
	 * number of elements inside the set
	 */
	def size: Int

	/**
	 * current fill state in percent
	 */
	def used: Double
	def isEmpty = size == 0

	def +=(c: LongHashSet)

	def +=(e: Long)
	def clear()
	def clear(new_expected_size: Int)

	def contains(o: Long): Boolean
	def iter: HashSetIterator
	def iter(groupID: Int, groupSize: Int): HashSetIterator

	/**
	 * Ensures the set is large enough to contain the specified number of entries.
	 */
	def ensureSizeFor(expectedSize: Int)

	def foreach(func: Long => Unit) {
		val i = iter

		while (i.hasNext)
			func(i.unsafe_next)
	}

	def toList = {
		var r = List[Long]()
		foreach(r ::= _)
		r
	}

	/**
	 * @return the search deep required to access elements (average, max, oneAccessPercent)
	 */
	def depth: HashSetDepth

	def bitDistribution: Array[Long] = {
		val it = iter
		val result = Array.fill[Long](64)(0L)

		while (it.hasNext) {
			val elem = it.unsafe_next

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
