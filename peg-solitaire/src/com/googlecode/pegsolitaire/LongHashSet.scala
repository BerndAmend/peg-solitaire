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

	def newInstance: LongHashSet = new StandardLongHashSet
	def newInstance(expectedSize: Int): LongHashSet = new StandardLongHashSet(expectedSize)
	def newInstance(c: LongHashSet): LongHashSet = new StandardLongHashSet(c)
}

trait HashSetIterator {
	def hasNext: Boolean
	def next: Long

	/**
	 * call this function ONLY if you really know what you are doing
	 */
	def unsafe_next: Long
}

trait LongHashSet {
	def size: Int

	/**
	 * current fill state in percent
	 */
	def used: Double
	def isEmpty: Boolean

	def +=(c: LongHashSet)

	/**
	 * Works just like    { @link # addAll ( Collection ) }, but for arrays. Used to avoid
	 * having to synthesize a collection in    { @link Sets }.
	 * ignores INVALID_ELEMENT entries in the Array
	 */
	def +=(elements: Array[Long])
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
	def foreach(func: Long => Unit)
	def toList: List[Long]

	case class Depth(average: Double, max: Int, oneAccessPercent: Double) {
		override def toString = "(HashSet accessDepth average=" + average + " max=" + max + " 1 access required=" + oneAccessPercent + ")"
	}

	/**
	 * @return the search deep required to access elements (average, max, oneAccessPercent)
	 */
	def depth: Depth;
	def bitDistribution: Array[Long];
	def bitDistributionString: String;
}
