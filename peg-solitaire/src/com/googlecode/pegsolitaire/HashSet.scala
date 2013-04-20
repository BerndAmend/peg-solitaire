/**
 * Peg Solitaire
 * Copyright (C) 2010-2013 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.googlecode.pegsolitaire

object HashSet {
	def sizeFitsIntoCapacity(expectedSize: Int, currentSize: Int) = 4 * expectedSize < 3 * currentSize
	
	def computeCapacityForSize(expectedSize: Int, currentSize: Int) = {
		var newCapacity = currentSize
		while (!sizeFitsIntoCapacity(expectedSize, newCapacity))
			newCapacity <<= 1
		newCapacity
    }
}

trait HashSet {
	protected var _size = 0

	def size = _size
	def table_size: Int
	
	/**
	 * current fill state in percent
	 */
	def used = if (table_size == 0) 1.0 else (size.toDouble / table_size.toDouble)
	def isEmpty = size == 0
	
	/**
	 * @return the search deep required to access elements (average, max, oneAccessPercent)
	 */
	def depth: HashSetDepthTrait
	
	/**
	 * Removes all elements from the HashSet and frees the required memory.
	 */
	def clear()

	/**
	 * Removes all elements from the HashSet and allocates the internal
	 * memory to fit new_expected_size elements.
	 */
	def clear(new_expected_size: Int)
}