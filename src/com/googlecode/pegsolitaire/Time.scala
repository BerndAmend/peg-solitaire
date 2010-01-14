/**
 * Peg Solitaire Solver  Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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

object Time {
	def apply[T](name: String)(block: => T) {
		val start = System.currentTimeMillis
		try {
			block
		} finally {
			val diff = System.currentTimeMillis - start
			val seconds = diff / 1000.0
			val minutes = seconds / 60.0
			println(name + " completed, time taken: " + diff + " ms = " + seconds + " = " + minutes + " min")
		}
	}
}
