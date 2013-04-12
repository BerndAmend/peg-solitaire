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

object Time {
	/**
	 * output: (name, milliseconds)
	 */
	def apply[T](output: Long => Unit)(block: => T) = {
		val start = System.currentTimeMillis
		var result: T = null.asInstanceOf[T]
		try {
			result = block
		} finally {
			val time			= System.currentTimeMillis - start
			output(time)
		}
		result
	}

	def apply[T](name: String)(block: => T): T = apply[T]((time: Long) => println(name + " (took " + Helper.millisecToString(time) + ")"))(block)
}
