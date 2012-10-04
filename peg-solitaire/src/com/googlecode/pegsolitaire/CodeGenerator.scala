/**
 * Peg Solitaire
 * Copyright (C) 2010-2012 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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

object CodeGenerator {

	def apply(in: Array[Array[Int]]): String = {
		val field: Array[Int] = (in.flatten.filter( _ != -1))

		// calculate operations
		val output = new scala.collection.mutable.HashMap[Int, Long]

		for (i <- (field.length-1).to(0,-1)) {
			val mask = 1L << i

			val e = field(field.length-1-i)
			val diff = e - i

			if(output contains diff) {
			  output(diff) |= mask
			} else {
			  output(diff) = mask
			}
		}

		// generate code
		val result = new StringBuilder

		result append "(\n   "
		var pos = 0
		for(i <- output) {
			result append "((f & "
			result append i._2
			result append "L)"

			if (i._1 > 0) {
				result append " << "
				result append math.abs(i._1)
			} else if (i._1 < 0) {
				result append " >> "
				result append math.abs(i._1)
			}
			result append ")"

			if(pos%4 == 3)
				result append "\n"

			if(pos != output.size-1)
				result append " | "

			pos += 1
		}

		result append ")"

		result.result()
	}
}
