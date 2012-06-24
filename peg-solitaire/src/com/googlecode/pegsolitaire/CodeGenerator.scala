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
		val result = new StringBuilder

		result append "(\n   "
		
		for (i <- (field.length-1).to(0,-1)) {
			result append "((f & ( 1L << "
			result append "%2d".format(i)
			result append "))"

			val e = field(field.length-1-i)
			if (e-i != 0) {
				if(i < e) {
					result append " << "
					result append "%2d".format(math.abs(e - i))
				} else {
					result append " >> "
					result append "%2d".format(math.abs(e - i))
				}
			} else {
				result append "      "
			}
			result append ")"
			
			if((field.length-1-i)%3 == 2)
				result append "\n"
			
			if(i != 0)
				result append " | "
		}
		
		result append ")"

		result.result()
	}
}