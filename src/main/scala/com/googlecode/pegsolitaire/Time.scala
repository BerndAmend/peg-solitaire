/**
 * Peg Solitaire
 * Copyright (C) 2010-2011 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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
	def apply[T](name: String, output: String => Unit = println)(block: => T) = {
		val start = System.currentTimeMillis
		var result: T = null.asInstanceOf[T]
		try {
			result = block
		} finally {
			val time			= System.currentTimeMillis - start

			val milliseconds	= (time % 1000L, time / 1000L)
			val seconds			= (milliseconds._2 % 60L, milliseconds._2 / 60L)
			val minutes			= (seconds._2 % 60L, seconds._2 / 60L)
			val hours			= (minutes._2 % 24L, minutes._2 / 24L)
			val days			= (hours._2 % 24L, 0)

			val str = new StringBuilder
			str append name
			str append " (took"

			if(days._1 > 0) {
				str append " "
				str append days._1
				str append (if(days._1 == 1) " day" else " days")
			}

			if(hours._1 > 0) {
				str append " "
				str append hours._1
				str append (if(hours._1 == 1) " hour" else " hours")
			}

			if(minutes._1 > 0) {
				str append " "
				str append minutes._1
				str append (if(minutes._1 == 1) " minute" else " minutes")
			}

			if(seconds._1 > 0) {
				str append " "
				str append seconds._1
				str append (if(seconds._1 == 1) " second" else " seconds")
			}

			if(milliseconds._1 > 0) {
				str append " "
				str append milliseconds._1
				str append (if(milliseconds._1 == 1) " millisecond" else " milliseconds")
			}

			if(time == 0) {
				str append " 0 milliseconds"
			}

			str append ")"

			output(str.toString)
		}
		result
	}
}
