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

object Helper {
	var enableColor = false
	var enableDebug = false

	object Color extends Enumeration {
		val red = Value(Console.RED)
		val green = Value(Console.GREEN)
		val blue = Value(Console.BLUE)
	}

	def printColoredText(msg: String, color: Color.Value) = {
		if(enableColor)
			print(color + msg + Console.RESET)
		else
			print(msg)
		Console.flush
	}
	
	def printlnColoredText(msg: String, color: Color.Value) = {
		if(enableColor)
			println(color + msg + Console.RESET)
		else
			println(msg)
	}

	def printError(msg: String) = printColoredText(msg, Color.red)
	def printlnError(msg: String) = printlnColoredText(msg, Color.red)

	def printDebug(msg: String) = if(enableDebug) printColoredText(msg, Color.blue)
	def printlnDebug(msg: String) = if(enableDebug) printlnColoredText(msg, Color.blue)

	/**
	 * debug that always prints a line feed
	 */
	def printlnlnDebug(msg: String) = if(enableDebug) printlnColoredText(msg, Color.blue) else println()

	/**
	 * very bad implementation
	 */
	def mixStrings(str1: String, str2: String, seperator: String, str2label: String) = {
		val sb = new StringBuilder
		val ss1 = str1.split("\n")
		val ss2 = str2.split("\n")

		val ss1length = ss1.length
		val ss2length = ss2.length + 1
		val maxlength = scala.math.max(ss1length, ss2length)

		var i = 0
		while(i<maxlength) {
			if(i<ss1length)
				sb append ss1(i)

			sb append seperator

			if(i==0) {
				sb append str2label
				for(c <- 0 until (ss2(0).length - str2label.length))
					sb += ' '
			} else if((i-1)<ss2length)
				sb append ss2(i-1)

			sb append "\n"
			i+=1
		}
		sb.toString
	}

	def readYesOrNo(msg: String): Boolean = {
		while (true) {
			print(msg + " ")
			Console.flush
			val input = readLine
			if(input == null) {
				println("Bye, bye")
				sys.exit(0)
			}
			input.toLowerCase match {
				case "y" => return true
				case "yes" => return true
				case "n" => return false
				case "no" => return false
				case _ => printlnError("error: invalid input, please try again")
			}
		}
		false
	}

	def millisecToString(time: Long): String = {
		val milliseconds	= (time % 1000L, time / 1000L)
		val seconds			= (milliseconds._2 % 60L, milliseconds._2 / 60L)
		val minutes			= (seconds._2 % 60L, seconds._2 / 60L)
		val hours			= (minutes._2 % 24L, minutes._2 / 24L)
		val days			= (hours._2 % 24L, 0)

		val str = new StringBuilder

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

		str.toString
	}
}
