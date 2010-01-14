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
import Helper._

class PegSolitaireSolver(val game: PegSolitaire) {

	/**
	 *  solution(0) is unused
	 */
	val solution = new Array[LongHashSet](game.length)

	def this(game: PegSolitaire, startFields: Iterable[Long]) {
		this(game)

		val deadends = Array.fill[BigDecimal](game.length)(BigDecimal(0))

		for(e <- startFields) {
			val bc = game.length - java.lang.Long.bitCount(e)
			if(bc <= 0)
				throw new Exception("Invalid number of bits set (" + e.toString + ")")

			if(solution(bc) == null)
				solution(bc) = new LongHashSet

			solution(bc) add e
		}

		// search first non free game field
		var start = 0
		while(solution(start) == null) {
			start += 1

			if(start == game.length)
				throw new Exception("no set field found")
		}

		for (sol <- (start+1) until game.length) {
			// Forward propagation
			printColoredText("search fields with " + sol + " removed pegs", Color.green)
			Console.flush
			if(solution(sol) == null)
				solution(sol) = new LongHashSet
			val current = solution(sol)

			var backpropagationRequired = false
			val oldprevioussize = solution(sol - 1).size

			val iter1 = solution(sol - 1).iterator
			while(iter1.hasNext) {
				if (!game.addFollower(iter1.next, current)) {
					backpropagationRequired = true
					iter1.remove
				}
			}

			printlnColoredText(", found " + current.size + " fields", Color.green)

			val newprevioussize = solution(sol - 1).size

			solution(sol-1).shrink

			// Backpropagation
			def backpropagation() {
				println("  clean field list with " + (sol - 1) + " removed pegs: left = " + newprevioussize + "  deleted = " + (oldprevioussize - newprevioussize))
				deadends(sol-1) += (oldprevioussize - newprevioussize) // update dead end counter
				for (i <- (sol - 1).to(start+1, -1)) {
					var changed = false
					val cur = solution(i)
					val oldpresize = solution(i - 1).size

					val iter2 = solution(i - 1).iterator
					while(iter2.hasNext) {
						if (!game.hasFollower(iter2.next, cur)) {
							changed = true
							iter2.remove
						}
					}

					val newpresize = solution(i - 1).size

					if (changed) {
						println("  clean field list with " + (i - 1) + " removed pegs: left = " + newpresize + "  deleted = " + (oldpresize - newpresize))
						deadends(i-1) += (oldpresize - newpresize) // update dead end counter
						solution(i-1).shrink
					} else {
						return
					}
				}
			}

			if (backpropagationRequired) {
				backpropagation
				println()
			}
		}

		println("\nFields without a 1 peg solution")
		var count = BigDecimal(0)
		for(i <- 0 until deadends.length) {
			if(deadends(i) > 0) {
				println("  There are " + deadends(i) + " with " + i + " removed pegs")
				count += deadends(i)
			}
		}
		printlnColoredText("There are " + count + " fields which doesn't result in a 1 peg solution", Color.blue)
	}

	def this(game: PegSolitaire, startField: Long) {
		this(game, List(startField))
	}

	def this(game: PegSolitaire, filename: String) {
		this(game)

		for(i <- 0 until game.length)
			solution(i) = null

		//<number of bits set:Int><number of values:Int><values...:Long>
		val in =    new java.io.DataInputStream(
						new java.util.zip.GZIPInputStream(
							new java.io.BufferedInputStream(
								new ProgressCallbackInputStream(
									new java.io.FileInputStream(filename),
									0.01 , percent => {print((percent*100).toInt + "% loaded from file \"" + filename + "\".\r"); Console.flush}
								)
							)
						)
					)

		try {
			var run = true
			while (run) {
				var i = 0
				try {
					i = in.readInt
				} catch {
					case _ => run = false
				}

				if (run) {
					if (i < 0)
						throw new Exception("i=" + i + " is lower than 0")
					if (i >= solution.length)
						throw new Exception("i=" + i + " is higher or equal " + solution.length)

					val size = in.readInt

					val newLongHashSet = new LongHashSet(size)

					solution(i) = newLongHashSet

					var pos = 0
					while (pos < size) {
						newLongHashSet += in.readLong
						pos += 1
					}
				}
			}
		} finally {
			in.close
		}
	}

	protected def getCompleteList(solutionNumber: Int): List[Long] = game.getCompleteList(solution(solutionNumber).toList)

	def getStart(): List[Long] = {
		var start = 0
		while(solution(start) == null || solution(start).size == 0) {
			start += 1

			if(start == game.length)
				throw new Exception("no set field found")
		}

		getCompleteList(start)
	}

	def getEnd(): List[Long] = {
		var end = game.length-1
		while(solution(end) == null || solution(end).size != 0) {
			end -= 1

			if(end == -1)
				throw new Exception("solution is empty")
		}

		getCompleteList(end)
	}

	def getFollower(field: Long): List[Long] = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos + 1 >= game.length)
			return List[Long]()
		val next = solution(fieldPos + 1)
		if(next == null)
			return List[Long]()
		var result = new LongHashSet
		var i = 0
		while (i < game.movemask.size) {
			var tmp = field & game.movemask(i)
			if (tmp == game.checkmask1(i) || tmp == game.checkmask2(i)) {
				val n = field ^ game.movemask(i)

				if (game.isInLongHashSet(n, next))
					result += n
			}

			i += 1
		}

		result.toList
	}

	/**
	 *  every line has the same format
	 * <number of bits set:Long><number of values:Long><values...:Long>
	 */
	def save(filename: String) {
		val out = new java.io.DataOutputStream(
					new java.util.zip.GZIPOutputStream(
						new java.io.BufferedOutputStream(
							new java.io.FileOutputStream(filename))))

		try {
			for(i <- 0 until game.length) {
				if(solution(i) != null) {
					out.writeInt(i)
					out.writeInt(solution(i).size)
					val iter = solution(i).iterator
					while(iter.hasNext)
						out.writeLong(iter.next)
				}
			}
		} finally {
			out.close
		}
	}

	/**
	 * ToDo: use getStart and getEnd to allow incomplete games
	 */
	def countPossibleGames(): BigDecimal = {
		println("\nCount how many ways are available to solve the board (this may take a while)")
		var previous: scala.collection.mutable.HashMap[Long, BigDecimal] = null
		var current = new scala.collection.mutable.HashMap[Long, BigDecimal]

		// init the current with BigDecimal = 1
		var iter = solution(game.length-1).iterator
		while(iter.hasNext)
			game.getEquivalentFields(iter.next) foreach {v => current(v) = 1 }

		for (i <- (game.length - 2).to(1, -1)) {
			print("  determine values for " + i + " removed pegs\r")
			Console.flush

			previous = current
			current = new scala.collection.mutable.HashMap[Long, BigDecimal]

			var iter = solution(i).iterator
			while(iter.hasNext)
				game.getEquivalentFields(iter.next) foreach {
					v =>
						for(f <- getFollower(v)) {
							try {
								current(v) += previous(f)
							} catch {
								case _ => current(v) = previous(f)
							}
						}
				}
		}

		var count = BigDecimal(0)
		for(s <- current)
			count += s._2

		count
	}

}

