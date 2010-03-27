/**
 * Peg Solitaire
 * Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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
import scala.actors.Actor
import scala.actors.Actor._

object Solver {

	/**
	 * load a solution from file
	 *
	 * Description of the used file
	 * <version: Int>
	 * <type: Int>
	 * <Possible Moves count: Int><Possible Moves: repeated Int> # only available in user mode
	 * <boardDescription length><boardDescription: repeated Char> # only available in user mode
	 * <number of bits set:Int><number of values: Int><values: repated Long>: repeated until eof
	 */
	def fromFile(filename: String): Solver = {
		var output: Solver = null

		val in = new java.io.DataInputStream(
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
			// read version
			val version = in.readInt

			if(version != 1)
				throw new Exception("unsupported version")

			val boardType = GameType(in.readInt)

			// read and create game
			boardType match {
				case GameType.English => output = new Solver(EnglishBoard)
				case GameType.European => output = new Solver(EuropeanBoard)
				case GameType.Holes15 => output = new Solver(Board15Holes)
				case GameType.User =>
					val possibleMoves = new Array[MoveDirections.Value](in.readInt)

					for(i <- 0 until possibleMoves.size)
						possibleMoves(i) = MoveDirections(in.readInt)

					// read board Description
					var boardDescription = new Array[Char](in.readInt)
					for(i <- 0 until boardDescription.size)
						boardDescription(i) = in.readChar

					output = new Solver(new Board(new String(boardDescription), possibleMoves))

				case _ => throw new Exception("unsupported boardType")
			}

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
					if (i >= output.solution.length)
						throw new Exception("i=" + i + " is higher or equal " + output.solution.length)

					val size = in.readInt

					val newLongHashSet = new LongHashSet(size)

					output.solution(i) = newLongHashSet

					var pos = 0
					while (pos < size) {
						val n = in.readLong
						if((output.game.length-java.lang.Long.bitCount(n)) == i) {
							newLongHashSet += n
						} else {
							printlnError("error: ignore invalid entry (" + n + ")")
						}
							pos += 1
					}
				}
			}
		} finally {
			in.close
		}

		output
	}

}

class Solver(val game: Board) {

	val calculateForward = new Processing
	val calculateBackward = new Processing
	val cleanForward = new Processing
	val cleanBackward = new Processing

	/**
	 *  solution(0) is unused
	 */
	val solution = new Array[LongHashSet](game.length)

	def this(game: Board, startFields: Iterable[Long]) {
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

			if(Helper.enableDebug) {
				printColoredText(", found " + current.size + " fields", Color.green)
				printlnDebug(" (HashSet collisions=" + current.collisions + ")")
			} else {
				printlnColoredText(", found " + current.size + " fields", Color.green)
			}

			solution(sol-1).shrink

			val newprevioussize = solution(sol - 1).size

			// Backpropagation
			def backpropagation() {
				if(Helper.enableDebug) {
					print("  clean field list with " + (sol - 1) + " removed pegs: deleted = " + (oldprevioussize - newprevioussize) + "  left = " + newprevioussize)
					printlnDebug(" (HashSet collisions=" + solution(sol - 1).collisions + ")")
				} else {
					println("  clean field list with " + (sol - 1) + " removed pegs: deleted = " + (oldprevioussize - newprevioussize) + "  left = " + newprevioussize)
				}

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
						if(Helper.enableDebug) {
							print("  clean field list with " + (i - 1) + " removed pegs: deleted = " + (oldpresize - newpresize) + "  left = " + newpresize)
							printlnDebug(" (HashSet collisions=" + solution(i - 1).collisions + ")")
						} else {
							println("  clean field list with " + (i - 1) + " removed pegs: deleted = " + (oldpresize - newpresize) + "  left = " + newpresize)
						}

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

	def this(game: Board, startField: Long) {
		this(game, List(startField))
	}

	protected def getCompleteList(solutionNumber: Int): List[Long] = game.getCompleteList(solution(solutionNumber).toList)

	/**
	 * @return a list of all possible start-fields
	 */
	def getStart(): List[Long] = {
		var start = 0
		while(solution(start) == null || solution(start).size == 0) {
			start += 1

			if(start == game.length)
				throw new Exception("no set field found")
		}

		getCompleteList(start)
	}

	/**
	 * @return a list of all possible end-fields
	 */
	def getEnd(): List[Long] = {
		var end = game.length-1
		while(solution(end) == null || solution(end).size != 0) {
			end -= 1

			if(end == -1)
				throw new Exception("solution is empty")
		}

		getCompleteList(end)
	}

	/**
	 * @return all follower for a provided field
	 */
	def getFollower(field: Long): List[Long] = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos + 1 >= game.length)
			return List[Long]()
		val next = solution(fieldPos + 1)
		if(next == null)
			return List[Long]()

		game.getFollower(field, next).toList
	}

	/**
	 * @return all follower for a provided field
	 */
	def getPredecessor(field: Long): List[Long] = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos - 1 <= 0)
			return List[Long]()
		val previous = solution(fieldPos - 1)
		if(previous == null)
			return List[Long]()

		game.getPredecessor(field, previous).toList
	}

	/**
	 * save solution to a file
     */
	def save(filename: String) {
		val out = new java.io.DataOutputStream(
					new java.util.zip.GZIPOutputStream(
						new java.io.BufferedOutputStream(
							new java.io.FileOutputStream(filename))))

		out.writeInt(1) // version
		out.writeInt(game.gameType.id) // type

		if(game.gameType == GameType.User) {
			// move directions
			out.writeInt(game.moveDirections.length)
			for(i <- game.moveDirections)
				out.writeInt(i.id)
			// board description
			out.writeInt(game.boardDescription.length)
			for(i <- game.boardDescription)
				out.writeChar(i)
		}

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
	 * count how many different games are playable
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

	case class CalculateForward(num: Int)
	case class CalculateBackward(num: Int)
	case class CleanForward(num: Int)
	case class CleanBackward(num: Int)

	class Processing extends Actor {

		def act() = {
			loop {
				react {
					case v: CalculateForward => {

					}
					case v: CalculateBackward => {
						// ToDo
					}
					case v: CleanForward => { // message is send by CalculateBackward
						// ToDo
					}
					case v: CleanBackward => { // message is send by CalculateForward
						//
					}
				}
			}
		}

		start
	}

}

