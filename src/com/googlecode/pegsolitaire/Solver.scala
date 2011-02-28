/**
 *  Peg Solitaire
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
import Helper._
import scala.concurrent.ops._

object Boards extends Enumeration {
	val English = Value("english")
	val European = Value("euro")
	val Holes15 = Value("15holes")
	val User = Value("user")

	object EnglishBoard extends Board(
""". . o o o . .
. . o o o . .
o o o o o o o
o o o o o o o
o o o o o o o
. . o o o . .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	object EuropeanBoard extends Board(
""". . o o o . .
. o o o o o .
o o o o o o o
o o o o o o o
o o o o o o o
. o o o o o .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	object Holes15Board extends Board(
"""o . . . .
o o . . .
o o o . .
o o o o .
o o o o o""", Array(MoveDirections.Horizontal, MoveDirections.Vertical, MoveDirections.LeftDiagonal))
}

object Solver {

	/**
	 * load a solution from file
	 *
	 * Description of the used file
	 * <version: Int>
	 * <type: Int | only in version 1>
	 * <Possible Moves count: Int><Possible Moves: repeated Int> # only available in user mode
	 * <boardDescription length><boardDescription: repeated Char> # only available in user mode
	 * <number of bits set:Int><number of values: Int><values: repeated Long>: repeated until eof
	 *
	 * Version changes
	 *   1: Initial version
	 *   2: All fields are handled as User defined fields, since equal fields are now detected for all games.
	 *      Due to this change it is not possible to correctly load new results with an old program version.
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

			var boardType = Boards.User
			var deleteDuplicateFields = false
			version match {
				case 1 =>
					boardType = Boards(in.readInt)
					if(boardType != Boards.English) {
						println("Old file version. To save memory it is recommended to load and save the database.")
						deleteDuplicateFields = true
					}
				case 2 =>
				case _ =>
			    throw new Exception("unsupported version")
			}

			// read and create game
			boardType match {
				case Boards.English => output = new Solver(Boards.EnglishBoard)
				case Boards.European => output = new Solver(Boards.EuropeanBoard)
				case Boards.Holes15 => output = new Solver(Boards.Holes15Board)
				case Boards.User =>
					val possibleMoves = new Array[MoveDirections.Value](in.readInt)

					for(i <- 0 until possibleMoves.size)
						possibleMoves(i) = MoveDirections(in.readInt)

					// read board Description
					val boardDescription = new Array[Char](in.readInt)
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
						throw new Exception("file corruption detected, number of pegs is lower than 0")
					if (i >= output.solution.length)
						throw new Exception("file corruption detected, number of pegs is highter than the field size")

					val size = in.readInt

					val newLongHashSet = new LongHashSet(size)

					output.solution(i) = newLongHashSet

					val requiredNumberOfBitsSet = output.game.length-i

					for( pos <- 0 until size) {
						val n = in.readLong
						if(java.lang.Long.bitCount(n) == requiredNumberOfBitsSet) {
								if(deleteDuplicateFields) // no performance penalty by doing this inside the loop
									output.game.addToLongHashSet(n, newLongHashSet)
								else
									newLongHashSet += n
						} else {
							throw new Exception("file corruption detected, invalid field")
						}
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

	/**
	 *  solution(0) is unused
	 */
	val solution = new Array[LongHashSet](game.length)

	/**
	 * used to count in the ctor the dead ends
	 */
	private val deadends = Array.fill[Long](game.length)(0L)

	def this(game: Board, startFields: Iterable[Long], reduceMemory: Boolean = false, parallelProcessing: Boolean = false) {
		this(game)

		for(e <- startFields) {
			val bc = game.length - java.lang.Long.bitCount(e)
			if(bc <= 0)
				throw new Exception("Invalid number of bits set (" + e.toString + ")")

			if(solution(bc) == null)
				solution(bc) = new LongHashSet

			solution(bc) add e
		}

		if(parallelProcessing) {
			if(reduceMemory) {
				for (sol <- (getStartNum+1) until game.length)
					if(calculateForward(sol))
						cleanBackwardParallel(getEndNum-1)
			} else {
				for (sol <- (getStartNum+1) until game.length)
					calculateForward(sol)

				cleanBackwardParallel(getEndNum-1)
			}
		} else {
			if(reduceMemory) {
				for (sol <- (getStartNum+1) until game.length)
					if(calculateForward(sol))
						cleanBackward(getEndNum-1)
			} else {
				for (sol <- (getStartNum+1) until game.length)
					calculateForward(sol)

				cleanBackward(getEndNum-1)
			}
		}

		println("\nFields without a 1 peg solution")
		var count = 0L
		for(i <- 0 until deadends.length) {
			if(deadends(i) > 0L) {
				println("  There are " + deadends(i) + " with " + i + " removed pegs")
				count += deadends(i)
			}
		}
		printlnColoredText("There are " + count + " fields which doesn't result in a 1 peg solution", Color.blue)
	}

	protected def getCompleteList(solutionNumber: Int): Iterable[Long] = game.getCompleteList(solution(solutionNumber))

	/**
	 *  @return a list of all possible start-fields
	 */
	def getStart: Iterable[Long] = getCompleteList(getStartNum)

	/**
	 * @return first non empty solution set id
	 */
	def getStartNum: Int = {
		var start = 0
		while(solution(start) == null || solution(start).size == 0) {
			start += 1

			if(start == game.length)
				throw new Exception("no set field found")
		}

		start
	}

	/**
	 *  @return a list of all possible end-fields
	 */
	def getEnd: Iterable[Long] = getCompleteList(getEndNum)

	/**
	 * @return last non empty solution set id
	 */
	def getEndNum: Int = {
		var end = game.length-1
		while(solution(end) == null || solution(end).size == 0) {
			end -= 1

			if(end == -1)
				throw new Exception("solution is empty")
		}

		end
	}

	/**
	 * @return all follower for a provided field
	 */
	def getFollower(field: Long): Iterable[Long] = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos + 1 >= game.length)
			return new LongHashSet
		val next = solution(fieldPos + 1)
		if(next == null)
			return new LongHashSet

		game.getFollower(field, next)
	}

	/**
	 * @return all follower for a provided field
	 */
	def getPredecessorSet(field: Long): Iterable[Long] = {
		val fieldPos = game.length - java.lang.Long.bitCount(field)
		if (fieldPos - 1 <= 0)
			return new LongHashSet
		val previous = solution(fieldPos - 1)
		if(previous == null)
			return new LongHashSet

		game.getPredecessor(field, previous)
	}

	/**
	 * save solution to a file
     */
	def save(filename: String) {
		val out = new java.io.DataOutputStream(
					new java.util.zip.GZIPOutputStream(
						new java.io.BufferedOutputStream(
							new java.io.FileOutputStream(filename))))

		out.writeInt(2) // version

		// move directions
		out.writeInt(game.moveDirections.length)
		for(i <- game.moveDirections)
			out.writeInt(i.id)
		// board description
		out.writeInt(game.boardDescription.length)
		for(i <- game.boardDescription)
			out.writeChar(i)

		try {
			for(i <- 0 until game.length) {
				if(solution(i) != null && solution(i).size > 0) {
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
		game.getCompleteList(solution(game.length-1)) foreach {v => current(v) = 1 }

		for (i <- (game.length - 2).to(1, -1)) {
			print("  determine values for " + i + " removed pegs\r")
			Console.flush

			previous = current
			current = new scala.collection.mutable.HashMap[Long, BigDecimal]

			game.getCompleteList(solution(i)) foreach {
					v =>
						val viter = getFollower(v).iterator
						while(viter.hasNext) {
							val f = viter.next
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

	private def printDepthDebug(current: LongHashSet) = if(enableDebug) printlnDebug(" " + current.depth) else println()

	/**
	 * Forward propagation
	 */
	private def calculateForward(sol: Int): Boolean = {
		printColoredText("search fields with " + sol + " removed pegs", Color.green)

		if(solution(sol) == null)
			solution(sol) = new LongHashSet

		var deadEndFields = 0L
		val current = solution(sol)
		val iter = solution(sol - 1).iterator
		while(iter.hasNext) {
			if (!game.addFollower(iter.next, current)) {
				deadEndFields += 1L
				iter.remove
			}
		}

		printColoredText(", found " + current.size + " fields", Color.green)
		printDepthDebug(current)

		if (deadEndFields > 0L) {
			solution(sol-1).shrink
			deadends(sol-1) += deadEndFields // update dead end counter

			print("  clean field list with " + (sol - 1) + " removed pegs: dead ends = " + deadEndFields + "  currently valid = " + solution(sol - 1).size)
			printDepthDebug(solution(sol - 1))
		}

		deadEndFields > 0L
	}

	/**
	 * untested
	 */
	private def calculateBackward(sol: Int): Boolean = {
		printColoredText("search fields with " + sol + " removed pegs", Color.green)

		if(solution(sol) == null)
			solution(sol) = new LongHashSet

		var deadEndFields = 0L
		val current = solution(sol)
		val iter = solution(sol + 1).iterator
		while(iter.hasNext) {
			if (!game.addFollower(iter.next, current)) {
				deadEndFields += 1L
				iter.remove
			}
		}

		printColoredText(", found " + current.size + " fields", Color.green)
		printDepthDebug(current)

		if (deadEndFields > 0L) {
			solution(sol+1).shrink
			deadends(sol+1) += deadEndFields // update dead end counter

			print("  clean field list with " + (sol + 1) + " removed pegs: dead ends = " + deadEndFields + "  currently valid = " + solution(sol + 1).size)
			printDepthDebug(solution(sol + 1))
		}

		deadEndFields > 0L
	}

	/**
	 * untested
	 */
	private def cleanForward(pos: Int) {
		for (i <- pos until getEndNum) {

			var deadEndFields = 0L
			val current = solution(i-1)
			val iter = solution(i).iterator
			while(iter.hasNext) {
				if (!game.hasPredecessor(iter.next, current)) {
					deadEndFields += 1L
					iter.remove
				}
			}

			if (deadEndFields > 0L) {
				deadends(i) += deadEndFields // update dead end counter
				solution(i).shrink
				print("  clean field list with " + i + " removed pegs: dead ends = " + deadEndFields + "  left = " + solution(i).size)
				printDepthDebug(solution(i))

			} else {
				return
			}
		}
	}

	private def cleanBackward(pos: Int) {
		for (i <- (pos-1).until(getStartNum, -1)) {

			var deadEndFields = 0L
			val current = solution(i+1)
			val iter = solution(i).iterator
			while(iter.hasNext) {
				if (!game.hasFollower(iter.next, current)) {
					deadEndFields += 1L
					iter.remove
				}
			}

			if (deadEndFields > 0L) {
				deadends(i) += deadEndFields // update dead end counter
				solution(i).shrink
				print("  clean field list with " + i + " removed pegs: dead ends = " + deadEndFields + "  left = " + solution(i).size)
				printDepthDebug(solution(i))
			} else {
				return
			}
		}
	}

	private def cleanBackwardParallel(pos: Int) {
		val threadCount = Runtime.getRuntime.availableProcessors
		
		for (i <- (pos - 1).until(getStartNum, -1)) {

			val results = (0 until threadCount).map {
				threadID => future[(Long, Long, List[Long])] {
					var deadEndFields = 0L
					var resultSize = 0L
					val current = solution(i + 1)
					val iter = solution(i).iteratorRead(threadID, threadCount)
					var result: List[Long] = List[Long]()
					while (iter.hasNext) {
						val elem = iter.next
						if (game.hasFollower(elem, current)) {
							result ::= elem
							resultSize += 1L
						} else {
							deadEndFields += 1L
						}
					}
					(deadEndFields, resultSize, result)
				}
			}

			// merge
			val resultSize = results.foldLeft(0L) ( _ + _()._2) // wait for results and calculate new hashset size
			val newsol = new LongHashSet()
			var deadEndFields = 0L
			for (e <- results) {
				val r = e()
				deadEndFields += r._1 // update dead end counter
				newsol += r._3
			}

			solution(i) = newsol
			deadends(i) += deadEndFields

			if (deadEndFields > 0L) {
				print("  clean field list with " + i + " removed pegs: dead ends = " + deadEndFields + "  left = " + solution(i).size)
				printDepthDebug(solution(i))
			} else {
				return
			}
		}
	}

}

