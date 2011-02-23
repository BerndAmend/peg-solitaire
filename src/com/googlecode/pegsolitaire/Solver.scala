/**
 *  Peg Solitaire
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
import scala.concurrent.ops._

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

	/**
	 *  solution(0) is unused
	 */
	val solution = new Array[LongHashSet](game.length)

	/**
	 * used to count in the ctor the dead ends
	 */
	private val deadends = Array.fill[BigDecimal](game.length)(BigDecimal(0))

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
		var count = BigDecimal(0)
		for(i <- 0 until deadends.length) {
			if(deadends(i) > 0) {
				println("  There are " + deadends(i) + " with " + i + " removed pegs")
				count += deadends(i)
			}
		}
		printlnColoredText("There are " + count + " fields which doesn't result in a 1 peg solution", Color.blue)
	}

	protected def getCompleteList(solutionNumber: Int): List[Long] = game.getCompleteList(solution(solutionNumber))

	/**
	 *  @return a list of all possible start-fields
	 */
	def getStart: List[Long] = getCompleteList(getStartNum)

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
	def getEnd: List[Long] = getCompleteList(getEndNum)

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
	def getFollower(field: Long): LongHashSet = {
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
	def getPredecessorSet(field: Long): LongHashSet = {
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
		val iter = solution(game.length-1).iterator
		while(iter.hasNext)
			game.getEquivalentFields(iter.next) foreach {v => current(v) = 1 }

		for (i <- (game.length - 2).to(1, -1)) {
			print("  determine values for " + i + " removed pegs\r")
			Console.flush

			previous = current
			current = new scala.collection.mutable.HashMap[Long, BigDecimal]

			val iter = solution(i).iterator
			while(iter.hasNext)
				game.getEquivalentFields(iter.next) foreach {
					v =>
						var viter = getFollower(v).iterator
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

		var deadEndFields = 0
		val current = solution(sol)
		val iter = solution(sol - 1).iterator
		while(iter.hasNext) {
			if (!game.addFollower(iter.next, current)) {
				deadEndFields += 1
				iter.remove
			}
		}

		printColoredText(", found " + current.size + " fields", Color.green)
		printDepthDebug(current)

		if (deadEndFields > 0) {
			solution(sol-1).shrink
			deadends(sol-1) += deadEndFields // update dead end counter

			print("  clean field list with " + (sol - 1) + " removed pegs: dead ends = " + deadEndFields + "  currently valid = " + solution(sol - 1).size)
			printDepthDebug(solution(sol - 1))
		}

		deadEndFields > 0
	}

	/**
	 * untested
	 */
	private def calculateBackward(sol: Int): Boolean = {
		printColoredText("search fields with " + sol + " removed pegs", Color.green)

		if(solution(sol) == null)
			solution(sol) = new LongHashSet

		var deadEndFields = 0
		val current = solution(sol)
		val iter = solution(sol + 1).iterator
		while(iter.hasNext) {
			if (!game.addFollower(iter.next, current)) {
				deadEndFields += 1
				iter.remove
			}
		}

		printColoredText(", found " + current.size + " fields", Color.green)
		printDepthDebug(current)

		if (deadEndFields > 0) {
			solution(sol+1).shrink
			deadends(sol+1) += deadEndFields // update dead end counter

			print("  clean field list with " + (sol + 1) + " removed pegs: dead ends = " + deadEndFields + "  currently valid = " + solution(sol + 1).size)
			printDepthDebug(solution(sol + 1))
		}

		deadEndFields > 0
	}

	/**
	 * untested
	 */
	private def cleanForward(pos: Int) {
		for (i <- pos until getEndNum) {

			var deadEndFields = 0
			val current = solution(i-1)
			val iter = solution(i).iterator
			while(iter.hasNext) {
				if (!game.hasPredecessor(iter.next, current)) {
					deadEndFields += 1
					iter.remove
				}
			}

			if (deadEndFields > 0) {
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

			var deadEndFields = 0
			val current = solution(i+1)
			val iter = solution(i).iterator
			while(iter.hasNext) {
				if (!game.hasFollower(iter.next, current)) {
					deadEndFields += 1
					iter.remove
				}
			}

			if (deadEndFields > 0) {
				deadends(i) += deadEndFields // update dead end counter
				solution(i).shrink
				print("  clean field list with " + i + " removed pegs: dead ends = " + deadEndFields + "  left = " + solution(i).size)
				printDepthDebug(solution(i))

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

