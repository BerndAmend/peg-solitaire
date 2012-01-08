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
import Helper._
import scala.concurrent.ops._

trait StatusObserver {
	def begin_forward_calculation()
	def end_forward_calculation(required_time: Long)
	
	def begin_backward_cleaning()
	def end_backward_cleaning(required_time: Long)
	
	def begin_forward_calculation_step(removed_pegs: Int)
	def end_forward_calculation_step(removed_pegs: Int, solutions: LongHashSet)

	def begin_backward_cleaning_step(removed_pegs: Int)
	def end_backward_cleaning_step(removed_pegs: Int, deadends: Long)
	
	def dead_ends(count: Long)
}

class DummyObserver extends StatusObserver {
	def begin_forward_calculation() {}
	def end_forward_calculation(required_time: Long) {}

	def begin_backward_cleaning() {}
	def end_backward_cleaning(required_time: Long) {}

	def begin_forward_calculation_step(removed_pegs: Int) {}
	def end_forward_calculation_step(removed_pegs: Int, solutions: LongHashSet) {}

	def begin_backward_cleaning_step(removed_pegs: Int) {}
	def end_backward_cleaning_step(removed_pegs: Int, deadends: Long) {}

	def dead_ends(count: Long) {}
}

object Boards extends Enumeration {
	val English = Value("english")
	val European = Value("euro")
	val Holes15 = Value("15holes")
	val User = Value("user")

	val EnglishBoard = new Board(
""". . o o o . .
. . o o o . .
o o o o o o o
o o o o o o o
o o o o o o o
. . o o o . .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	val EuropeanBoard = new Board(
""". . o o o . .
. o o o o o .
o o o o o o o
o o o o o o o
o o o o o o o
. o o o o o .
. . o o o . .""", Array(MoveDirections.Horizontal, MoveDirections.Vertical))

	val Holes15Board = new Board(
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
	def fromFile(filename: String, observer: StatusObserver): Solver = {
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
				case Boards.English => output = new Solver(Boards.EnglishBoard, observer)
				case Boards.European => output = new Solver(Boards.EuropeanBoard, observer)
				case Boards.Holes15 => output = new Solver(Boards.Holes15Board, observer)
				case Boards.User =>
					val possibleMoves = new Array[MoveDirections.Value](in.readInt)

					for(i <- 0 until possibleMoves.size)
						possibleMoves(i) = MoveDirections(in.readInt)

					// read board Description
					val boardDescription = new Array[Char](in.readInt)
					for(i <- 0 until boardDescription.size)
						boardDescription(i) = in.readChar

					output = new Solver(new Board(new String(boardDescription), possibleMoves), observer)

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
									newLongHashSet += output.game.boardHelper.getNormalform(n)
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

	/**
	 * @param threadCount threadCount 0=automatic
	 */
class Solver(val game: Board, val observer: StatusObserver, threadcount: Int) {
	require(threadcount >= 0)
	val thread_count = if(threadcount==0) Runtime.getRuntime.availableProcessors else threadcount

	/**
	 *  solution(0) is unused
	 */
	val solution = Array.fill(game.length)(new LongHashSet)

	/**
	 * used to count in the ctor the dead ends
	 */
	private val deadends = Array.fill[Long](game.length)(0L)

	def this(game: Board, observer: StatusObserver) = this(game, observer, 0)

	def this(game: Board, startFields: Iterable[Long], observer: StatusObserver, threadcount: Int = 0) {
		this(game, observer, threadcount)

		for(e <- startFields) {
			val bc = game.length - java.lang.Long.bitCount(e)
			if(bc <= 0)
				throw new Exception("Invalid number of bits set (" + e.toString + ")")

			solution(bc) add e
		}


		observer.begin_forward_calculation()
		Time(observer.end_forward_calculation _) {
			for (sol <- (getStartNum+1) until game.length) {
				observer.begin_forward_calculation_step(sol)
				calculateForward(sol)
				observer.end_forward_calculation_step(sol, solution(sol))
			}
		}
		
		println()

		// cleaning the gamefield after every step is useless
		observer.begin_backward_cleaning()
		Time(observer.end_backward_cleaning _) {
			cleanBackward(getEndNum)
		}

		observer.dead_ends(deadends.sum)
	}

	protected def getCompleteList(solutionNumber: Int): LongHashSet = game.getCompleteList(solution(solutionNumber))
	def getCompleteList(h: LongHashSet) = game.getCompleteList(h)

	/**
	 *  @return a list of all possible start-fields
	 */
	def getStart: LongHashSet = getCompleteList(getStartNum)

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
	def getEnd: LongHashSet = getCompleteList(getEndNum)

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
		game.getPredecessor(field, solution(fieldPos - 1))
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
					solution(i) foreach { out.writeLong(_) }
				}
			}
		} finally {
			out.close
		}
	}

	/**
	 * Count how many ways are available to solve the board (this may take a while)
	 * ToDo: use getStart and getEnd to allow incomplete games
	 */
	def countPossibleGames(): BigDecimal = {
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
						getFollower(v) foreach {
							f =>
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

	private def calculateForward (sol: Int): Unit = calculateNextStep(sol, -1, true) //game.addFollower)
	private def calculateBackward(sol: Int): Unit = calculateNextStep(sol,  1, false) //game.addPredecessor)

	@specialized(AnyRef)
  private def calculateNextStep(sol: Int, next: Int, follower: Boolean) {
	  val results = (0 until thread_count).map {
      threadID => future[Boolean] {
        val iter = solution(sol + next).iteratorRead(threadID, thread_count)
        var result = new LongHashSet()
	      if(follower) {
	        while (iter.hasNext) {
		        game.addFollower(iter.unsafe_next, result)
          }
	      } else {
		      while (iter.hasNext) {
			      game.addPredecessor(iter.unsafe_next, result)
          }
	      }
	      val current = solution(sol)
	      current.synchronized {
		      current += result
	      }
        true
      }
    }

	  results foreach (_())
  }

	private def cleanForward(pos: Int) = cleanNextStep(pos, 1, pos, getEndNum, false) //game.hasPredecessor)
	private def cleanBackward(pos: Int) = cleanNextStep(pos, -1, pos, getStartNum, true) //game.hasFollower)

	private def cleanNextStep(pos: Int, next: Int, from: Int, to: Int, follower: Boolean) { // func: (Long, LongHashSet) => Boolean) {
		for (i <- from.until(to, next)) {

			observer.begin_backward_cleaning_step(i+next)

			val results = (0 until thread_count).map {
				threadID => future[(Long, LongHashSet)] {
					var deadEndFields = 0L
					val current = solution(i)
					val iter = solution(i + next).iteratorRead(threadID, thread_count)
					var result = new LongHashSet
					if(follower) {
						while (iter.hasNext) {
							val elem = iter.unsafe_next
							if (game.hasFollower(elem, current)) {
								result += elem
							} else {
								deadEndFields += 1L
							}
						}
					} else {
						while (iter.hasNext) {
							val elem = iter.unsafe_next
							if (game.hasPredecessor(elem, current)) {
								result += elem
							} else {
								deadEndFields += 1L
							}
						}
					}
					(deadEndFields, result)
				}
			}

			// merge
			val resultSize = results.foldLeft(0) ( _ + _()._2.size) // wait for results and calculate new hashset size
			val newsol = new LongHashSet(resultSize)
			var deadEndFields = 0L
			for (e <- results) {
				val r = e()
				deadEndFields += r._1 // update dead end counter
				newsol += r._2
			}

			solution(i + next) = newsol
			deadends(i + next) += deadEndFields

			observer.end_backward_cleaning_step(i+next, deadEndFields)
			if (deadEndFields == 0L) {
				return
			}
		}
	}

}
